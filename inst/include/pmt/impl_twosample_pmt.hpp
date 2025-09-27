#pragma once

template <bool progress, typename T>
RObject impl_twosample_pmt(
    NumericVector x,
    NumericVector y,
    T&& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(x, y);
    auto twosample_update = [&statistic_container, &statistic_closure, x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    if (std::isnan(n_permu)) {
        statistic_container.allocate(1, n_permu);

#ifdef SETJMP
        SETJMP(statistic_func)
#endif

        twosample_update();
    } else {
        if (x.size() > y.size()) {
            std::swap(x, y);
        }

        R_xlen_t m = x.size();
        R_xlen_t n = y.size() + m;

        if (n_permu == 0) {
            auto swap_update = [&twosample_update, x, y, m, p = [n]() {
                std::vector<R_xlen_t> p;
                p.reserve(n);
                for (R_xlen_t i = 0; i < n; i++) {
                    p.emplace_back(i);
                }
                return p;
            }()](const R_xlen_t out, const R_xlen_t in) mutable {
                std::swap(x[p[out]], y[p[in] - m]);
                std::swap(p[out], p[in]);
                twosample_update();
            };

            // Algorithm R, "revolving-door combinations", TAOCP 4A/1
            std::vector<R_xlen_t> c;
            c.reserve(m + 1);
            for (R_xlen_t i = 0; i < m; i++) {
                c.emplace_back(i);
            }
            c.emplace_back(n);

            R_xlen_t j = 0;
            auto R4 = [&c, &j, &swap_update]() {
                if (c[j] > j) {
                    swap_update(c[j], j - 1);
                    c[j] = std::exchange(c[j - 1], j - 1);
                    return true;
                } else {
                    j++;
                    return false;
                }
            };
            auto R5 = [&c, &j, &swap_update]() {
                if (c[j] + 1 < c[j + 1]) {
                    swap_update(c[j - 1], c[j] + 1);
                    c[j - 1] = c[j]++;
                    return true;
                } else {
                    j++;
                    return false;
                }
            };

            statistic_container.allocate(1, C(n, m));

#ifdef SETJMP
            SETJMP(statistic_func)
#endif

            twosample_update();

            statistic_container.switch_ptr();
            twosample_update();
            if (m & 1) {
                while (j < m) {
                    if (c[0] + 1 < c[1]) {
                        swap_update(c[0], c[0] + 1);
                        c[0]++;
                        continue;
                    }

                    j = 1;
                    while (j < m && !R4() && !R5()) { }
                }
            } else {
                while (j < m) {
                    if (c[0] > 0) {
                        swap_update(c[0], c[0] - 1);
                        c[0]--;
                        continue;
                    }

                    j = 1;
                    if (R5()) {
                        continue;
                    }
                    while (j < m && !R4() && !R5()) { }
                }
            }
        } else {
            statistic_container.allocate(1, n_permu);

#ifdef SETJMP
            SETJMP(statistic_func)
#endif

            twosample_update();

            statistic_container.switch_ptr();
            do {
                for (R_xlen_t i = 0; i < m; i++) {
                    R_xlen_t j = static_cast<R_xlen_t>(unif_rand() * (n - i)) + i - m;
                    bool c = j >= 0;
                    swap_if(c, x[i], y[j * c]);
                }
            } while (twosample_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}