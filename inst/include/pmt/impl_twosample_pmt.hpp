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
#ifdef SETJMP
        SETJMP(statistic_func)
#endif
        statistic_container.init(twosample_update, 1);
    } else {
        NumericVector x_ = x.size() < y.size() ? x : y;
        NumericVector y_ = x.size() < y.size() ? y : x;

        R_xlen_t m = x_.size();
        R_xlen_t n = y_.size() + m;

        if (n_permu == 0) {
            std::vector<R_xlen_t> p;
            p.reserve(n);
            for (R_xlen_t i = 0; i < n; i++) {
                p.emplace_back(i);
            }
            auto swap_update = [x_, y_, m, &p, &twosample_update](const R_xlen_t out, const R_xlen_t in) mutable {
                std::swap(x_[p[out]], y_[p[in] - m]);
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

            R_xlen_t j;
            auto R4 = [&c, &j, &swap_update]() mutable {
                if (c[j] > j) {
                    swap_update(c[j], j - 1);
                    c[j] = std::exchange(c[j - 1], j - 1);
                    return true;
                } else {
                    j++;
                    return false;
                }
            };
            auto R5 = [&c, &j, &swap_update]() mutable {
                if (c[j] + 1 < c[j + 1]) {
                    swap_update(c[j - 1], c[j] + 1);
                    c[j - 1] = c[j]++;
                    return true;
                } else {
                    j++;
                    return false;
                }
            };

#ifdef SETJMP
            SETJMP(statistic_func)
#endif
            statistic_container.init(twosample_update, 1, C(n, m));

            twosample_update();

            j = 0;
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
#ifdef SETJMP
            SETJMP(statistic_func)
#endif
            statistic_container.init(twosample_update, 1, n_permu);

            do {
                for (R_xlen_t i = 0; i < m; i++) {
                    R_xlen_t j = static_cast<R_xlen_t>(unif_rand() * (n - i)) + i - m;
                    bool c = j >= 0;
                    swap_if(c, x_[i], y_[j * c]);
                }
            } while (twosample_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}