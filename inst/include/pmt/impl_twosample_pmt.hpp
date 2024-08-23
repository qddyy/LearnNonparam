template <typename T, typename U>
NumericVector impl_twosample_pmt(
    NumericVector x,
    NumericVector y,
    const U& statistic_func,
    const std::string type,
    const R_xlen_t n_permu)
{
    T bar;

    R_len_t m = x.size();
    R_len_t n = m + y.size();

    auto statistic_closure = statistic_func(x, y);
    auto twosample_update = [x, y, &statistic_closure, &bar]() {
        return bar << statistic_closure(x, y);
    };

    bar.init_statistic(twosample_update);

    if (type != "permu") {
        return bar.close();
    }

    R_len_t i, j;
    if (n_permu == 0) {
        IntegerVector p(n, 0);

        for (i = m; i < n; i++) {
            p[i] = 1;
        }
        bar.init_statistic_permu(n_permutation(p));

        for (i = 0; i < n; i++) {
            p[i] = i;
        }
        auto swap_update = [x, y, p, m, &twosample_update](const R_len_t x_out, const R_len_t x_in) mutable {
            std::swap(x[p[x_out]], y[p[x_in] - m]);
            std::swap(p[x_out], p[x_in]);
            twosample_update();
        };

        // Algorithm R, "revolving-door combinations", TAOCP 4A/1
        IntegerVector c(no_init(m + 1));
        for (i = 0; i < m; i++) {
            c[i] = i;
        }
        c[m] = n;

        twosample_update();

        auto R4 = [c, &j, &swap_update]() mutable {
            if (c[j] > j) {
                swap_update(c[j], j - 1);
                c[j] = std::exchange(c[j - 1], j - 1);
                return true;
            } else {
                j++;
                return false;
            }
        };
        auto R5 = [c, &j, &swap_update]() mutable {
            if (c[j] + 1 < c[j + 1]) {
                swap_update(c[j - 1], c[j] + 1);
                c[j - 1] = c[j]++;
                return true;
            } else {
                j++;
                return false;
            }
        };

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
        bar.init_statistic_permu(n_permu);

        do {
            for (i = 0; i < m; i++) {
                j = i + rand_int(n - i);
                if (j >= m) {
                    std::swap(x[i], y[j - m]);
                }
            }
        } while (twosample_update());
    }

    return bar.close();
}