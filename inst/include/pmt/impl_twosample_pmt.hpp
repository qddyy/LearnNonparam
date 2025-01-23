template <bool progress, typename T>
RObject impl_twosample_pmt(
    NumericVector x,
    NumericVector y,
    const T& statistic_func,
    const double n_permu)
{
    Stat<progress> statistic_container;

    auto twosample_update = [&statistic_container, statistic_closure = statistic_func(x, y), x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    if (std::isnan(n_permu)) {
        statistic_container.init(twosample_update, 1);
    } else {
        NumericVector x_ = x.size() < y.size() ? x : y;
        NumericVector y_ = x.size() < y.size() ? y : x;

        R_xlen_t m = x_.size();
        R_xlen_t n = y_.size();

        R_xlen_t i, j;

        std::unordered_multimap<double, R_xlen_t> x_map;
        std::vector<decltype(x_map.begin())> inv_map;
        x_map.reserve(m);
        inv_map.reserve(m);
        for (i = 0; i < m; i++) {
            inv_map.emplace_back(x_map.emplace(x_[i], i));
        }

        j = 0;
        while (j < n) {
            auto it = x_map.find(y_[j]);
            if (it != x_map.end()) {
                std::swap(y_[j], y_[--n]);
                x_map.erase(inv_map[--m]);
                if (it->first != x_[m]) {
                    std::swap(x_[it->second], x_[m]);
                    inv_map[it->second] = x_map.emplace(x_[it->second], it->second);
                    x_map.erase(it);
                }
            } else {
                j++;
            }
        }

        n += m;
        if (n_permu == 0) {
            statistic_container.init(twosample_update, 1, C(n, m));

            std::vector<R_xlen_t> p;
            p.reserve(n);
            for (i = 0; i < n; i++) {
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
            for (i = 0; i < m; i++) {
                c.emplace_back(i);
            }
            c.emplace_back(n);

            twosample_update();

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
            statistic_container.init(twosample_update, 1, n_permu);

            do {
                for (i = 0; i < m; i++) {
                    j = i + rand_int(n - i);
                    if (j >= m) {
                        std::swap(x_[i], y_[j - m]);
                    }
                }
            } while (twosample_update());
        }
    }

    return static_cast<RObject>(statistic_container);
}