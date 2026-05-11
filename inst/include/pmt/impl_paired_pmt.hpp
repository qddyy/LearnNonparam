#pragma once

#include <cstdint>

template <typename T, typename = std::enable_if_t<std::is_unsigned_v<T> && sizeof(T) == 4>>
int countr_zero_u32(T x)
{
    static constexpr std::array<int, 32> multiply_debruijn_bit_position = {
        0, 1, 28, 2, 29, 14, 24, 3,
        30, 22, 20, 15, 25, 17, 4, 8,
        31, 27, 13, 23, 21, 19, 16, 7,
        26, 12, 18, 6, 11, 5, 10, 9
    };

    return multiply_debruijn_bit_position[static_cast<std::uint32_t>((x & -x) * 0x077CB531U) >> 27];
}

template <bool progress, typename T>
RObject impl_paired_pmt(
    NumericVector x,
    NumericVector y,
    T&& statistic_func,
    const double n_permu
)
{
    Stat<progress> statistic_container;

    auto statistic_closure = statistic_func(x, y);
    auto paired_update = [&statistic_container, &statistic_closure, x, y]() {
        return statistic_container << statistic_closure(x, y);
    };

    R_xlen_t n = x.size();

    statistic_container.allocate(1, n_permu != 0 ? n_permu : R_xlen_t{ 1 } << n);

#ifdef SETJMP
    SETJMP(statistic_func)
#endif

    paired_update();

    if (!std::isnan(n_permu)) {
        for (R_xlen_t i = 0; i < n; i++) {
            if (x[i] == y[i]) {
                while (--n > i && x[n] == y[n]) { }
                std::swap(x[i], x[n]);
                std::swap(y[i], y[n]);
            }
        }

        statistic_container.switch_ptr();
        if (n_permu == 0) {
            R_xlen_t swapped = 0;
            for (R_xlen_t i = 0; i < n; i = swapped & (R_xlen_t{ 1 } << i) ? 0 : i + 1) {
                if (i == 0) {
                    paired_update();
                }

                std::swap(x[i], y[i]);
                swapped ^= (1 << i);
            }
        } else {
            const R_xlen_t full_n = (n / 32) * 32;

            auto unif_rand_u32 = []() -> std::uint32_t {
                return unif_rand() * 4294967296.0;
            };

            auto do_flip = [x, y](R_xlen_t start, std::uint32_t flip) mutable {
                while (flip) {
                    R_xlen_t i = start + countr_zero_u32(flip);
                    std::swap(x[i], y[i]);
                    flip &= flip - 1;
                }
            };

            if (n == full_n) {
                do {
                    for (R_xlen_t b = 0; b < full_n; b += 32) {
                        do_flip(b, unif_rand_u32());
                    }
                } while (paired_update());
            } else {
                const std::uint32_t tail_mask = (std::uint32_t { 1 } << (n - full_n)) - 1u;
                do {
                    for (R_xlen_t b = 0; b < full_n; b += 32) {
                        do_flip(b, unif_rand_u32());
                    }
                    do_flip(full_n, unif_rand_u32() & tail_mask);
                } while (paired_update());
            }
        }
    }

    return static_cast<RObject>(statistic_container);
}