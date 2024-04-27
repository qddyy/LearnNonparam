#pragma once

#include <algorithm>

template <typename T, typename... Args>
void sort(T&& v, Args... compare)
{
    std::sort(v.begin(), v.end(), compare...);
}

template <typename T>
T rand_int(T n)
{
    return floor(unif_rand() * n);
}

template <typename T>
void random_shuffle(T v)
{
    R_len_t n = v.size();
    for (R_len_t i = 0; i < n - 1; i++) {
        R_len_t j = i + rand_int(n - i);
        std::swap(v[i], v[j]);
    }
}

template <typename T>
bool next_permutation(T v)
{
    return std::next_permutation(v.begin(), v.end());
}

template <typename T>
R_xlen_t n_permutation(T v)
{
    double A = 1;

    R_len_t n = v.size();
    R_len_t n_i = 0;
    double current = v[0];
    for (R_len_t i = 0; i < n; i++) {
        A *= (i + 1);
        if (v[i] == current) {
            A /= ++n_i;
        } else {
            n_i = 1;
        }
        current = v[i];
    }

    return static_cast<R_xlen_t>(A);
}