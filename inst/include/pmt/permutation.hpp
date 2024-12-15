#pragma once

#include <algorithm>
#include <iterator>

template <typename T>
T rand_int(T n)
{
    return static_cast<T>(unif_rand() * n);
}

template <typename T>
void random_shuffle(T first, T last)
{
    using diff_t = typename std::iterator_traits<T>::difference_type;

    diff_t n = std::distance(first, last);

    for (diff_t i = 0; i < n - 1; i++) {
        diff_t j = i + rand_int(n - i);
        std::iter_swap(first + i, first + j);
    }
}

template <typename T>
bool next_permutation(T first, T last)
{
    return std::next_permutation(first, last);
}

template <typename T>
double n_permutation(T first, T last)
{
    double A = 1.0;

    typename std::iterator_traits<T>::difference_type rep = 0;

    auto val = *first;
    for (auto it = first; it != last; it++) {
        A *= std::distance(first, it) + 1;
        if (*it == val) {
            A /= ++rep;
        } else {
            rep = 1;
            val = *it;
        }
    }

    return A;
}

template <typename T>
auto random_shuffle(T& v)
{
    return random_shuffle(v.begin(), v.end());
}

template <typename T>
auto next_permutation(T& v)
{
    return next_permutation(v.begin(), v.end());
}

template <typename T>
auto n_permutation(const T& v)
{
    return n_permutation(v.begin(), v.end());
}