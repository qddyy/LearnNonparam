#pragma once

#include <algorithm>
#include <cstring>
#include <iterator>
#include <type_traits>
#include <unordered_map>

template <typename T, typename = std::enable_if_t<std::is_trivially_copyable<T>::value>>
void swap_if(bool c, T& a, T& b) noexcept
{
    struct alignas(T) {
        unsigned char value[sizeof(T)];
    } buffer[2];
    std::memcpy(buffer[1].value, &b, sizeof(T));
    std::memcpy(buffer[0].value, &a, sizeof(T));
    std::memcpy(&a, buffer[c].value, sizeof(T));
    std::memcpy(&b, buffer[1 - c].value, sizeof(T));
}

template <typename T>
void random_shuffle(T first, T last)
{
    auto n = std::distance(first, last);

    for (decltype(n) i = 0; i < n - 1; i++) {
        std::iter_swap(first + i, first + i + static_cast<decltype(n)>(unif_rand() * (n - i)));
    }
}

template <typename T>
bool next_permutation(T first, T last)
{
    return std::next_permutation(first, last);
}

template <typename T, typename = std::enable_if_t<std::is_integral<T>::value>>
double C(T n, T k)
{
    T i = 0, j = n - k;

    double C = 1.0;
    while (i < k) {
        C *= ++j;
        C /= ++i;
    }

    return C;
}

template <typename T>
double n_permutation(T first, T last)
{
    std::unordered_map<typename std::iterator_traits<T>::value_type, typename std::iterator_traits<T>::difference_type> freq;
    freq.reserve(std::distance(first, last));
    for (auto it = first; it != last; it++) {
        freq[*it]++;
    }

    double A = 1.0;
    typename decltype(freq)::mapped_type n = 0;
    for (auto it = freq.begin(); it != freq.end(); it++) {
        n += it->second;
        A *= C(n, it->second);
    }

    return A;
}

template <typename T>
auto random_shuffle(T&& v)
{
    return ::random_shuffle(v.begin(), v.end());
}

template <typename T>
auto next_permutation(T&& v)
{
    return ::next_permutation(v.begin(), v.end());
}

template <typename T>
auto n_permutation(T&& v)
{
    return ::n_permutation(v.begin(), v.end());
}