#pragma once

#include <array>
#include <utility>

constexpr unsigned bar_width = 50;

using ProgressBar = std::array<char, bar_width + 19>;

constexpr std::array<char, 10> num_char_map = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };

template <unsigned n, unsigned... Is>
constexpr auto generate_bar(std::integer_sequence<unsigned, Is...>)
{
#define SET_RED '\033', '[', '3', '1', 'm'
#define SET_GREEN '\033', '[', '3', '2', 'm'
#define BAR_WITHOUT_PERCENT ' ', '[', (Is < n * bar_width / 100 ? '=' : ' ')..., ']', ' ', '\0'

    return (n < 10) ?
        ProgressBar {
            '\015',
            SET_RED, ' ', num_char_map[n], '%',
            SET_GREEN, BAR_WITHOUT_PERCENT
        } :
        ProgressBar {
            '\015',
            SET_RED, num_char_map[n / 10], num_char_map[n % 10], '%',
            SET_GREEN, BAR_WITHOUT_PERCENT
        };
}

template <unsigned... Is>
constexpr auto generate_bars(std::integer_sequence<unsigned, Is...>)
{
    return std::array<ProgressBar, sizeof...(Is)> {
        generate_bar<Is>(std::make_integer_sequence<unsigned, bar_width>())...
    };
}

constexpr auto generated_bars = generate_bars(std::make_integer_sequence<unsigned, 100>());

template <typename T>
class PermuBarBase {
private:
    T& _Derived()
    {
        return static_cast<T&>(*this);
    }

public:
    template <typename U>
    void init(R_xlen_t n_permu, U update_lambda, R_len_t statistic_size = 1)
    {
        _init_statistic_buffer(statistic_size, 1);
        update_lambda();
        _statistic = _statistic_buffer;

        _Derived().init_impl(n_permu, statistic_size);
    }

    bool operator<<(double statistic)
    {
        return _Derived().update_impl(statistic);
    }

    bool operator<<(SEXP statistic)
    {
        return _Derived().update_impl(as<double>(statistic));
    }

    NumericVector close()
    {
        _statistic.attr("permu") = _statistic_buffer;

        return _Derived().close_impl();
    }

protected:
    NumericVector _statistic;

    R_xlen_t _buffer_i;
    R_xlen_t _buffer_size;

    NumericVector _statistic_buffer;

    void _init_statistic_buffer(R_xlen_t n_statistic, R_len_t statistic_size)
    {
        _statistic_buffer = NumericVector(no_init(n_statistic * statistic_size));

        _buffer_i = 0;
        _buffer_size = _statistic_buffer.size();

        if (statistic_size > 1) {
            _statistic_buffer.attr("dim") = IntegerVector::create(statistic_size, n_statistic);
        }
    }

    bool _update_double(double statistic)
    {
        _statistic_buffer[_buffer_i++] = statistic;

        return _buffer_i != _buffer_size;
    }
};

class PermuBarHide : public PermuBarBase<PermuBarHide> {
public:
    void init_impl(R_xlen_t n_permu, R_len_t statistic_size)
    {
        _init_statistic_buffer(n_permu, statistic_size);
    }

    bool update_impl(double statistic)
    {
        return _update_double(statistic);
    }

    NumericVector close_impl()
    {
        return _statistic;
    }
};

class PermuBarShow : public PermuBarBase<PermuBarShow> {
public:
    void init_impl(R_xlen_t n_permu, R_len_t statistic_size)
    {
        _init_statistic_buffer(n_permu, statistic_size);

        _update_i = 0;
        _update_every = (_buffer_size < 100) ? 1 : _buffer_size / 100;

        _print();
    }

    bool update_impl(double statistic)
    {
        _update_i++;
        if (_update_i == _update_every) {
            _update_i = 0;
            _print();
        }

        return _update_double(statistic);
    }

    NumericVector close_impl()
    {
        Rcout << "\015\033[K\033[0m";

        return _statistic;
    }

private:
    R_xlen_t _update_i = 0;
    R_xlen_t _update_every = 2;

    void _print()
    {
        unsigned percent = static_cast<unsigned>(100 * _buffer_i / _buffer_size);

        Rcout << generated_bars[percent].data();
    }
};