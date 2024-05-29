#pragma once

#include <array>
#include <utility>

constexpr unsigned bar_width = 50;

using ProgressBar = std::array<char, bar_width + 19>;

constexpr std::array<char, 10> num_char_map = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };

template <unsigned n, unsigned... Is>
constexpr auto generate_bar(std::integer_sequence<unsigned, Is...>)
{
    unsigned fill = n * bar_width / 100;
    return ProgressBar {
        '\015',
        '\033', '[', '3', '1', 'm',
        (n < 10) ? ' ' : num_char_map[n / 10],
        (n < 10) ? num_char_map[n] : num_char_map[n % 10], '%',
        '\033', '[', '3', '6', 'm',
        ' ', '|', (Is + 1 < fill ? '-' : (Is + 1 == fill ? '>' : ' '))..., '|', ' ',
        '\0'
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

class PermuBarHide {
public:
    template <typename T>
    void init(R_xlen_t n_permu, T update, R_len_t statistic_size = 1)
    {
        _init_statistic_buffer(statistic_size, 1);
        update();
        _statistic = _statistic_buffer;

        _init_statistic_buffer(n_permu, statistic_size);
    }

    bool operator<<(double statistic)
    {
        _statistic_buffer[_buffer_i++] = statistic;

        return _buffer_i != _buffer_size;
    }

    NumericVector close()
    {
        _statistic.attr("permu") = _statistic_buffer;

        return _statistic;
    }

protected:
    R_xlen_t _buffer_i;
    R_xlen_t _buffer_size;

private:
    NumericVector _statistic;

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
};

class PermuBarShow : public PermuBarHide {
public:
    template <typename... Args>
    auto init(Args&&... args)
    {
        PermuBarHide::init(std::forward<Args>(args)...);

        _show_i = 0;
        _show_every = (_buffer_size < 100) ? 1 : _buffer_size / 100;

        _show();
    }

    template <typename... Args>
    auto operator<<(Args&&... args)
    {
        if (++_show_i == _show_every) {
            _show_i = 0;
            _show();
        }

        return PermuBarHide::operator<<(std::forward<Args>(args)...);
    }

    auto close()
    {
        Rcout << "\015\033[K\033[0m";

        return PermuBarHide::close();
    }

private:
    R_xlen_t _show_i = 0;
    R_xlen_t _show_every = 2;

    void _show()
    {
        unsigned percent = static_cast<unsigned>(100 * _buffer_i / _buffer_size);

        Rcout << generated_bars[percent].data();
    }
};