#pragma once

#include <array>
#include <utility>

constexpr unsigned bar_width = 50;

using progress_bar = std::array<char, bar_width + 19>;

constexpr std::array<char, 10> num_char_map = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };

template <unsigned n, unsigned... seq>
constexpr progress_bar generate_bar(std::integer_sequence<unsigned, seq...>)
{
    constexpr unsigned fill = n * bar_width / 100;
    return {
        '\015',
        '\033', '[', '3', '1', 'm',
        (n < 10) ? ' ' : num_char_map[n / 10],
        (n < 10) ? num_char_map[n] : num_char_map[n % 10], '%',
        '\033', '[', '3', '6', 'm',
        ' ', '|', (seq + 1 < fill ? '-' : (seq + 1 == fill ? '>' : ' '))..., '|', ' ',
        '\0'
    };
}

template <unsigned... seq>
constexpr std::array<progress_bar, sizeof...(seq)> generate_bars(std::integer_sequence<unsigned, seq...>)
{
    return { generate_bar<seq>(std::make_integer_sequence<unsigned, bar_width>())... };
}

constexpr auto generated_bars = generate_bars(std::make_integer_sequence<unsigned, 100>());

template <bool progress>
class Stat {
public:
    Stat() :
        _progress_every(0),
        _progress_i(0) { }

    template <typename T>
    void init(T&& update, R_xlen_t size)
    {
        _init_buffer(size);

        update();
        _statistic = std::exchange(_buffer, NumericVector(0));
    }

    template <typename T>
    void init(T&& update, R_xlen_t size, double n_permu)
    {
        double n = n_permu * size;
        if (n > R_XLEN_T_MAX) {
            stop("Too many permutations");
        }

        _init_buffer(size);

        update();
        _statistic = _buffer;

        _init_buffer(static_cast<R_xlen_t>(n));

        if (size > 1) {
            _buffer.attr("dim") = Dimension(size, n_permu);
        }

        _init_progress();
    }

    bool operator<<(double statistic)
    {
        _update_progress();

        _buffer[_buffer_i++] = statistic;

        return _buffer_i != _buffer_size;
    }

    explicit operator RObject()
    {
        _clear_progress();

        _statistic.attr("permu") = _buffer;

        return _statistic;
    };

private:
    RObject _statistic;

    NumericVector _buffer;

    R_xlen_t _buffer_size;
    R_xlen_t _buffer_i;

    R_xlen_t _progress_every;
    R_xlen_t _progress_i;

    void _init_buffer(R_xlen_t size)
    {
        _buffer = NumericVector(no_init(size));

        _buffer_i = 0;
        _buffer_size = size;
    }

    void _init_progress();

    void _update_progress();

    void _clear_progress();
};

template <>
void Stat<false>::_init_progress() { }

template <>
void Stat<false>::_update_progress() { }

template <>
void Stat<false>::_clear_progress() { }

template <>
void Stat<true>::_init_progress()
{
    _progress_i = 0;
    _progress_every = _buffer_size < 100 ? 1 : _buffer_size / 100;

    Rcout << generated_bars[0].data();
}

template <>
void Stat<true>::_update_progress()
{
    if (++_progress_i == _progress_every) {
        _progress_i = 0;

        Rcout << generated_bars[static_cast<int>(100 * _buffer_i / _buffer_size)].data();
    }
}

template <>
void Stat<true>::_clear_progress()
{
    Rcout << "\015\033[K\033[0m";
}