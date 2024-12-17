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
    Stat(R_xlen_t statistic_size = 1) :
        _progress_i(0),
        _progress_every(2),
        _statistic_size(statistic_size) { }

    template <typename T>
    void init_statistic(T& update)
    {
        _init_statistic_buffer(_statistic_size, 1);

        update();

        _statistic = _statistic_buffer;
        _statistic_buffer = NumericVector(0);
    }

    void init_statistic_permu(double n_permu)
    {
        _init_statistic_buffer(n_permu, _statistic_size);

        _init_progress();
    }

    bool operator<<(double statistic)
    {
        _update_progress();

        _statistic_buffer[_buffer_i++] = statistic;

        return _buffer_i != _buffer_size;
    }

    RObject close()
    {
        _clear_progress();

        _statistic.attr("permu") = _statistic_buffer;

        return _statistic;
    };

private:
    RObject _statistic;

    NumericVector _statistic_buffer;

    R_xlen_t _buffer_i;
    R_xlen_t _buffer_size;

    R_xlen_t _progress_i;
    R_xlen_t _progress_every;

    R_xlen_t _statistic_size;

    void _init_statistic_buffer(double n, R_xlen_t size)
    {
        double total = n * size;
        if (total <= 0 || total > R_XLEN_T_MAX) {
            stop("Too many permutations");
        }

        _statistic_buffer = NumericVector(no_init(static_cast<R_xlen_t>(total)));

        _buffer_i = 0;
        _buffer_size = _statistic_buffer.size();

        if (size > 1) {
            _statistic_buffer.attr("dim") = IntegerVector::create(size, n);
        }
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
    _progress_every = (_buffer_size < 100) ? 1 : _buffer_size / 100;

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