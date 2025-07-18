#pragma once

#include <array>
#include <chrono>
#include <utility>

constexpr unsigned bar_width = 32;

using progress_bar = std::array<char, bar_width + 44>;

constexpr std::array<char, 10> num_char_map = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };

template <unsigned percent, unsigned... bar_seq>
constexpr progress_bar generate_bar(std::integer_sequence<unsigned, bar_seq...>)
{
    constexpr unsigned fill = percent * bar_width / 100;
    return {
        '\015',
        '\033', '[', 'K',
        '\033', '[', '3', '1', 'm',
        (percent < 10) ? ' ' : num_char_map[percent / 10],
        (percent < 10) ? num_char_map[percent] : num_char_map[percent % 10], '%', '%',
        '\033', '[', '3', '6', 'm',
        ' ', (bar_seq + 1 < fill ? '=' : (bar_seq + 1 == fill ? '>' : '-'))..., ' ',
        '%', '.', '3', 'g', '/', 's',
        '\033', '[', '3', '2', 'm',
        ' ', 'E', 'T', 'A', ':', ' ',
        '%', '.', '3', 'g', 's', ' ',
        '\0'
    };
}

template <unsigned... percent_seq>
constexpr std::array<progress_bar, sizeof...(percent_seq)> generate_bars(std::integer_sequence<unsigned, percent_seq...>)
{
    return { generate_bar<percent_seq>(std::make_integer_sequence<unsigned, bar_width>())... };
}

constexpr auto generated_bars = generate_bars(std::make_integer_sequence<unsigned, 100>());

template <bool progress>
class Stat {
public:
    Stat() :
        _progress_every(0),
        _progress_i(0) { }

    template <typename T>
    void init(const T& update, R_xlen_t size)
    {
        _init_statistic(update, size);

        _buffer = NumericVector(0);
    }

    template <typename T>
    void init(const T& update, R_xlen_t size, double n_permu)
    {
        double n = n_permu * size;
        if (n > R_XLEN_T_MAX) {
            stop("Too many permutations");
        }

        _init_statistic(update, size);

        _init_buffer(static_cast<R_xlen_t>(n));
        if (size > 1) {
            _buffer.attr("dim") = IntegerVector::create(size, n_permu);
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
        _statistic.attr("permu") = _buffer;

        return _statistic;
    }

    ~Stat()
    {
        _clear_progress();
    }

private:
    RObject _statistic;

    template <typename T>
    void _init_statistic(const T& update, R_xlen_t size);

    NumericVector _buffer;

    R_xlen_t _buffer_size;
    R_xlen_t _buffer_i;

    void _init_buffer(R_xlen_t size)
    {
        _buffer = NumericVector(no_init(size));

        _buffer_size = size;
        _buffer_i = 0;
    }

    R_xlen_t _progress_every;
    R_xlen_t _progress_i;

    void _init_progress();

    void _update_progress();

    void _clear_progress();

    double _speed;

    std::chrono::steady_clock::time_point _time;
};

template <>
template <typename T>
void Stat<false>::_init_statistic(const T& update, R_xlen_t size)
{
    _init_buffer(size);

    update();

    _statistic = _buffer;
}

template <>
void Stat<false>::_init_progress() { }

template <>
void Stat<false>::_update_progress() { }

template <>
void Stat<false>::_clear_progress() { }

double elapsed(std::chrono::steady_clock::time_point& time)
{
    std::chrono::steady_clock::time_point now = std::chrono::steady_clock::now();
    std::chrono::duration<double, typename std::chrono::seconds::period> elapsed = now - time;
    time = now;

    return elapsed.count();
}

template <>
template <typename T>
void Stat<true>::_init_statistic(const T& update, R_xlen_t size)
{
    _init_buffer(size);

    _time = std::chrono::steady_clock::now();
    update();
    _speed = size / elapsed(_time);

    _statistic = _buffer;
}

template <>
void Stat<true>::_init_progress()
{
    _progress_every = _buffer_size < 100 ? 1 : _buffer_size / 100;
    _progress_i = 0;

    double eta = _buffer_size / _speed;

    Rprintf(generated_bars[0].data(), _speed, eta);
}

template <>
void Stat<true>::_update_progress()
{
    if (++_progress_i == _progress_every) {
        _progress_i = 0;

        _speed = 0.2 * _speed + 0.8 * _progress_every / elapsed(_time);

        double eta = (_buffer_size - _buffer_i) / _speed;

        Rprintf(generated_bars[100 * _buffer_i / _buffer_size].data(), _speed, eta);
    }
}

template <>
void Stat<true>::_clear_progress()
{
    Rcout << "\015\033[K\033[0m";
}