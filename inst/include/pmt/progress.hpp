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

double elapsed(std::chrono::steady_clock::time_point& time)
{
    std::chrono::steady_clock::time_point now = std::chrono::steady_clock::now();
    std::chrono::duration<double, typename std::chrono::seconds::period> elapsed = now - time;
    time = now;

    return elapsed.count();
}

template <bool progress>
class Stat {
public:
    Stat() :
        _progress_every(0),
        _progress_i(0) { }

    void allocate(R_xlen_t size, double n_permu)
    {
        _statistic = Rf_allocVector(REALSXP, size);

        _statistic_ptr = REAL(_statistic);
        _statistic_end = _statistic_ptr + size;

        if (!std::isnan(n_permu)) {
            Shield<SEXP> statistic_permu(Rf_allocVector(REALSXP, size * n_permu));
            if (size > 1) {
                Rf_setAttrib(statistic_permu, R_DimSymbol, IntegerVector::create(size, n_permu));
            }
            _statistic.attr("permu") = statistic_permu;
        }

        _time = std::chrono::steady_clock::now();
    }

    void switch_ptr()
    {
        _speed = 1 / elapsed(_time);

        _statistic_begin = REAL(_statistic.attr("permu"));
        _statistic_end = _statistic_begin + Rf_xlength(_statistic.attr("permu"));
        _statistic_ptr = _statistic_begin;

        _init_progress();
    }

    bool operator<<(double statistic)
    {
        *_statistic_ptr++ = statistic;

        _update_progress();

        return _statistic_ptr != _statistic_end;
    }

    explicit operator RObject()
    {
        return _statistic;
    }

    ~Stat()
    {
        _clear_progress();
    }

private:
    RObject _statistic;

    double* _statistic_begin;
    double* _statistic_end;
    double* _statistic_ptr;

    R_xlen_t _progress_every;
    R_xlen_t _progress_i;

    void _init_progress();

    void _update_progress();

    void _clear_progress();

    double _speed;

    std::chrono::steady_clock::time_point _time;
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
    _progress_every = std::max(1.0, _speed * 8);
    _progress_i = 0;

    _time = std::chrono::steady_clock::now();

    Rprintf(generated_bars[0].data(), _speed, (_statistic_end - _statistic_begin) / _speed);
}

template <>
void Stat<true>::_update_progress()
{
    if (++_progress_i == _progress_every) {
        _progress_i = 0;

        _speed = 0.2 * _speed + 0.8 * _progress_every / elapsed(_time);

        Rprintf(generated_bars[100 * (_statistic_ptr - _statistic_begin) / (_statistic_end - _statistic_begin)].data(), _speed, (_statistic_end - _statistic_ptr) / _speed);
    }
}

template <>
void Stat<true>::_clear_progress()
{
    Rcout << "\015\033[K\033[0m";
}