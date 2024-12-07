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

class PermuBarHide {
public:
    PermuBarHide(const R_len_t statistic_size = 1) :
        _statistic_size(statistic_size) { }

    template <typename T>
    void init_statistic(const T& update_bar)
    {
        _init_statistic_buffer(_statistic_size, 1);

        update_bar();

        _statistic = _statistic_buffer;
        _statistic_buffer = NumericVector(0);
    }

    void init_statistic_permu(const double n_permu)
    {
        _init_statistic_buffer(n_permu, _statistic_size);
    }

    bool operator<<(const double statistic)
    {
        _statistic_buffer[_buffer_i++] = statistic;

        return _buffer_i != _buffer_size;
    }

    NumericVector close()
    {
        _statistic.attr("permu") = _statistic_buffer;

        return _statistic;
    }

private:
    const R_len_t _statistic_size;

    NumericVector _statistic;

    NumericVector _statistic_buffer;

    void _init_statistic_buffer(const double n, const R_len_t size)
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

protected:
    R_xlen_t _buffer_i;
    R_xlen_t _buffer_size;
};

class PermuBarShow : public PermuBarHide {
public:
    template <typename... Args>
    PermuBarShow(Args&&... args) :
        PermuBarHide(std::forward<Args>(args)...),
        _show_i(0),
        _show_every(2) { }

    template <typename... Args>
    auto init_statistic_permu(Args&&... args)
    {
        PermuBarHide::init_statistic_permu(std::forward<Args>(args)...);

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

    template <typename... Args>
    auto close(Args&&... args)
    {
        Rcout << "\015\033[K\033[0m";

        return PermuBarHide::close(std::forward<Args>(args)...);
    }

private:
    R_xlen_t _show_i;
    R_xlen_t _show_every;

    void _show() const
    {
        Rcout << generated_bars[static_cast<int>(100 * _buffer_i / _buffer_size)].data();
    }
};