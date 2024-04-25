#pragma once

#include <functional>

using twosample_closure = std::function<double(const NumericVector&, const NumericVector&)>;
using twosample_func = twosample_closure(*)(NumericVector, NumericVector);

using ksample_closure = std::function<double(const NumericVector&, const IntegerVector&)>;
using ksample_func = ksample_closure(*)(NumericVector, IntegerVector);

using paired_closure = std::function<double(const NumericVector&, const NumericVector&)>;
using paired_func = paired_closure(*)(NumericVector, NumericVector);

using rcbd_closure = std::function<double(const NumericMatrix&)>;
using rcbd_func = rcbd_closure(*)(NumericMatrix);

using association_closure = std::function<double(const NumericVector&, const NumericVector&)>;
using association_func = association_closure(*)(NumericVector, NumericVector);

using table_closure = std::function<double(const IntegerMatrix&)>;
using table_func = table_closure(*)(IntegerMatrix);