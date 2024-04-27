#pragma once

#include <functional>

using twosample_closure = std::function<double(const NumericVector, const NumericVector)>;
using twosample_func = std::function<twosample_closure(NumericVector, NumericVector)>;

using ksample_closure = std::function<double(const NumericVector, const IntegerVector&)>;
using ksample_func = std::function<ksample_closure(NumericVector, IntegerVector)>;

using paired_closure = std::function<double(const NumericVector, const NumericVector)>;
using paired_func = std::function<paired_closure(NumericVector, NumericVector)>;

using rcbd_closure = std::function<double(const NumericMatrix)>;
using rcbd_func = std::function<rcbd_closure(NumericMatrix)>;

using association_closure = std::function<double(const NumericVector, const NumericVector)>;
using association_func = std::function<association_closure(NumericVector, NumericVector)>;

using table_closure = std::function<double(const IntegerMatrix)>;
using table_func = std::function<table_closure(IntegerMatrix)>;