% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sign.R
\name{Sign}
\alias{Sign}
\alias{paired.sign}
\title{Two-Sample Sign Test}
\description{
Performs two-sample sign test on samples.
}
\examples{
t <- pmt(
    "paired.sign",
    alternative = "greater", n_permu = 0
)$test(
    rep(c(+1, -1), c(12, 5)), rep(0, 17)
)$print()

t$type <- "asymp"
t

}
\section{Super classes}{
\code{\link[LearnNonparam:PermuTest]{LearnNonparam::PermuTest}} -> \code{\link[LearnNonparam:TwoSampleTest]{LearnNonparam::TwoSampleTest}} -> \code{\link[LearnNonparam:TwoSamplePairedTest]{LearnNonparam::TwoSamplePairedTest}} -> \code{Sign}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{correct}}{Whether to apply continuity correction.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Sign-new}{\code{Sign$new()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="LearnNonparam" data-topic="PermuTest" data-id="plot"><a href='../../LearnNonparam/html/PermuTest.html#method-PermuTest-plot'><code>LearnNonparam::PermuTest$plot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LearnNonparam" data-topic="PermuTest" data-id="print"><a href='../../LearnNonparam/html/PermuTest.html#method-PermuTest-print'><code>LearnNonparam::PermuTest$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LearnNonparam" data-topic="PermuTest" data-id="test"><a href='../../LearnNonparam/html/PermuTest.html#method-PermuTest-test'><code>LearnNonparam::PermuTest$test()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Sign-new"></a>}}
\if{latex}{\out{\hypertarget{method-Sign-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{Sign} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Sign$new(
  type = c("permu", "asymp"),
  alternative = c("two_sided", "less", "greater"),
  n_permu = 10000,
  correct = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{a character string specifying the way to calculate the p-value.}

\item{\code{alternative}}{a character string specifying the alternative hypothesis.}

\item{\code{n_permu}}{an integer indicating number of permutations for the permutation distribution. If set to \code{0}, all permutations will be used.}

\item{\code{correct}}{a logical indicating whether to apply continuity correction in the normal approximation for the p-value.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Sign} object.
}
}
}
