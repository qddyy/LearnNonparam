% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PairedDifference.R
\name{PairedDifference}
\alias{PairedDifference}
\alias{paired.difference}
\title{Paired Comparison Based on Differences}
\description{
Performs differences based paired comparison on samples.
}
\examples{
pmt(
    "paired.difference",
    alternative = "greater", scoring = "none", n_permu = 0
)$test(Table4.1.1)$print()

pmt(
    "paired.difference", n_permu = 0
)$test(Table4.1.3)$print()

t <- pmt(
    "paired.difference", scoring = "rank",
    alternative = "greater", n_permu = 0
)$test(Table4.1.1)$print()

t$type <- "asymp"
t

}
\section{Super classes}{
\code{\link[LearnNonparam:PermuTest]{LearnNonparam::PermuTest}} -> \code{\link[LearnNonparam:TwoSampleTest]{LearnNonparam::TwoSampleTest}} -> \code{\link[LearnNonparam:TwoSamplePairedTest]{LearnNonparam::TwoSamplePairedTest}} -> \code{PairedDifference}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{correct}}{Whether to apply continuity correction when \code{scoring} is set to \code{"rank"}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PairedDifference-new}{\code{PairedDifference$new()}}
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
\if{html}{\out{<a id="method-PairedDifference-new"></a>}}
\if{latex}{\out{\hypertarget{method-PairedDifference-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{PairedDifference} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PairedDifference$new(
  type = c("permu", "asymp"),
  method = c("with_zeros", "without_zeros"),
  scoring = c("none", "rank", "vw", "expon"),
  alternative = c("two_sided", "less", "greater"),
  null_value = 0,
  n_permu = 10000,
  correct = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{a character string specifying the way to calculate the p-value.}

\item{\code{method}}{a character string specifying the method of ranking data in computing adjusted signed scores for tied data, must be one of \code{"with_zeros"} (default) or \code{"without_zeros"}.}

\item{\code{scoring}}{a character string specifying the scoring system.}

\item{\code{alternative}}{a character string specifying the alternative hypothesis.}

\item{\code{null_value}}{a number indicating the true value of the location shift.}

\item{\code{n_permu}}{an integer indicating number of permutations for the permutation distribution. If set to \code{0}, all permutations will be used.}

\item{\code{correct}}{a logical indicating whether to apply continuity correction in the normal approximation for the p-value when \code{scoring} is set to \code{"rank"}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{PairedDifference} object.
}
}
}
