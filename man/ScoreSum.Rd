% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScoreSum.R
\name{ScoreSum}
\alias{ScoreSum}
\alias{twosample.scoresum}
\title{Two-Sample Test Based on Sum of Scores}
\description{
Performs sum of scores based two-sample test on samples. It is almost the same as two-sample wilcoxon rank sum test but uses more scoring systems.
}
\examples{
pmt(
    "twosample.scoresum", scoring = "expon",
    alternative = "greater", n_permu = 0
)$test(Table2.6.2)$print()

}
\section{Super classes}{
\code{\link[LearnNonparam:PermuTest]{LearnNonparam::PermuTest}} -> \code{\link[LearnNonparam:TwoSampleTest]{LearnNonparam::TwoSampleTest}} -> \code{\link[LearnNonparam:TwoSampleLocationTest]{LearnNonparam::TwoSampleLocationTest}} -> \code{ScoreSum}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ScoreSum-new}{\code{ScoreSum$new()}}
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
\if{html}{\out{<a id="method-ScoreSum-new"></a>}}
\if{latex}{\out{\hypertarget{method-ScoreSum-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{ScoreSum} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScoreSum$new(
  scoring = c("rank", "vw", "expon"),
  alternative = c("two_sided", "less", "greater"),
  null_value = 0,
  n_permu = 10000
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{scoring}}{a character string specifying the scoring system.}

\item{\code{alternative}}{a character string specifying the alternative hypothesis.}

\item{\code{null_value}}{a number indicating the true value of the location shift.}

\item{\code{n_permu}}{an integer indicating number of permutations for the permutation distribution. If set to \code{0}, all permutations will be used.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{ScoreSum} object.
}
}
}
