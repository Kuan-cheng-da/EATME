% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EWMA_p_two.R
\name{EWMA_p_two}
\alias{EWMA_p_two}
\title{The two-sided control limits of an EWMA-p chart}
\usage{
EWMA_p_two(p, lambda, n, pi1 = 1, pi2 = pi1, ARL0 = 200, M = 500, error = 10)
}
\arguments{
\item{p}{The proportion of defectives in the in-control process.}

\item{lambda}{An EWMA smooth constant, which is a scalar in [0,1].}

\item{n}{A sample size in the data.}

\item{pi1}{The proportion that the observed defectives are the same as unobserved ones.}

\item{pi2}{The proportion that the observed non-defectives are the same as unobserved ones.}

\item{ARL0}{A prespecified average run length (ARL) of a control chart in the in-control process.}

\item{M}{The number of simulation times for the Monte Carlo method}

\item{error}{The tolerant for the absolute different between an itevated ARL calue and prespecified \code{ARL0}.}
}
\value{
\code{L1}\eqn{\hspace{2.2cm}} The coefficient of the upper control limit.

\code{L2}\eqn{\hspace{2.2cm}} The coefficient of the lower control limit.

\code{hat_ARL0}\eqn{\hspace{1.1cm}} The estimated in-control average run length based on given \code{L1} and \code{L2}.

\code{hat_MRL0}\eqn{\hspace{1.1cm}} The estimated in-control median of run length based on given \code{L1} and \code{L2}.

\code{hat_SDRL0}\eqn{\hspace{0.9cm}} The estimated in-control standard deviation of run length based on given \code{L1} and \code{L2}.

\code{UCL}\eqn{\hspace{2cm}} The limiting value of the upper control limit with \code{L1}.

\code{LCL}\eqn{\hspace{2cm}} The limiting value of the lower control limit with \code{L2}.
}
\description{
This function is used to calculate the two-sided control limit for EWMA-p charts with the correction of measurement error effects.
If two truly classified probabilities \code{pi1} and \code{pi2} are given by 1, then the corresponding control limit is free of measurement error.
}
\examples{
EWMA_p_two(0.2,0.05,5,1,1)
}
