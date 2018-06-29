\name{productivity.measures}
\alias{productivity.measures}
\title{Measures of Productivity and Lexical Richness (zipfR)}
\encoding{UTF-8}

\description{

  Compute various measures of productivity and lexical richness from
  an observed or expected frequency spectrum or vocabulary growth curve.

}

\usage{

productivity.measures(obj, measures, ...)

}

\arguments{

  \item{obj}{a suitable data object from which productivity measures
    can be computed. Currently either a frequency spectrum
    (of class \code{spc}) or a vocabulary growth curve (of class \code{vgc}).}

  \item{measures}{character vector naming the productivity measures to
    be computed (see "Productivity Measures" below).
    Names may be abbreviated as long as they remain unique.}

  \item{...}{additional arguments passed on to the method implementation
    (see respective manpages for details)}
  
}

\value{

  If \code{obj} is a frequency spectrum: A numeric vector of the same length as \code{measures} with the
  corresponding observed or expected values of the productivity measures.

  If \code{obj} is a vocabulary growth curves: A numeric matrix with columns corresponding to the selected
  productivity measures and rows corresponding to the sample sizes of the vocabulary growth curve.
  
}

\details{

  For an observed frequency spectrum or vocabulary growth curve, sample estimates of the productivity measures are computed.
  In the case of vocabulary growth curves, computation is only possible if a sufficient number of spectrum elements (usually
  \eqn{V_1} and \eqn{V_2}) are available. \eqn{L} and \eqn{D} are not supported for vocabulary growth curves. Any unavailable
  measures appear as missing value (\code{NA}) in the result vector.

  For an expected frequency spectrum or vocabulary growth curve, the expectations of the productivity measures are computed.
  They are exact for \eqn{K}, \eqn{D}, \eqn{R}, \eqn{C}, \eqn{P}, TTR and \eqn{V}, but only approximations for \eqn{S}, \eqn{H} and Hapax.
  A suitable number of expected spectrum elements are required for \eqn{K} and \eqn{D}, which are not available for vocbulary growth curves.
  
}

\section{Productivity Measures}{

  The following productivity measures are currently supported:

  \describe{
    \item{\code{K}:}{Yule's (1944) \eqn{K = 10000 \cdot \frac{ \sum_m m^2 V_m - N}{ N^2 }}{K = 10000 * (SUM(m) m^2 Vm - N) / N^2} }
    \item{\code{D}:}{Simpson's (1949) \eqn{D = \sum_m V_m \frac{m}{N}\cdot \frac{m-1}{N-1}}{D = SUM(m) Vm * (m / N) * ((m - 1) / (N - 1))} }
    \item{\code{R}:}{Guiraud's (1954) \eqn{R = V / \sqrt{N}} }
    \item{\code{S}:}{Sichel's (1975) \eqn{S = V_2 / V}{S = V2 / V}, i.e. the proportion of \emph{dis legomena} }
    \item{\code{H}:}{Honoré's (1979) \eqn{H = 100 \frac{ \log N }{ 1 - V_1 / V }}{H = 100 * log(N) / (1 - V1 / V)}, a transformation of the proportion of \emph{hapax legomena} adjusted for sample size}
    \item{\code{C}:}{Herdan's (1964) \eqn{C = \frac{ \log V }{ \log N }}{C = log(V) / log(N)} }
    \item{\code{P}:}{Baayen's (1991) productivity index \eqn{P = \frac{V_1}{N}}{P = V1 / N}, which corresponds to the slope of the vocabulary growth curve (under random sampling assumptions) }
    \item{\code{TTR}:}{the type-token ratio TTR = \eqn{V / N} }
    \item{\code{Hapax}:}{the proportion of \emph{hapax legomena} \eqn{\frac{V_1}{V}}{V1 / V} }
    \item{\code{V}:}{the total number of types \eqn{V} }
%%    \item{\code{}:}{}
  }

}

\seealso{

  Methods \code{\link{productivity.measures.spc}} (for observed and expected frequency spectra)
  and \code{\link{productivity.measures.vgc}} (for observed and expected vocabulary growth curves).
  
}

\keyword{ methods }
\keyword{ univar }

\examples{

## TODO

}
