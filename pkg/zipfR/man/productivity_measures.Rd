\name{productivity.measures}
\alias{productivity.measures}
\alias{productivity.measures.tfl}
\alias{productivity.measures.spc}
\alias{productivity.measures.vgc}
\alias{productivity.measures.default}
\title{Measures of Productivity and Lexical Richness (zipfR)}
\encoding{UTF-8}

\description{

  Compute various measures of productivity and lexical richness from
  an observed frequency spectrum, or type-frequency list, from an
  observed vocabulary growth curve or from a vector of tokens.

}

\usage{
productivity.measures(obj, measures, ...)

\method{productivity.measures}{tfl}(obj, measures, ...)
\method{productivity.measures}{spc}(obj, measures, ...)
\method{productivity.measures}{vgc}(obj, measures, ...)

\method{productivity.measures}{default}(obj, measures, ...)
}

\arguments{

  \item{obj}{a suitable data object from which productivity measures
    can be computed. Currently either a frequency spectrum
    (of class \code{spc}), a type-frequency list (of class \code{tfl}),
    a vocabulary growth curve (of class \code{vgc}), or a token vector.}

  \item{measures}{character vector naming the productivity measures to
    be computed (see "Productivity Measures" below).
    Names may be abbreviated as long as they remain unique.
    If unspecified, all supported measures are computed.}

  \item{...}{additional arguments passed on to the method implementations
    (currently, no further arguments are recognized)}

}

\value{

  If \code{obj} is a frequency spectrum, type-frequency list or token vector:
  A numeric vector of the same length as \code{measures} with the corresponding observed values of the productivity measures.

  If \code{obj} is a vocabulary growth curves:
  A numeric matrix with columns corresponding to the selected productivity measures and rows corresponding to the sample sizes of the vocabulary growth curve.

}

\details{

  This function computes productivity measures based on an observed frequency spectrum, type-frequency list or vocabulary growth curve.
  If an \emph{expected} spectrum or VGC is passed, the expectations \eqn{E[V]}, \eqn{E[V_m]} will simply be substituted for the sample values \eqn{V}, \eqn{V_m} in the equations. In most cases, this does \emph{not} yield the expected value of the productivity measure!
  
  Some measures can only be computed from a complete frequency spectrum.  They will return \code{NA} if \code{obj} is an incomplete spectrum or type-frequency list, an expected spectrum or a vocabulary growth curve is passed.
  
  Some other measures can only be computed is a sufficient number of spectrum elements is included in a vocabulary growth curve (usually at least
  \eqn{V_1} and \eqn{V_2}), and will return \code{NA} otherwise.

  Such limitations are indicated in the list of measures below (unless spectrum elements \eqn{V_1} and \eqn{V_2} are sufficient).

  For an expected frequency spectrum or vocabulary growth curve, accuracte expectations can be computed for the measures
  \eqn{R}, \eqn{C}, \eqn{P}, TTR and \eqn{V}.  For \eqn{S}, \eqn{H} and Hapaxes, the expecations are often reasonably
  good approximations (based on a normal approximation of the ratio \eqn{V_m / V} derived from Evert 
  (2004b, Lemma A.8) using an (incorrect) independence assumption for \eqn{V_m} and \eqn{V - V_m}).
  
}

\section{Productivity Measures}{

  The following productivity measures are currently supported:

  \describe{
    \item{\code{K}:}{
      Yule's (1944) \eqn{K = 10000 \cdot \frac{ \sum_m m^2 V_m - N}{ N^2 }}{K = 10000 * (SUM(m) m^2 Vm - N) / N^2} \cr
      (only for complete observed frequency spectrum)
    }
    \item{\code{D}:}{
      Simpson's (1949) \eqn{D = \sum_m V_m \frac{m}{N}\cdot \frac{m-1}{N-1}}{D = SUM(m) Vm * (m / N) * ((m - 1) / (N - 1))} \cr
      (only for complete observed frequency spectrum)
    }
    \item{\code{R}:}{
      Guiraud's (1954) \eqn{R = V / \sqrt{N}} 
    }
    \item{\code{S}:}{
      Sichel's (1975) \eqn{S = V_2 / V}{S = V2 / V}, i.e. the proportion of \emph{dis legomena} 
    }
    \item{\code{H}:}{
      Honoré's (1979) \eqn{H = 100 \frac{ \log N }{ 1 - V_1 / V }}{H = 100 * log(N) / (1 - V1 / V)}, a transformation of the proportion of \emph{hapax legomena} adjusted for sample size
    }
    \item{\code{C}:}{
      Herdan's (1964) \eqn{C = \frac{ \log V }{ \log N }}{C = log(V) / log(N)} 
    }
    \item{\code{P}:}{
      Baayen's (1991) productivity index \eqn{P = \frac{V_1}{N}}{P = V1 / N}, which corresponds to the slope of the vocabulary growth curve (under random sampling assumptions) 
    }
    \item{\code{TTR}:}{
      the type-token ratio TTR = \eqn{V / N}
    }
    \item{\code{Hapax}:}{
      the proportion of \emph{hapax legomena} \eqn{\frac{V_1}{V}}{V1 / V}
    }
    \item{\code{V}:}{
      the total number of types \eqn{V} 
    }
%%    \item{\code{}:}{}
  }

}

\references{

  Evert, Stefan (2004b). \emph{The Statistics of Word Cooccurrences: Word
  Pairs and Collocations.} PhD Thesis, IMS, University of Stuttgart.
  URN urn:nbn:de:bsz:93-opus-23714
  \url{http://elib.uni-stuttgart.de/opus/volltexte/2005/2371/}

}


\seealso{

  \code{\link{lnre.bootstrap}} and \code{\link{bootstrap.confint}} for parametric bootstrapping experiments,
  which help to determine the true expectations and sampling distributions of all productivity measures.
  
}

\keyword{ methods }
\keyword{ univar }

\examples{

## TODO

}
