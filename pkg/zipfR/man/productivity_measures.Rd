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
  an observed frequency spectrum or type-frequency list, from an
  observed vocabulary growth curve, or from a vector of tokens.

}

\usage{
productivity.measures(obj, measures, data.frame=TRUE, ...)

\method{productivity.measures}{tfl}(obj, measures, data.frame=TRUE, ...)
\method{productivity.measures}{spc}(obj, measures, data.frame=TRUE, ...)
\method{productivity.measures}{vgc}(obj, measures, data.frame=TRUE, ...)

\method{productivity.measures}{default}(obj, measures, data.frame=TRUE, ...)
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
    
  \item{data.frame}{if \code{TRUE}, the return value is converted to a data frame
    for convenience in interactive use (default).}

  \item{...}{additional arguments passed on to the method implementations
    (currently, no further arguments are recognized)}

}

\value{

  If \code{obj} is a frequency spectrum, type-frequency list or token vector:
  A numeric vector of the same length as \code{measures} with the corresponding observed values of the productivity measures.
  If \code{data.frame=TRUE} (the default), a single-row data frame is returned.

  If \code{obj} is a vocabulary growth curve:
  A numeric matrix with columns corresponding to the selected productivity measures and rows corresponding to the sample sizes of the vocabulary growth curve.
  If \code{data.frame=TRUE} (the default), the matrix is converted to a data frame.

}

\details{

  This function computes productivity measures based on an observed frequency spectrum, type-frequency list or vocabulary growth curve.
  If an \emph{expected} spectrum or VGC is passed, the expectations \eqn{E[V]}, \eqn{E[V_m]} will simply be substituted for the sample values \eqn{V}, \eqn{V_m} in the equations. In most cases, this does \emph{not} yield the expected value of the productivity measure!
  
  Some measures can only be computed from a complete frequency spectrum.  They will return \code{NA} if \code{obj} is an incomplete spectrum or type-frequency list, an expected spectrum or a vocabulary growth curve is passed.
  
  Some other measures can only be computed is a sufficient number of spectrum elements is included in a vocabulary growth curve (usually at least
  \eqn{V_1} and \eqn{V_2}), and will return \code{NA} otherwise.

  Such limitations are indicated in the list of measures below (unless spectrum elements \eqn{V_1} and \eqn{V_2} are sufficient).
  
}

\section{Productivity Measures}{

  The following productivity measures are currently supported:

  \describe{
    \item{\code{V}:}{
      the total number of types \eqn{V} 
    }
    \item{\code{TTR}:}{
      the type-token ratio TTR = \eqn{V / N}
    }
    \item{\code{R}:}{ 
      Guiraud's (1954) \eqn{R = V / \sqrt{N}}. An equivalent measure is Carroll's (1964) \eqn{CTTR = R / \sqrt{2}}.
    }
    \item{\code{C}:}{
      Herdan's (1964) \eqn{C = \frac{ \log V }{ \log N }}{C = log(V) / log(N)} 
    }
    \item{\code{k}:}{
      Dugast's (1979) \eqn{k = \frac{ \log V }{ \log \log N}}{k = log(V) / log(log(N))}
    }
    \item{\code{U}:}{
      Dugast's (1978, 1979) \eqn{U = \frac{ (\log N)^2 }{ \log N - \log V}}{U = log(N)^2 / (log(N) - log(V))}.
      Maas (1972) proposed an equivalent measure \eqn{a^2 = 1 / U}.
    }
    \item{\code{W}:}{
      Brunet's (1978) \eqn{W = N ^ {V ^ {-a}}}{W = N ^ (V ^ -a)} with \eqn{a = 0.172}. 
    }
    \item{\code{P}:}{
      Baayen's (1991) productivity index \eqn{P = \frac{V_1}{N}}{P = V1 / N}, which corresponds to the slope of the vocabulary growth curve (under random sampling assumptions) 
    }
    \item{\code{Hapax}:}{
      the proportion of \emph{hapax legomena} \eqn{\frac{V_1}{V}}{V1 / V} is a direct estimate for the parameter \eqn{\alpha = 1 / a} of a population following the Zipf-Mandelbrot law (Evert 2004b: 130).
    }
    \item{\code{H}:}{
      Honoré's (1979) \eqn{H = 100 \frac{ \log N }{ 1 - V_1 / V }}{H = 100 * log(N) / (1 - V1 / V)}, a transformation of the proportion of \emph{hapax legomena} adjusted for sample size
    }
    \item{\code{S}:}{
      Sichel's (1975) \eqn{S = V_2 / V}{S = V2 / V}, i.e. the proportion of \emph{dis legomena}. Michéa's (1969, 1971) \eqn{M = 1 / S} is an equivalent measure.
    }
    \item{\code{alpha2}:}{
      Evert's \eqn{\alpha_2 = 1 - 2 \frac{V_2}{V_1}}{alpha2 = 1 - 2 * V2 / V1} is another direct estimate for the parameter \eqn{\alpha = 1 / a} of a Zipf-Mandelbrot population (Evert 2004b: 127).
    }
    \item{\code{K}:}{
      Yule's (1944) \eqn{K = 10^4 \cdot \frac{ \sum_m m^2 V_m - N}{ N^2 }}{K = 10000 * (SUM(m) m^2 Vm - N) / N^2} \cr
      (only for complete frequency spectrum or type-frequency list). Herdan (1955) proposes an almost equivalent measure \eqn{v_m \approx \sqrt{K}}{v_m ~ sqrt(K)} based on a different derviation. Both measures converge for large \eqn{N} and \eqn{V}.
      Yule's \eqn{K} is almost identical to Simpson's \eqn{D} and is an unbiased estimator for the same population coefficient \eqn{\delta} under an independent Poisson sampling scheme.
      A measure of \emph{lexical poverty}, i.e. smaller values correpond to higher productivity.
    }
    \item{\code{D}:}{
      Simpson's (1949) \eqn{D = \sum_m V_m \frac{m}{N}\cdot \frac{m-1}{N-1}}{D = SUM(m) Vm * (m / N) * ((m - 1) / (N - 1))} \cr
      (only for complete frequency spectrum or type-frequency list) is a slightly modified version of Yule's \eqn{K}.
      This measure is an unbiased estimator for a population coefficient \eqn{\delta}, representing the probability of picking the same type twice in two consecutive draws from the population. 
      A measure of \emph{lexical poverty}, i.e. smaller values correpond to higher productivity.
    }
    \item{\code{Entropy}:}{
      Entropy of the sample frequency distribution \eqn{-\sum_m V_m \frac{m}{N} \log_2 \frac{m}{N}}{- SUM(m) Vm * (m / N) * log2(m / N)} \cr
      (only for complete frequency spectrum or type-frequency list). This is not a reliable estimator of population entropy. It is therefore not recommended as a productivity measure and has only been included for evaluation studies.
      A measure of \emph{lexical poverty}, i.e. smaller values correpond to higher productivity.
    }
    \item{\code{eta}:}{
      Normalised entropy or \emph{evenness} \eqn{\eta = \textrm{Entropy} / \log_2 V}{eta = Entropy / log2(V)}\cr
      (only for complete frequency spectrum or type-frequency list) where \eqn{\log_2 V}{log2(V)} is the largest possible value for a sample with the observed vocabulary size (obtained for a uniform distribution).  Therefore, \eqn{0 \le \eta \le 1}.
      Not recommended as a productivity measure because it is expected to produce erratic and counterintuitive results.
    }
%%    \item{\code{}:}{}
  }

  See Sec. 2.1 of the technical report \href{https://zipfr.r-forge.r-project.org/materials/inside-zipfr.pdf}{Inside \emph{zipfR}} for further details and references.

}

\references{

  Evert, Stefan (2004b). \emph{The Statistics of Word Cooccurrences: Word
  Pairs and Collocations.} PhD Thesis, IMS, University of Stuttgart.
  URN urn:nbn:de:bsz:93-opus-23714
  \url{http://elib.uni-stuttgart.de/opus/volltexte/2005/2371/}

}


\seealso{

  \code{\link{lnre.productivity.measures}} for parametric bootstrapping and approximate expectations 
  of productivity measures in random samples from a LNRE population.

}

\keyword{ methods }
\keyword{ univar }

\examples{
rbind(
  AllTexts=productivity.measures(Brown.spc),
  Fiction=productivity.measures(BrownImag.spc),
  NonFiction=productivity.measures(BrownInform.spc))

## can be applied to token vector, type-frequency list, or frequency spectrum
bar.vec <- EvertLuedeling2001$bar
bar1 <- productivity.measures(bar.vec)          # token vector
bar2 <- productivity.measures(vec2tfl(bar.vec)) # type-frequency list
bar3 <- productivity.measures(vec2spc(bar.vec)) # frequency spectrum
print(rbind(tokens=bar1, tfl=bar2, spc=bar3))
\dontshow{
stopifnot(all.equal(bar1, bar2)) # hidden unit test
stopifnot(all.equal(bar1, bar3))
}
## sample-size dependency of productivity measures in Brown corpus
## (note that only a subset of the measures can be computed)
n <- c(10e3, 50e3, 100e3, 200e3, 500e3, 1e6)
idx <- N(Brown.emp.vgc) \%in\% n
my.vgc <- vgc(N=N(Brown.emp.vgc)[idx],
              V=V(Brown.emp.vgc)[idx],
              Vm=list(Vm(Brown.emp.vgc, 1)[idx]))
print(my.vgc) # since we don't have a subset method for VGCs yet
productivity.measures(my.vgc)

productivity.measures(my.vgc, measures=c("TTR", "P")) # selected measures

## parametric bootstrapping to obtain sampling distribution of measures
## (much easier with ?lnre.productivity.measures)
model <- lnre("zm", spc=ItaRi.spc) # realistic LNRE model
res <- lnre.bootstrap(model, 1e6, ESTIMATOR=identity,
                      STATISTIC=productivity.measures)
bootstrap.confint(res, method="normal")
}
