\name{confint.lnre}
\alias{confint.lnre}
\title{Confidence Intervals for LNRE Model Parameters (zipfR)}

\description{

  Compute bootstrapped confidence intervals for LNRE model parameters.
  The supplied model must contain a sufficient number of bootstrapping replicates.

}

\usage{

\method{confint}{lnre}(object, parm, level=0.95,
        method=c("normal", "mad", "empirical"), plot=FALSE, breaks="Sturges", \dots)

}

\arguments{

  \item{object}{an LNRE model (i.e. an object belonging to a subclass of \code{lnre}) with bootstrapping data}

  \item{parm}{model parameter(s) for which confidence intervals are desired.
    If unspecified, all parameters as well as population diversity \eqn{S} and goodness-of-fit statistic \eqn{X^2} are shown.}

  \item{level}{desired confidence level}

  \item{method}{type of confidence interval to be estimated (see "Details" below)}

  \item{plot}{if \code{TRUE}, plot bootstrapping histogram of the respective model parameter with density estimate and confidence interval}

  \item{breaks}{breakpoints for histogram shown with \code{plot=TRUE} (see \code{\link{hist}} for details)}
  
  \item{\dots}{all other arguments are ignored}

}

\details{

  This function computes three types of confidence intervals:

  \describe{

    \item{\code{normal}:}{
      Wald-type confidence interval based on normal approximation of the bootstrapped distribution (default).
      This method is unreliable if the bootstrapping produces outlier results and can report confidence limits
      outside the feasible parameter range (e.g. negative population diversity \eqn{S}).
      For this reason, it is strongly recommended to use a more robust type of confidence interval.
    }

    \item{\code{mad}:}{
      Robust asymmetric confidence intervals around the median based on a robust estimator of median absolute deviation (MAD).
      This method is applicable in most situations and requires fewer bootstrap replicates than empirical confidence intervals.
    }

    \item{\code{empirical}:}{
      The empirical inter-quantile range. This is the only method guaranteed to stay within the feasible parameter range,
      but requires a large number of bootstrap replicates for reliable confidence intervals (e.g. at least 120 replicates
      at 95\% confidence).
    }
    
  }

}

\seealso{

  \code{\link{lnre}} for estimating LNRE models with bootstrap replicates,
  \code{\link{lnre.bootstrap}} for the underlying parameteric bootstrapping code.

}

\examples{

model <- lnre("fzm", spc=BrownAdj.spc, bootstrap=20)
confint(model, "alpha", method="mad") # Zipf slope
confint(model, "S", method="mad")     # population diversity
confint(model, "S") # default normal approximation works well in this case

}

\keyword{ htest }
