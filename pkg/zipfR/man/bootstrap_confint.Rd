\name{bootstrap.confint}
\alias{bootstrap.confint}
\title{Estimate Confidence Intervals from Parametric Bootstrap Simulations (zipfR)}

\description{

  Estimate confidence intervals for empirical distributions obtained by parametric bootstrapping.
  The input data must contain a sufficient number of bootstrap replicates for the desired confidence level.

}

\usage{

bootstrap.confint(x, level=0.95, 
                  method=c("normal", "mad", "empirical"))

}

\arguments{

  \item{x}{a numeric matrix, with rows corresponding to bootstrap replicates and columns corresponding to different statistics or coefficients. The matrix should have column labels, which will be preserved in the result.}

  \item{level}{desired confidence level (two-sided)}

  \item{method}{type of confidence interval to be estimated (see "Details" below)}

}

\details{

  This function can compute three different types of confidence intervals, selected by the \code{method} parameter.  In addition, corresponding estimates of central tendency (\code{center}) and spread (\code{spread}) of the distribution are returned.

  \describe{

    \item{\code{normal}:}{
      Wald-type confidence interval based on normal approximation of the bootstrapped distribution (default).
      Central tendency is given by the sample mean, spread by standard deviation.
      
      This method is unreliable if the bootstrapping produces outlier results and can report confidence limits
      outside the feasible range of a parameter or coefficient (e.g. a negative population diversity \eqn{S}).
      For this reason, it is strongly recommended to use a more robust type of confidence interval.
    }

    \item{\code{mad}:}{
      Robust asymmetric confidence intervals around the median, using separate estimates for left and right median absolute deviation (MAD) as robust approximations of standard deviation.
      Central tendency is given by the median, and spread by the average of left and right standard deviation (estimated via MAD).
      
      This method is applicable in most situations and requires fewer bootstrap replicates than empirical confidence intervals. Note that the values are different from those returned by \code{\link{mad}} because of the separate left and right estimators.
    }

    \item{\code{empirical}:}{
      The empirical inter-quantile range, e.g. 2.5\% to 97.5\% for default \code{conf.level=.95}.
      Note that the actual range might be slightly different depending on the number of bootstrap replicates available.
      Central tendency is given by the median, and spread by the inter-quartile range (IQR) re-scaled to be comparable to standard deviation (cf. \code{\link{IQR}}).
      
      This is the only method guaranteed to stay within feasible range, but requires a large number of bootstrap replicates
      for reliable confidence intervals (e.g. at least 120 replicates for the default 95\% confidence level).
    }
    
  }

}

\value{

  A numeric matrix with the same number of columns and column labels as \code{x}, and four rows:
  
  \enumerate{
    \item the lower boundary of the confidence interval (labelled with the corresponding quantile, e.g. \code{2.5\%})
    \item the upper boundary of the confidence interval (labelled with the corresponding quantile, e.g. \code{97.5\%})
    \item an estimate of central tendency (labelled \code{center})
    \item an estimate of spread on a scale comparable to standard deviaton (labelled \code{spread})
  }

}

\seealso{

  \code{bootstrap.confint} is usually applied to the output of \code{\link{lnre.bootstrap}} with \code{simplify=TRUE}.
  In particual, \code{\link{confint.lnre}} uses this function to obtain bootstrapped confidence intervals for LNRE model parameters and other coefficients.

}

\examples{
M <- cbind(alpha=rnorm(200, 10, 5),      # alpha has Gaussian distribution around mean = 10
           beta=rlnorm(200, log(10), 1)) # beta has log-normal distribution around median = 10

summary(M) # overview of the distribution

bootstrap.confint(M, method="normal") # normal approximation
bootstrap.confint(M, method="mad")    # robust asymmetric MAD estimates
bootstrap.confint(M, method="empirical")
}

\keyword{ univar }
