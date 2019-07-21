\name{confint.lnre}
\alias{confint.lnre}
\title{Confidence Intervals for LNRE Model Parameters (zipfR)}

\description{

  Compute bootstrapped confidence intervals for LNRE model parameters.
  The supplied model must contain a sufficient number of bootstrapping replicates.

}

\usage{
\method{confint}{lnre}(object, parm, level=0.95, method=c("mad", "normal", "empirical"), 
        plot=FALSE, breaks="Sturges", \dots)
}

\arguments{

  \item{object}{an LNRE model (i.e. an object belonging to a subclass of \code{lnre}) with bootstrapping data}

  \item{parm}{model parameter(s) for which confidence intervals are desired.
    If unspecified, all parameters as well as population diversity \eqn{S} and goodness-of-fit statistic \eqn{X^2} are shown.}

  \item{level}{desired confidence level (two-sided)}

  \item{method}{type of confidence interval to be estimated (see \code{\link{bootstrap.confint}} for details). Note that this parameter defaults to the asymmetric and more robust \code{mad} method here.}

  \item{plot}{if \code{TRUE}, plot bootstrapping histogram of the respective model parameter with density estimate and confidence interval}

  \item{breaks}{breakpoints for histogram shown with \code{plot=TRUE} (see \code{\link{hist}} for details)}
  
  \item{\dots}{all other arguments are ignored}

}

\value{

  A data frame with one numeric column for each selected model parameter (labelled with the parameter name) and four rows:
    \enumerate{
    \item the lower boundary of the confidence interval (labelled with the corresponding quantile, e.g. \code{2.5\%})
    \item the upper boundary of the confidence interval (labelled with the corresponding quantile, e.g. \code{97.5\%})
    \item an estimate of central tendency (labelled \code{center})
    \item an estimate of spread on a scale comparable to standard deviaton (labelled \code{spread})
  }

}

\seealso{

  \code{\link{lnre}} for estimating LNRE models with bootstrap replicates,
  \code{\link{lnre.bootstrap}} for the underlying parameteric bootstrapping code, and
  \code{\link{bootstrap.confint}} for the different methods of estimating confidence intervals.

}

\examples{
\donttest{
model <- lnre("fzm", spc=BrownAdj.spc, bootstrap=20)
confint(model, "alpha") # Zipf slope
confint(model, "S")     # population diversity
confint(model, "S", method="normal") # Gaussian approx works well in this case

confint(model) # overview
confint(model, "alpha", plot=TRUE) # visualize bootstrap distribution
}
}

\keyword{ htest }
