\name{lnre.goodness.of.fit}
\alias{lnre.goodness.of.fit}
\title{Goodness-of-fit Evaluation of LNRE Models (zipfR)}

\description{

  This function measures the goodness-of-fit of a LNRE model compared to
  an observed frequency spectrum, using a multivariate chi-squared test
  (Baayen 2001, p. 119ff).

}

\usage{

  lnre.goodness.of.fit(model, spc, n.estimated=0, m.max=15)

}

\arguments{

  \item{model}{an LNRE model object, belonging to a suitable subclass of
    \code{lnre}.}

  \item{spc}{an observed frequency spectrum, i.e. an object of class
    \code{spc}.  This can either be the spectrum on which the model
    parameters have been estimated, or a different, independent spectrum.}

  \item{n.estimated}{number of parameters of the LNRE model that have
    been estimated on \code{spc}.  This number is automatically
    subtracted from the degrees of freedom of the resulting chi-squared
    statistic.  When \code{spc} is an independent spectrum,
    \code{n.estimated} should always be set to the default value of 0.}

  \item{m.max}{number of spectrum elements that will be used to compute
    the chi-squared statistic.  The default value of 15 is also used by
    Baayen (2001).  For small samples, it may be sensible to
    use fewer spectrum elements, e.g. by setting \code{m.max=10} or
    \code{m.max=5}.  Depending on how many degrees of freedom have to be
    subtracted, \code{m.max} should not be chosen too low.}
    
}

\value{
  
  A data frame with one row and the following variables:

  \item{X2}{value of the multivariate chi-squared statistic \eqn{X^2}}

  \item{df}{number of degrees of freedom of \eqn{X^2}, corrected for the
    number of parameters that have been estimated on \code{spc}}

  \item{p}{p-value corresponding to \eqn{X^2}}
  
}

\details{

  By default, the number of spectrum elements included in the
  calculation of the chi-squared statistic may be reduced automatically
  in order to ensure that it is not dominated by the sampling error of
  spectrum elements with very small expected frequencies (which are
  scaled up due to the small variance of these random variables).  As an
  ad-hoc rule of thumb, spectrum elements \eqn{V_m} with variance less
  than 5 are excluded, since the normal approximation to their discrete
  distribution is likely to be inaccurate in this case.

  Automatic reduction is disabled when the parameter \code{m.max} is
  specified explicitly (use \code{m.max=15} to disable automatic
  reduction without changing the default value).
  
}

\references{

Baayen, R. Harald (2001). \emph{Word Frequency Distributions.} Kluwer,
  Dordrecht.

}

\seealso{

  \code{\link{lnre}} for more information about LNRE models
  
}

\keyword{ optimize }
\keyword{ models }
  
\examples{

## load spectrum of first 100k Brown tokens
data(Brown100k.spc)

## use this spectrum to compute zm and gigp
## models
zm <- lnre("zm",Brown100k.spc)
gigp <- lnre("gigp",Brown100k.spc)

## lnre.goodness.of.fit with appropriate
## n.estimated value produces the same multivariate
## chi-squared test that is reported in a model
## summary

## compare:
zm
lnre.goodness.of.fit(zm,Brown100k.spc,n.estimated=2)

gigp
lnre.goodness.of.fit(gigp,Brown100k.spc,n.estimated=3)

## goodness of fit of the 100k models calculated on the
## whole Brown spectrum (although this is superset of
## 100k spectrum, let's pretend it is an independent
## spectrum, and set n.estimated to 0)

data(Brown.spc)

lnre.goodness.of.fit(zm,Brown.spc,n.estimated=0)
lnre.goodness.of.fit(gigp,Brown.spc,n.estimated=0)


}
