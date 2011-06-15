\name{lnre.bootstrap}
\alias{lnre.bootstrap}
\title{Parametric bootstrapping for LNRE models (zipfR)}

\description{

  This function implements parametric bootstrapping for LNRE models, i.e. it draws a specified number of random samples from a trained \code{lnre} object.  For each sample, a new model is estimated and user-defined information is extracted from this model.  Please \bold{read the warning} under "Details" below.
  
}

\usage{

  lnre.bootstrap(model, N, ESTIMATOR, STATISTIC, replicates=100, simplify=FALSE, verbose=TRUE, \dots)

}

\arguments{

  \item{model}{a trained LNRE model, i.e. an object belonging to a subclass of \code{lnre}.  The model must provide a \code{\link{rlnre}} method to generate random samples from the underlying frequency distribution.}

  \item{N}{a single positive integer, specifying the size \eqn{N} (i.e. token count) of the individual bootstrap samples}

  \item{ESTIMATOR}{
    a callback function for estimating LNRE models in the bootstrap procedure.  It is called for each bootstrap sample with the observed frequency spectrum as first argument and must return a trained LNRE model (i.e. an object belonging to a subcass of \code{lnre}).  Additional arguments (\code{\dots}) are passed through to the estimator callback, so it is possible to use \code{ESTIMATOR=lnre} with appropriate settings.
  }

  \item{STATISTIC}{
    a callback function to extract relevant statistics from the bootstrapped LNRE models.  It is called for each bootstrap sample with the estimated LNRE model as single argument.  The return values are automatically aggregated across all bootstrap samples (see "Value" below).
  }

  \item{replicates}{a single positive integer, specifying the number of bootstrap samples to be generated}

  \item{simplify}{if \code{TRUE}, attempt to combine bootstrapping results into vector, matrix or data frame (otherwise, returns list)}

  \item{verbose}{if \code{TRUE}, shows progress bar in R console during execution (which can take quite a long time)}

  \item{\dots}{any further arguments are passed through to the \code{ESTIMATOR} callback function}

}

\value{

  A list containing the statistics obtained from each individual bootstrap sample.
  If \code{simplify=FALSE}, the return value may be automatically simplified to a vector, matrix or data frame as appropriate.
  
  Currently, automatic simplification is performed by \code{\link{sapply}}.  In particular, if each call to \code{STATISTIC} returns a single number, the result is a vector; if it returns vectors of the same length, the result is a matrix.  \emph{This behaviour is subject to change in future releases.}
 
 \bold{TODO:} document attributes of return value; should they be included only if \code{simplify=FALSE}?
  
}

\details{

  \bold{Warning:} The current implementation generates random samples of \emph{tokens} and will be very inefficient for large values of \code{N}.  It is not recommended to use this function for sample sizes of much more than 1 million tokens at this time.
  
}

\seealso{

  \code{\link{lnre}} for more information about LNRE models.  The high-level estimator function \code{\link{lnre}} uses \code{lnre.bootstrap} to collect data for approximate confidence intervals.
  
}

\keyword{ models }
\keyword{ distribution }
  
\examples{

## TODO

}

