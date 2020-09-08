\name{lnre.productivity.measures}
\alias{lnre.productivity.measures}
\title{Measures of Productivity and Lexical Richness (zipfR)}
\encoding{UTF-8}

\description{

  Compute expectations of various measures of productivity and lexical richness
  for a LNRE population.

}

\usage{
lnre.productivity.measures(model, N=NULL, measures, data.frame=TRUE, 
                           bootstrap=FALSE, method="normal", conf.level=.95,
                           replicates=1000, parallel=1L, verbose=TRUE, seed=NULL)

}

\arguments{

  \item{model}{an object belonging to a subclass of \code{lnre},
    representing a LNRE model}

  \item{measures}{character vector naming the productivity measures to
    be computed (see \code{\link{productivity.measures}} for details).
    Names may be abbreviated as long as they remain unique.
    If unspecified or \code{NULL}, all supported measures are included.}

  \item{N}{an integer vector, specifying the sample size(s) \eqn{N}
    for which the productivity measures will be calculated.  If
    \code{bootstrap=TRUE}, only a single sample size may be specified.
    \code{N} defaults to the sample size used for estimating \code{model}
    if unspecified or set to \code{NULL}.}

  \item{data.frame}{if \code{TRUE}, the return value is converted to a data frame
    for convenience in interactive use (default).}

  \item{bootstrap}{if \code{TRUE}, use parametric bootstrapping to estimate 
    expectations and confidence intervals for the productivity measures.
    Otherwise, approximate expectations are obtained directly from the LNRE 
    model (see \sQuote{Details} below for the approximations and simplifications used).}

  \item{method, conf.level}{type of confidence interval to be estimated by parametric
    bootstrapping and the requested confidence level; 
    see \code{\link{bootstrap.confint}} for details.}

  \item{replicates, parallel, seed, verbose}{if \code{bootstrap=TRUE}, these parameters
    are passed on to \code{lnre.bootstrap} to control the bootstrapping procedure; 
    see \code{\link{lnre.bootstrap}} for documentation.  In most cases, it is recommended
    to set \code{parallel} in order to speed up the expensive bootstrapping process.}

}

\value{

  If \code{bootstrap=FALSE}, a numeric matrix or data frame listing approximate expectations of the selected productivity measures,
  with one row for each sample size \code{N} and one column for each \code{measure}.  Rows and columns are labelled.
  
  If \code{bootstrap=TRUE}, a numeric matrix or data frame with one column for each productivity \code{measure} and four rows
  giving the lower and upper bound of the confidence interval, an estimate of central tendency, and an estimate of spread.
  See \code{\link{bootstrap.confint}} for details.

}

\details{

  If  \code{bootstrap=FALSE}, expected values of the productivity measures are computed based on the following approximations:
  
  \itemize{
    \item \code{V}, \code{TTR}, \code{R} and \code{P} are linear transformations of \eqn{V} or \eqn{V_1}{V1}, so expectations can be obtained directly from the \code{\link{EV}} and \code{\link{EVm}} methods.
  
    \item \code{C}, \code{k}, \code{U} and \code{W} are nonlinear transformations of \eqn{V}. In this case, the transformation function is approximated by a linear function around \eqn{E[V]}, which is reasonable under typical circumstances.
    
    \item \code{Hapax}, \code{S}, \code{alpha2} and \code{H} are based on ratios of two spectrum elements, in some cases with an additional nonlinear transformation. Expectations are based on normal approximations for \eqn{V} and \eqn{V_i} together with a generalisation of Díaz-Francés and Rubio's (2013: 313) result on the ratio of two independent normal distributions; for a nonlinear transformation the same linear approximation is made as above.
    
    \item \code{K} and \code{D} are (nearly) unbiased estimators of the population coefficient \eqn{\delta = \sum_{i=1}^{\infty} \pi_i^2}{delta = SUM(i) (pi_i)^2} (Simpson 1949: 688). 
  }
  
  Approximations used for expected values are explained in detail in Sec. 2.2 of the technical report \href{https://zipfr.r-forge.r-project.org/materials/inside-zipfr.pdf}{Inside \emph{zipfR}}.
  
}

\section{Productivity Measures}{

  See \code{\link{productivity.measures}} for a list of supported measures with equations and references.
  The measures \code{Entropy} and \code{eta} are only supported for \code{bootstrap=TRUE}.

}

\references{

Díaz-Francés, Eloísa and Rubio, Francisco J. (2013).
On the existence of a normal approximation to the distribution of the ratio of two independent normal random variables.
\emph{Statistical Papers}, \bold{54}(2), 309--323.

Simpson, E. H. (1949).
Measurement of diversity.
\emph{Nature}, \bold{163}, 688.

}


\seealso{

   \code{\link{productivity.measures}} computes productivity measures from observed data sets.
   See \code{\link{lnre}} for further information on LNRE models, and
   \code{\link{lnre.bootstrap}} and \code{\link{bootstrap.confint}} for details on the bootstrapping procedure.
  
}


\keyword{ methods }
\keyword{ univar }

\examples{
## plausible model for an author's vocabulary
model <- lnre("fzm", alpha=0.4, B=0.06, A=1e-12)

## approximate expectation for different sample sizes
lnre.productivity.measures(model, N=c(1000, 10000, 50000))

## estimate sampling distribution: 95% interval, mean, s.d.
## (using parametric bootstrapping, only one sample size at a time)
lnre.productivity.measures(model, N=1000, bootstrap=TRUE)

}

