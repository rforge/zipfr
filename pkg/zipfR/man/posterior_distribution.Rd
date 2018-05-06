\name{LNRE_posterior}
\alias{LNRE_posterior}
\alias{postdlnre}
\alias{postldlnre}
\alias{postplnre}
\alias{postqlnre}
\title{Posterior Distribution of LNRE Model (zipfR)}

\description{

  Posterior distribution over the type probability space of a LNRE
  model, given the observed frequency \eqn{m} in a sample. Posterior
  density (\code{postdlnre}) and log-transformed density
  (\code{postldlnre}) can be computed for all LNRE models. The
  distribution function (\code{postplnre}) and quantiles
  (\code{postqlnre}) are only available for selected types of models.

}

\usage{

postdlnre(model, x, m, N, \dots)
postldlnre(model, x, m, N, base=10, log.x=FALSE, \dots)
postplnre(model, q, m, N, lower.tail=FALSE, \dots)
postqlnre(model, p, m, N, lower.tail=FALSE, \dots)

}

\arguments{

  \item{model}{an object belonging to a subclass of \code{lnre},
    representing an LNRE model}

  \item{m}{frequency \eqn{m} of a type in the observed sample}

  \item{N}{sample size \eqn{N}}
  
  \item{x}{vector of type probabilities \eqn{pi} for which the posterior
    density function is evaluated}

  \item{q}{vector of type probability quantiles, i.e. threshold values
    \eqn{\rho} on the type probability axis}

  \item{p}{vector of tail probabilities}

  \item{base}{positive number, the base \eqn{a} with respect to which the
    log-transformation is peformed (see "Details" below)}

  \item{log.x}{if \code{TRUE}, the values passed in the argument
    \code{x} are assumed to be logarithmic, i.e. \eqn{\log_a \pi}}

  \item{lower.tail}{if \code{TRUE}, lower tail probabilities or type
    counts are returned / expected in the \code{p} argument.  Note that
    the defaults differ for distribution function and type distribution,
    and see "Details" below.}

  \item{\dots}{further arguments are passed through to the method
  implementations (currently unused)}
  
}

\value{

  A vector of non-negative numbers of the same length as the second
  argument (\code{x}, \code{p} or \code{q}).

  \code{postdlnre} returns the posterior type density \eqn{P(\pi | f = m)}
  for the values of \eqn{\pi} specified in the vector \code{x}.
  \code{postplnre} computes the posterior type distribution function
  \eqn{P(\pi \geq \rho | f = m)} (default) or its complement
  \eqn{P(\pi \leq \rho | f = m)}  (if \code{lower.tail=TRUE}).
  These correspond to \eqn{E[V_{m, >\rho}]} and \eqn{E[V_{m, \rho}]}, respectively (Evert 2004, p. 123).
  \code{postqlnre} returns quantiles, i.e. the inverse of the posterior
  type distribution function.
  
  \code{postldlnre} computes a logarithmically transformed version of
  the posterior type density, taking logarithms with respect to the
  base \eqn{a} specified in the \code{base} argument (default: \eqn{a=10}).
  Such log-transformed densities are useful for visualizing distributions,
  see \code{\link{ldlnre}} for more information.
    
}

\seealso{

  \code{\link{lnre}} for more information about LNRE models and how to
  initialize them, \code{\link{LNRE}} for type density and distribution
  functions (which represent the prior distribution).

}

\keyword{ distribution }
\keyword{ models }

\examples{

## TODO
  
}

