\name{LNRE}
\alias{LNRE}
\alias{rlnre}
\alias{dlnre}
\alias{plnre}
\alias{qlnre}
\alias{tdlnre}
\alias{tplnre}
\alias{tqlnre}
\alias{ldlnre}
\alias{ltdlnre}
\title{Type and Probability Distributions of LNRE Models (zipfR)}

\description{

  Type density \eqn{g} (\code{tdlnre}), type distribution \eqn{G}
  (\code{tplnre}), type quantiles \eqn{G^{-1}} (\code{tqlnre}),
  probability density \eqn{f} (\code{dlnre}), distribution function
  \eqn{F} (\code{plnre}), quantile function \eqn{F^{-1}} (\code{qlnre}),
  logarithmic type and probability densities (\code{ltdlnre} and
  \code{ldlnre}), and random sample generation (\code{rlnre}) for LNRE
  models.
  
}

\usage{

  tdlnre(model, x, \dots)
  tplnre(model, q, lower.tail=FALSE, \dots)
  tqlnre(model, p, lower.tail=FALSE, \dots)

  dlnre(model, x, \dots)
  plnre(model, q, lower.tail=TRUE, \dots)
  qlnre(model, p, lower.tail=TRUE, \dots)

  ltdlnre(model, x, base=10, log.x=FALSE, \dots)
  ldlnre(model, x, base=10, log.x=FALSE, \dots)

  rlnre(model, n, \dots)

}

\arguments{

  \item{model}{an object belonging to a subclass of \code{lnre},
    representing a LNRE model}

  \item{x}{vector of type probabilities \eqn{pi} for which the density
    function is evaluated}

  \item{q}{vector of type probability quantiles, i.e. threshold values
    \eqn{\rho} on the type probability axis}

  \item{p}{vector of tail probabilities}

  \item{lower.tail}{if \code{TRUE}, lower tail probabilities or type
    counts are returned / expected in the \code{p} argument.  Note that
    the defaults differ for distribution function and type distribution,
    and see "Details" below.}

  \item{base}{positive number, the base with respect to which the
    log-transformation is peformed (see "Details" below)}

  \item{log.x}{if \code{TRUE}, the values passed in the argument
    \code{x} are assumed to be logarithmic, i.e. \eqn{\log_a \pi}}

  \item{n}{size of random sample to generate.  If \code{length(n) > 1},
    the length is taken to be the number required.}

  \item{\dots}{further arguments are passed through to the method
  implementations (currently unused)}
  
}

\value{

  For \code{rnlre}, a factor of length \code{n}, representing a random
  sample from the population described by the specified LNRE model.

  For all other functions, a vector of non-negative numbers of the same
  length as the second argument (\code{x}, \code{p} or \code{q}).

  \code{tdlnre} returns the type density \eqn{g(\pi)} for the values of
  \eqn{\pi} specified in the vector \code{x}.  \code{tplnre} returns the
  type distribution \eqn{G(\rho)} (default) or its complement
  \eqn{1-G(\rho)} (if \code{lower.tail=TRUE}), for the values of
  \eqn{\rho} specified in the vector \code{q}.  \code{tqlnre} returns
  type quantiles, i.e. the inverse \eqn{G^{-1}(x)} (default) or
  \eqn{G^{-1}(S-x)} (if \code{lower.tail=TRUE}) of the type
  distribution, for the type counts \eqn{x} specified in the vector
  \code{p}.

  \code{dlnre} returns the probability density \eqn{f(\pi)} for the
  values of \eqn{\pi} specified in the vector \code{x}.  \code{plnre}
  returns the distribution function \eqn{F(\rho)} (default) or its
  complement \eqn{1-F(\rho)} (if \code{lower.tail=FALSE}), for the
  values of \eqn{\rho} specified in the vector \code{q}.  \code{qlnre}
  returns quantiles, i.e. the inverse \eqn{F^{-1}(p)} (default) or
  \eqn{F^{-1}(1-p)} (if \code{lower.tail=FALSE}) of the distribution
  function, for the probabilities \eqn{p} specified in the vector
  \code{p}.

  \code{ldlnre} and \code{ltdlnre} compute logarithmically transformed
  versions of the probability and type density functions, respectively,
  taking logarithms with respect to the base \eqn{a} specified in the
  \code{base} argument (default: \eqn{a=10}).  See "Details" above for
  more information.
    
}

\details{
  
  Note that the order in which arguments are specified differs from the
  analogous functions for common statistical distributions in the \R
  standard library.  In particular, the LNRE model \code{model} always
  has to be given as the first parameter so that \R can dispatch the
  function call to an appropriate method implementation for the chosen
  LNRE model.

  Some of the functions may not be available for certain types of LNRE
  models.  In particular, no analytical solutions are known for the
  distribution and quantiles of GIGP models, so the functions
  \code{tplnre}, \code{tqlnre}, \code{plnre}, \code{qlnre} and
  \code{rlnre} (which depends on \code{qlnre} and \code{tplnre}) are not
  implemented for objects of class \code{lnre.gigp}. 
  
  The default tails differ for the distribution function (\code{plnre},
  \code{qlnre}) and the type distribution (\code{tplnre},
  \code{tqlnre}), in order to match the definitions of \eqn{F(\rho)} and
  \eqn{G(\rho)}.  While the distribution function defaults to lower
  tails (\code{lower.tail=TRUE}, corresponding to \eqn{F} and
  \eqn{F^{-1}}), the type distribution defaults to upper tails
  (\code{lower.tail=FALSE}, corresponding to \eqn{G} and \eqn{G^{-1}}).

  Unlike for standard distriutions, logarithmic tail probabilities
  (\code{log.p=TRUE}) are not provided for the LNRE models, since here
  the focus is usually on the bulk of the distribution rather than on
  the extreme tails.
  
  The log-transformed density functions \eqn{f^*}{f*} and \eqn{g^*}{g*}
  returned by \code{ldlnre} and \code{ltdlnre}, respectively, can be
  understood as probability and type densities for \eqn{\log_a \pi}
  instead of \eqn{\pi}, and are useful for visualization of LNRE
  populations (with a logarithmic scale for the parameter \eqn{\pi} on
  the x-axis).  For example,

  \deqn{
    G(\log_a \rho) = \int_{\log_a \rho}^{0} g^*(t) \,dt
  }{
    G(log_a rho) = integral_{log_a rho}^{0} g*(t) dt 
  }
  
}

\seealso{

  \code{\link{lnre}} for more information about LNRE models and how to
  initialize them

  random samples generated with \code{rnlre} can be further processed
  with the functions \code{\link{vec2tfl}}, \code{\link{vec2spc}} and
  \code{\link{vec2vgc}}
  
}

\keyword{ distribution }
\keyword{ models }

\examples{

## define ZM and fZM LNRE models 
ZM <- lnre("zm", alpha=.8, B=1e-3)
FZM <- lnre("fzm", alpha=.8, A=1e-5, B=.05)

## random samples from the two models
head(table(rlnre(ZM, 10000)))
head(table(rlnre(FZM, 10000)))

## plot logarithmic type density functions
x <- 10^seq(-6, 1, by=.01)  # pi = 10^(-6) .. 10^(-1)
y.zm <- ltdlnre(ZM, x)
y.fzm <- ltdlnre(FZM, x)
\dontrun{plot(x, y.zm, type="l", lwd=2, col="red", log="x", ylim=c(0,14000))}
\dontrun{lines(x, y.fzm, lwd=2, col="blue")}
\dontrun{legend("topright", legend=c("ZM", "fZM"), lwd=3, col=c("red", "blue"))}

## probability pi_k of k-th type according to FZM model
k <- 10
plnre(FZM, tqlnre(FZM, k-1)) - plnre(FZM, tqlnre(FZM, k))

## number of types with pi >= 1e-6
tplnre(ZM, 1e-6)

## lower tail fails for infinite population size
\dontrun{tplnre(ZM, 1e-3, lower=TRUE)}

## total probability mass assigned to types with pi <= 1e-6
plnre(ZM, 1e-6)
  
}

