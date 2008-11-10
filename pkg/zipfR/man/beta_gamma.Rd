\name{beta_gamma}
\alias{Cgamma}
\alias{Rgamma}
\alias{Rgamma.inv}
\alias{Igamma}
\alias{Igamma.inv}
\alias{Cbeta}
\alias{Rbeta}
\alias{Rbeta.inv}
\alias{Ibeta}
\alias{Ibeta.inv}
\title{Incomplete Beta and Gamma Functions (zipfR)}

\description{

  The functions documented here compute incomplete and regularized
  Beta and Gamma functions as well as their logarithms and the
  corresponding inverse functions. These functions will be of interest
  to developers, not users of the toolkit.
  
}

\usage{

  Cgamma(a, log=!missing(base), base=exp(1))
  Igamma(a, x, lower=TRUE, log=!missing(base), base=exp(1))
  Igamma.inv(a, y, lower=TRUE, log=!missing(base), base=exp(1))
  Rgamma(a, x, lower=TRUE, log=!missing(base), base=exp(1))
  Rgamma.inv(a, y, lower=TRUE, log=!missing(base), base=exp(1))

  Cbeta(a, b, log=!missing(base), base=exp(1))
  Ibeta(x, a, b, lower=TRUE, log=!missing(base), base=exp(1))
  Ibeta.inv(y, a, b, lower=TRUE, log=!missing(base), base=exp(1))
  Rbeta(x, a, b, lower=TRUE, log=!missing(base), base=exp(1))
  Rbeta.inv(y, a, b, lower=TRUE, log=!missing(base), base=exp(1))

}

\arguments{

  \item{a, b}{non-negative numeric vectors, the parameters of the Gamma
    and Beta functions (\code{b} applies only to Beta functions)}

  \item{x}{a non-negative numeric vector, the point at which the
    incomplete or regularized Gamma or Beta function is evaluated (for
    the Beta functions, \code{x} must be in the range \eqn{[0,1]}}

  \item{y}{a non-negative numeric vector, the values of the Gamma or
    Beta function on linear or logarithmic scale}

  \item{lower}{whether to compute the lower (\code{TRUE}) or upper
    (\code{FALSE}) incomplete or regularized Gamma or Beta function}

  \item{log}{if \code{TRUE}, return values of the Gamma and Beta
    functions -- as well as the \code{y} argument of the inverse
    functions -- are on logarithmic scale}

  \item{base}{a positive number, specifying the base of the logarithmic
    scale for values of the Gamma and Beta functions (default: natural
    logarithm).  Setting the \code{base} parameter implies
    \code{log=TRUE}.}

}

\value{

  \code{Cgamma} returns the (complete) Gamma function evaluated at
  \code{a}, \eqn{\Gamma(a)}.  \code{Igamma} returns the (lower or upper)
  incomplete Gamma function with parameter \code{a} evaluated at point
  \code{x}, \eqn{\gamma(a,x)} (\code{lower=TRUE}) or \eqn{\Gamma(a,x)}
  (\code{lower=FALSE}).  \code{Rgamma} returns the corresponding
  regularized Gamma function, \eqn{P(a,x)} (\code{lower=TRUE}) or
  \eqn{Q(a,x)} (\code{lower=FALSE}).  If \code{log=TRUE}, the returned
  values are on logarithmic scale as specified by the \code{base}
  parameter.

  \code{Igamma.inv} and \code{Rgamma.inv} compute the inverse of the
  incomplete and regularized Gamma functions with respect to the
  parameter \code{x}.  I.e., \code{Igamma.inv(a,y)} returns the point
  \code{x} at which the (lower or upper) incomplete Gamma function with
  parameter \code{a} evaluates to \code{y}, and \emph{mutatis mutandis}
  for \code{Rgamma.inv(a,y)}.  If \code{log=TRUE}, the parameter
  \code{y} is taken to be on a logarithmic scale as specified by
  \code{base}.

  \code{Cbeta} returns the (complete) Beta function with arguments
  \code{a} and \code{b}, \eqn{B(a,b)}.  \code{Ibeta} returns the (lower
  or upper) incomplete Beta function with parameters \code{a} and
  \code{b}, evaluated at point \code{x}, \eqn{B(x;a,b)}
  (\code{lower=TRUE}) and \eqn{B^*(x;a,b)}{B*(x;a,b)}
  (\code{lower=FALSE}).  Note that in contrast to the Gamma functions,
  capital \eqn{B} refers to the \emph{lower} incomplete Beta function,
  and there is no standardized notation for the upper incomplete Beta
  function, so \eqn{B^*}{B*} is used here as an ad-hoc symbol.
  \code{Rbeta} returns the corresponding regularized Beta function,
  \eqn{I(x;a,b)} (\code{lower=FALSE}) or \eqn{I^*(x;a,b)}{I*(x;a,b)}
  (\code{lower=TRUE}).  If \code{log=TRUE}, the returned values are on
  logarithmic scale as specified by the \code{base} parameter.

  \code{Ibeta.inv} and \code{Rbeta.inv} compute the inverse of the
  incomplete and regularized Beta functions with respect to the
  parameter \code{x}.  I.e., \code{Ibeta.inv(y,a,b)} returns the point
  \code{x} at which the (lower or upper) incomplete Beta function with
  parameters \code{a} and \code{b} evaluates to \code{y}, and
  \emph{mutatis mutandis} for \code{Rbeta.inv(y,a,b)}.  If
  \code{log=TRUE}, the parameter \code{y} is taken to be on a
  logarithmic scale as specified by \code{base}.

  All Gamma and Beta functions can be vectorized in the arguments
  \code{x}, \code{y}, \code{a} and \code{b}, with the usual R value
  recycling rules in the case of multiple vectorizations.
  
}

\section{Mathematical Details}{

  The upper incomplete Gamma function is defined by the Gamma integral

  \deqn{\Gamma(a,x) = \int_x^{\infty} t^{a-1} e^{-t}\,dt}{%
    Gamma(a,x) = integral_x^Inf t^(a-1) exp(-t) dt
  }

  The lower incomplete Gamma function is defined by the complementary
  Gamma integral

  \deqn{\gamma(a,x) = \int_0^x t^{a-1} e^{-t}\,dt}{%
    gamma(a,x) = integral_0^x t^(a-1) exp(-t) dt
  }

  The complete Gamma function calculates the full Gamma integral,
  i.e. \eqn{\Gamma(a) = \gamma(a,0)}.  The regularized Gamma functions
  scale the corresponding incomplete Gamma functions to the interval
  \eqn{[0,1]}, by dividing through \eqn{\Gamma(a)}.  Thus, the lower
  regularized Gamma function is given by
  
  \deqn{P(a,x) = \frac{\gamma(a,x)}{\Gamma(a)}}{%
    P(a,x) = gamma(a,x) / Gamma(a) }

  and the upper regularized Gamma function is given by

  \deqn{Q(a,x) = \frac{\Gamma(a,x)}{\Gamma(a)}}{%
    Q(a,x) = Gamma(a,x) / Gamma(a) }

  The lower incomplete Beta function is defined by the Beta integral

  \deqn{B(x;a,b) = \int_0^x t^{a-1} (1-t)^{b-1}\,dt}{%
    B(x;a,b) = integral_0^x t^(a-1) (1-t)^(b-1) dt
  }

  and the upper incomplete Beta function is defined by the complementary
  integral

  \deqn{B^*(x;a,b) = \int_x^1 t^{a-1} (1-t)^{b-1}\,dt}{%
    B*(x;a,b) = integral_x^1 t^(a-1) (1-t)^(b-1) dt
  }

  The complete Beta function calculates the full Beta integral, i.e.
  \eqn{B(a,b) = B(1;a,b) = B^*(0;a,b)}{B(a,b) = B(1;a,b) = B*(0;a,b)}.
  The regularized Beta function scales the incomplete Beta function to
  the interval \eqn{[0,1]}, by dividing through \eqn{B(a,b)}.  The lower
  regularized Beta function is thus given by

  \deqn{I(x;a,b) = \frac{B(x;a,b)}{B(a,b)}}{%
    I(x;a,b) = B(x;a,b) / B(a,b)
  }

  and the upper regularized Beta function is given by 

  \deqn{I^*(x;a,b) = \frac{B^*(x;a,b)}{B(a,b)}}{%
    I*(x;a,b) = B*(x;a,b) / B(a,b)
  }

}

\seealso{

  \code{\link{gamma}} and \code{\link{lgamma}}, which are fully
  equivalent to \code{Cgamma}.  \code{\link{beta}} and
  \code{\link{lbeta}}, which are fully equivalent to \code{Cbeta}

  The implementations of the incomplete and regularized Gamma functions
  are based on the Gamma distribution (see \code{\link{pgamma}}), and
  those of the Beta functions are based on the Beta distribution (see
  \code{\link{pbeta}}).
  
}

\keyword{ math }
  
\examples{

Cgamma(5 + 1) # = factorial(5)

## P(X >= k) for Poisson distribution with mean alpha
alpha <- 5
k <- 10
Rgamma(k, alpha) # == ppois(k-1, alpha, lower=FALSE)

n <- 49
k <- 6
1 / ((n+1) * Cbeta(n-k+1, k+1)) # == choose(n, k)

## P(X >= k) for binomial distribution with parameters n and p
n <- 100
p <- .1
k <- 15
Rbeta(p, k, n-k+1) # == pbinom(k-1, n, p, lower=FALSE)

}

