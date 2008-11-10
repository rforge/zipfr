\name{lnre.gigp}
\alias{lnre.gigp}
\title{The Generalized Inverse Gauss-Poisson (GIGP) LNRE Model (zipfR)}
\description{

  The Generalized Inverse Gauss-Poisson (GIGP) LNRE model of Sichel
  (1971).

  The constructor function \code{lnre.gigp} is not user-visible.  It is
  invoked implicitly when \code{lnre} is called with LNRE model type
  \code{"gigp"}.
  
}

\usage{

  lnre.gigp(gamma=-.5, B=.01, C=.01, param=list())

  ## user call: lnre("gigp", spc=spc) or lnre("gigp", gamma=-.5, B=.01, C=.01)

}

\arguments{

  \item{gamma}{the \emph{shape} parameter \eqn{\gamma}, a negative
    number in the range \eqn{(-1,0)}.  \eqn{\gamma} corresponds to
    \eqn{-\alpha} in the Zipf-Mandelbrot notation.}

  \item{B}{the \emph{low-frequency decay} parameter \eqn{b}, a
    non-negative number.  This parameter determines how quickly the type
    density function vanishes for \eqn{\pi \to 0}{pi -> 0}, with larger
    values corresponding to faster decay.}
    
  \item{C}{the \emph{high-frequency decay} parameter \eqn{c}, a
    non-negative number.  This parameter determines how quickly the type
    density function vanishes for large values of \eqn{\pi}, with
    \emph{smaller} values corresponding to faster decay.}
    
  \item{param}{a list of parameters given as name-value pairs
    (alternative method of parameter specification)}
    
}

\value{

  A partially initialized object of class \code{lnre.gigp}, which is
  completed and passed back to the user by the \link{lnre} function.
  See \code{\link{lnre}} for a detailed description of \code{lnre.gigp}
  objects (as a subclass of \code{lnre}).
  
}

\details{

  The parameters of the GIGP model can either be specified as immediate
  arguments:

  \preformatted{
    lnre.gigp(gamma=-.47, B=.001, C=.001)
  }

  or as a list of name-value pairs:

  \preformatted{
    lnre.gigp(param=list(gamma=-.47, B=.001, C=.001))
  }

  which is usually more convenient when the constructor is invoked by
  another function (such as \code{lnre}).  If both immediate arguments
  and the \code{param} list are given, the immediate arguments override
  conflicting values in \code{param}.  For any parameters that are
  neither specified as immediate arguments nor listed in \code{param},
  the defaults from the function prototype are inserted.
  
  The \code{lnre.gigp} constructor also checks the types and ranges of
  parameter values and aborts with an error message if an invalid
  parameter is detected.

  Notice that the implementation of GIGP leads to numerical problems
  when estimating the expected frequency of high spectrum elements
  (you might start worrying if you need to go above \eqn{m=150}).

  Note that the parameters \eqn{b} and \eqn{c} are normally written in
  lowercase (e.g. Baayen 2001).  For the technical reasons, it was
  necessary to use uppercase letters \code{B} and \code{C} in this
  implementation.
  
}

\section{Mathematical Details}{

  Despite its fance name, the \bold{Generalized Inverse Gauss-Poisson}
  or \bold{GIGP model} belongs to the same class of LNRE models as ZM
  and fZM.  This class of models is characterized by a power-law in the
  type density function and derives from the \bold{Zipf-Mandelbrot law}
  (see \code{\link{lnre.zm}} for details on the relationship between
  power-law LNRE models and the Zipf-Mandelbrot law).

  The GIGP model is given by the type density function

  \deqn{
    g(\pi) := C\cdot \pi^{\gamma - 1} \cdot
    e^{- \frac{\pi}{c} - \frac{b^2 c}{4 \pi}}
  }{
    g(pi) := C * pi^(gamma-1) * exp(- pi/c - b^2*c/(4*pi))
  }

  with parameters \eqn{-1 < \gamma < 0} and \eqn{b, c \ge 0}.  The
  normalizing constant is

  \deqn{
    C = \frac{(2 / bc)^{\gamma+1}}{K_{\gamma+1}(b)}
  }{
    C = (2/bc)^(gamma+1) / besselK(b, gamma+1)
  }

  and the population vocabulary size is

  \deqn{
    S = \frac{2}{bc} \cdot \frac{K_{\gamma}(b)}{K_{\gamma+1}(b)}
  }{
    S = (2/bc) * (besselK(b, gamma) / besselK(b, gamma+1))
  }

  Note that the "shape" parameter \eqn{\gamma} corresponds to
  \eqn{-\alpha} in the ZM and fZM models.  The GIGP model was introduced
  by Sichel (1971).  See Baayen (2001, 89-93) for further details.
  
}

\references{

  Baayen, R. Harald (2001). \emph{Word Frequency Distributions.} Kluwer,
  Dordrecht.

  Sichel, H. S. (1971). On a family of discrete distributions
  particularly suited to represent long-tailed frequency
  data. \emph{Proceedings of the Third Symposium on Mathematical
    Statistics}, 51-97.
  
}

\seealso{

  \code{\link{lnre}} for pointers to relevant methods and functions for
  objects of class \code{lnre}, as well as a complete listing of LNRE
  models implemented in the \code{zipfR} library.
    
}

\keyword{ distribution }
\keyword{ models }
