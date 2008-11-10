\name{lnre.fzm}
\alias{lnre.fzm}
\title{The finite Zipf-Mandelbrot (fZM) LNRE Model (zipfR)}
\description{

  The finite Zipf-Mandelbrot (fZM) LNRE model of Evert (2004).

  The constructor function \code{lnre.fzm} is not user-visible.  It is
  invoked implicitly when \code{lnre} is called with LNRE model type
  \code{"fzm"}.
  
}

\usage{

  lnre.fzm(alpha=.8, A=1e-9, B=.01, param=list())

  ## user call: lnre("fzm", spc=spc) or lnre("fzm", alpha=.8, A=1e-9, B=.01)

}

\arguments{

  \item{alpha}{the \emph{shape} parameter \eqn{\alpha}, a number in the
    range \eqn{(0,1)}}

  \item{A}{the \emph{lower cutoff} parameter \eqn{A}, a positive number.
    Note that a valid set of parameters must satisfy \eqn{0 < A < B}.}
    
  \item{B}{the \emph{upper cutoff} parameter \eqn{B}, a positive number
    (\eqn{B > 1} is allowed although it is inconsistent with the
    interpretation of \eqn{B})}
    
  \item{param}{a list of parameters given as name-value pairs
    (alternative method of parameter specification)}
    
}

\value{

  A partially initialized object of class \code{lnre.fzm}, which is
  completed and passed back to the user by the \link{lnre} function.
  See \code{\link{lnre}} for a detailed description of \code{lnre.fzm}
  objects (as a subclass of \code{lnre}).
  
}

\details{

  The parameters of the fZM model can either be specified as immediate
  arguments:

  \preformatted{
    lnre.fzm(alpha=.5, A=5e-12, B=.1)
  }

  or as a list of name-value pairs:

  \preformatted{
    lnre.fzm(param=list(alpha=.5, A=5e-12, B=.1))
  }

  which is usually more convenient when the constructor is invoked by
  another function (such as \code{lnre}).  If both immediate arguments
  and the \code{param} list are given, the immediate arguments override
  conflicting values in \code{param}.  For any parameters that are
  neither specified as immediate arguments nor listed in \code{param},
  the defaults from the function prototype are inserted.
  
  The \code{lnre.fzm} constructor also checks the types and ranges of
  parameter values and aborts with an error message if an invalid
  parameter is detected.

  \bold{NB:} parameter estimation is faster and more robust for the
  inexact fZM model, so you might consider passing the
  \code{exact=FALSE} option to \code{lnre} unless you intend to make
  predictions for small sample sizes \eqn{N} and/or high spectrum elements
  \eqn{E[V_m(N)]} (\eqn{m \gg 1}{m >> 1}) with the model.

}

\section{Mathematical Details}{

  Similar to ZM, the \bold{fZM model} is a LNRE re-formulation of the
  \bold{Zipf-Mandelbrot} law for a population with a finite vocabulary
  size \eqn{S}, i.e.

  \deqn{
    \pi_k = \frac{C}{(k + b) ^ a}
  }{
    pi_k = C / (k + b)^a
  }
  
  for \eqn{k = 1, \ldots, S}.  The parameters of the Zipf-Mandelbrot law
  are \eqn{a > 1}, \eqn{b \ge 1} and \eqn{S} (see also Baayen 2001,
  101ff).  The fZM model is given by the \bold{type density function}

  \deqn{
    g(\pi) := C\cdot \pi^{-\alpha-1}
  }{
    g(pi) := C * pi^(-alpha-1)
  }

  for \eqn{A \le \pi \le B} (and \eqn{\pi = 0} otherwise), and has three
  \bold{parameters} \eqn{0 < \alpha < 1} and \eqn{0 < A < B \le 1}.  The
  normalizing constant is

  \deqn{
    C = \frac{ 1 - \alpha }{ B^{1 - \alpha} - A^{1 - \alpha} }
  }{
    C = (1 - alpha) / (B^(1-alpha) - A^(1-alpha))
  }

  and the population vocabulary size is

  \deqn{
    S = \frac{1 - \alpha}{\alpha} \cdot
    \frac{ A^{-\alpha} - B^{-\alpha} }{ B^{1 - \alpha} - A^{1 - \alpha} }
  }{

    S = (C / alpha) * (A^(alpha) - B^(alpha))
  }

  See Evert (2004) and the \code{\link{lnre.zm}} manpage for further
  details.

}

\references{
  
  Baayen, R. Harald (2001). \emph{Word Frequency Distributions.} Kluwer,
  Dordrecht.

  Evert, Stefan (2004). A simple LNRE model for random character
  sequences. \emph{Proceedings of JADT 2004}, 411-422.
  
}

\seealso{

  \code{\link{lnre}} for pointers to relevant methods and functions for
  objects of class \code{lnre}, as well as a complete listing of LNRE
  models implemented in the \code{zipfR} library.
    
}

\keyword{ distribution }
\keyword{ models }
