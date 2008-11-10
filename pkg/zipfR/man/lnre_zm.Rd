\name{lnre.zm}
\alias{lnre.zm}
\title{The Zipf-Mandelbrot (ZM) LNRE Model (zipfR)}
\description{

  The Zipf-Mandelbrot (ZM) LNRE model of Evert (2004).

  The constructor function \code{lnre.zm} is not user-visible.  It is
  invoked implicitly when \code{lnre} is called with LNRE model type
  \code{"zm"}.
  
}

\usage{

  lnre.zm(alpha=.8, B=.01, param=list())

  ## user call: lnre("zm", spc=spc) or lnre("zm", alpha=.8, B=.1)

}

\arguments{

  \item{alpha}{the \emph{shape} parameter \eqn{\alpha}, a number in the
    range \eqn{(0,1)}}

  \item{B}{the \emph{upper cutoff} parameter \eqn{B}, a positive number
    (\eqn{B > 1} is allowed although it is inconsistent with the
    interpretation of \eqn{B})}
    
  \item{param}{a list of parameters given as name-value pairs
    (alternative method of parameter specification)}
    
}

\value{

  A partially initialized object of class \code{lnre.zm}, which is
  completed and passed back to the user by the \link{lnre} function.
  See \code{\link{lnre}} for a detailed description of \code{lnre.zm}
  objects (as a subclass of \code{lnre}).
  
}

\details{

  The parameters of the ZM model can either be specified as immediate
  arguments:

  \preformatted{
    lnre.zm(alpha=.5, B=.1)
  }

  or as a list of name-value pairs:

  \preformatted{
    lnre.zm(param=list(alpha=.5, B=.1))
  }

  which is usually more convenient when the constructor is invoked by
  another function (such as \code{lnre}).  If both immediate arguments
  and the \code{param} list are given, the immediate arguments override
  conflicting values in \code{param}.  For any parameters that are
  neither specified as immediate arguments nor listed in \code{param},
  the defaults from the function prototype are inserted.

  The \code{lnre.zm} constructor also checks the types and ranges of
  parameter values and aborts with an error message if an invalid
  parameter is detected.
  
}

\section{Mathematical Details}{

  The \bold{ZM model} is a re-formulation of the \bold{Zipf-Mandelbrot}
  law

  \deqn{
    \pi_k = \frac{C}{(k + b) ^ a}
  }{
    pi_k = C / (k + b)^a
  }

  with parameters \eqn{a > 1} and \eqn{b \ge 1} (see also Baayen 2001,
  101ff) as a LNRE model.  It is given by the \bold{type density
  function}

  \deqn{
    g(\pi) := C\cdot \pi^{-\alpha-1}
  }{
    g(pi) := C * pi^(-alpha-1)
  }

  for \eqn{0 \le \pi \le B} (and \eqn{\pi = 0} otherwise), with the
  \bold{parameters} \eqn{0 < \alpha < 1} and \eqn{0 < B \le 1}.  The
  normalizing constant is 

  \deqn{
    C = \frac{ 1 - \alpha }{ B^{1 - \alpha} }
  }{
    C = (1 - alpha) / B^(1-alpha)
  }

  and the population vocabulary size is \eqn{S = \infty}{S = Inf}.  The
  parameters of the ZM model are related to those of the original
  Zipf-Mandelbrot law by \eqn{a = 1/\alpha} and \eqn{b = (1 -
  \alpha)/(B \cdot \alpha)}{b = (1 - alpha) / (B * alpha)}.  See Evert
  (2004) for further details.

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

