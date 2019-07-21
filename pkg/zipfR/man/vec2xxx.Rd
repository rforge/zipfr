\name{vec2xxx}
\alias{vec2tfl}
\alias{vec2spc}
\alias{vec2vgc}
\title{Type-Token Statistics for Samples and Empirical Data (zipfR)}

\description{

  Compute type-frequency list, frequency spectrum and vocabulary growth
  curve from a token vector representing a random sample or an observed
  sequence of tokens.
  
}

\usage{

  vec2tfl(x)

  vec2spc(x)

  vec2vgc(x, steps=200, stepsize=NA, m.max=0)

}

\arguments{

  \item{x}{a vector of length \eqn{N_0}, representing a random sample or
    other observed data set of \eqn{N_0} tokens.  For each token, the
    corresponding element of \code{x} specifies the \emph{type} that the
    token belongs to.  Usually, \code{x} is a character vector, but it
    might also specify integer IDs in some cases.}

  \item{steps}{number of steps for which vocabulary growth data
    \eqn{V(N)} is calculated.  The values of \eqn{N} will be evenly
    spaced (up to rounding differences) from \eqn{N=1} to \eqn{N=N_0}.}

  \item{stepsize}{alternative way of specifying the steps of the
    vocabulary growth curve.  In this case, vocabulary growth data will
    be calculated every \code{stepsize} tokens.  The first step is
    chosen such that the last step corresponds to the full sample
    (\eqn{N=N_0}).  Only one of the parameters \code{steps} and
    \code{stepsize} may be specified.}
  
  \item{m.max}{an integer in the range $1 \ldots 9$, specifying how many
    spectrum elements \eqn{V_m(N)} to include in the vocabulary growth
    curve.  By default only vocabulary size \eqn{V(N)} is calculated,
    i.e. \code{m.max=0}.}
}

\value{

  An object of class \code{tfl}, \code{spc} or \code{vgc}, representing
  the type frequency list, frequency spectrum or vocabulary growth curve
  of the token vector \code{x}, respectively.
  
}

\details{

  There are two main applications for the \code{vec2xxx} functions:

  \describe{

    \item{a)}{They can be used to calculate type-token statistics and
      vocabulary growth curves for random samples generated from a LNRE
      model (with the \code{\link{rlnre}} function).}

    \item{b)}{They provide an easy way to process a user's own data
      without having to rely on external scripts to compute frequency
      spectra and vocabulary growth curves.  All that is needed is a
      text file in one-token-per-line formt (i.e. where each token is
      given on a separate line).  See "Examples" below for further
      hints.}

  }

  Both applications work well for samples of up to approx. 1 million
  tokens.  For considerably larger data sets, specialized external
  software should be used, such as the Perl scripts provided on the
  \code{zipfR} homepage.
  
}

\seealso{

  \code{\link{tfl}}, \code{\link{spc}} and \code{\link{vgc}} for more
  information about type frequency lists, frequency spectra and
  vocabulary growth curves

  \code{\link{rlnre}} for generating random samples (in the form of the
  required token vectors) from a LNRE model

  \code{\link{readLines}} and \code{\link{scan}} for loading token
  vectors from disk files

}

\keyword{ manip }
  
\examples{

## type-token statistics for random samples from a LNRE distribution

model <- lnre("fzm", alpha=.5, A=1e-6, B=.05)
x <- rlnre(model, 100000)

vec2tfl(x)
vec2spc(x)  # same as tfl2spc(vec2tfl(x))
vec2vgc(x)

sample.spc <- vec2spc(x)
exp.spc <- lnre.spc(model, 100000)
\donttest{plot(exp.spc, sample.spc)}

sample.vgc <- vec2vgc(x, m.max=1, steps=500)
exp.vgc <- lnre.vgc(model, N=N(sample.vgc), m.max=1)
\donttest{plot(exp.vgc, sample.vgc, add.m=1)}

\dontrun{
## load token vector from a file in one-token-per-line format
x <- readLines(filename)
x <- readLines(file.choose()) # with file selection dialog 

## you can also perform whitespace tokenization and filter the data
brown <- scan("brown.pos", what=character(0), quote="")
nouns <- grep("/NNS?$", brown, value=TRUE)
plot(vec2spc(nouns))
plot(vec2vgc(nouns, m.max=1), add.m=1)
}
}
