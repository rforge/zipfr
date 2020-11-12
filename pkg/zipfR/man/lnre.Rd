\name{lnre}
\alias{lnre}
\alias{lnre.object}
\title{LNRE Models (zipfR)}
\description{

  LNRE model constructor, returns an object representing a LNRE model
  with the specified parameters, or allows parameters to be estimated
  automatically from an observed frequency spectrum.
  
}

\usage{

  lnre(type=c("zm", "fzm", "gigp"),
       spc=NULL, debug=FALSE,
       cost=c("gof", "chisq", "linear", "smooth.linear", "mse", "exact"),
       m.max=15, runs=5,
       method=c("Nelder-Mead", "NLM", "BFGS", "SANN", "Custom"),
       exact=TRUE, sampling=c("Poisson", "multinomial"),
       bootstrap=0, verbose=TRUE, parallel=1L,
       \dots)

}

\arguments{

  \item{type}{class of LNRE model to use (see "LNRE Models" below)}

  \item{spc}{observed frequency spectrum used to estimate model
    parameters. After parameter optimisation, goodness-of-fit of 
    the final model is tested again \code{spc}. Can be omitted if
    all model parameters are set explicitly or if not needed by a
    user-defined cost function (see \sQuote{Cost Functions} for details).}

  \item{debug}{if \code{TRUE}, detailed debugging information will be
    printed during parameter estimation}

  \item{cost}{cost function for measuring the "distance" between
    observed and expected vocabulary size and frequency spectrum.
    Parameters are estimated by minimizing this cost function (see 
    \sQuote{Cost Functions} below for a list of built-in cost functions
    and details on user-defined cost functions).}
  
  \item{m.max}{number of spectrum elements considered by the cost
    function (see "Cost Functions" below for more information). If
    unspecified, the default is automatically adjusted to avoid small
    spectrum elements that may be mathematically unreliable.}

  \item{runs}{number of parameter optimization runs with random
    initialization. Parameters from the run that achieves the smallest
    value of the cost function will be selected. Currently not supported
    for \code{method="Custom"}, please use \emph{runs=1} in this case.}
  
  \item{method}{algorithm used for parameter estimation, by minimizing
    the value of the cost function (see "Parameter Estimation" below for
    details, and "Minimization Algorithms" for descriptions of the
    available algorithms)}

  \item{exact}{if \code{FALSE}, certain LNRE models will be allowed to
    use approximations when calculating expected values and variances,
    in order to improve performance and numerical stability.  However,
    the computed values might be inaccurate or inconsistent in "extreme"
    situations: in particular, \eqn{E[V]} might be larger than \eqn{N}
    when \eqn{N} is very small; \eqn{\sum_m E[V_m]} can be larger than
    \eqn{E[V]} at the same \eqn{N}; \eqn{\sum_m m \cdot E[V_m]}{sum_m (m
    * E[V_m])} can be larger than \eqn{N}}

  \item{sampling}{type of random sampling model to use.  \code{Poisson}
    sampling is mathematically simpler and allows fast and robust
    calculations, while \code{multinomial} sampling is more accurate
    especially for very small samples.  \code{Poisson} sampling is the
    default and should be unproblematic for sample sizes \eqn{N \ge
    10000}.  \bold{NB:} The \code{multinomial} sampling option has not
    been implemented yet.}
  
  \item{bootstrap}{number of bootstrap samples used to estimate confidence
    intervals for estimated model parameters.  Recommended values are
    \code{bootstrap=100} or \code{bootstrap=200}.  Bootstrapping can be very
    time-consuming and should not be used if the underlying sample size
    is very large (roughly, more than 1 million tokens).  See
    \code{\link{lnre.bootstrap}} for further information and warnings.}

  \item{parallel}{whether to use parallelisation for the bootstrapping procedure
    (highly recommended). See \code{\link{lnre.bootstrap}} for details.}

  \item{verbose}{if \code{TRUE}, a progress bar will be shown in the R console
    during the bootstrapping procedure}

  \item{\dots}{all further named arguments are interpreted as parameter
    values for the chosen LNRE model (see the respective manpages for
    names and descriptions of the model parameters)}
    
}

\value{

  An object of a suitable subclass of \code{lnre}, depending on the
  \code{type} argument (e.g. \code{lnre.fzm} for \code{type="fzm"}).
  This object represents a LNRE model of the selected type with the
  specified parameter values, or with parameter values estimated from
  the observed frequency spectrum \code{spc}.

  The internal structure of \code{lnre} objects is described on the
  \code{\link{lnre.details}} manpage (intended for developers).
  
}

\details{
  
  Currently, the following LNRE models are supported by the \code{zipfR}
  package:

  The \bold{Zipf-Mandelbrot (ZM)} LNRE model (see \code{\link{lnre.zm}}
  for details).

  The \bold{finite Zipf-Mandelbrot (fZM)} LNRE model (see
  \code{\link{lnre.fzm}} for details).

  The \bold{Generalized Inverse Gauss-Poisson (GIGP)} LNRE model (see
  \code{\link{lnre.gigp}} for details).
  
  If explicit model parameters are specified in addition to an observed
  frequency spectrum \code{spc}, these parameters are fixed to the given
  values and are excluded from the estimation procedure.  This feature
  can be useful if fully automatic parameter estimation leads to a poor
  or counterintuitive fit.
  
}

\section{Parameter Estimation}{

  Automatic parameter estimation for LNRE models is performed by
  matching the expected vocabulary size and frequency spectrum of the
  model against the observed data passed in the \code{spc} argument.

  For this purpose, a \emph{cost function} has to be defined as a
  measure of the "distance" between observed and expected frequency
  spectrum.  Parameters are then estimated by applying a
  \emph{minimization algorithm} in order to find those parameter values
  that lead to the smallest possible cost.

  Parameter estimation is a crucial and often also quite critical step
  in the application of LNRE models.  Depending on the shape of the
  observed frequency spectrum, the automatic estimation procedure may
  result in a poor and counter-intuitive fit, or may fail altogether.

  Usually, multiple runs of the minimization are performed with different
  random start values. An error will only be reported if all the estimation
  runs fail. Such multiple runs have not been implemented for the \code{Custom}
  minimization method yet; please specify \code{runs=1} in this case.
  
  Users can influence parameter estimation by choosing from a range of
  predefined cost functions and from several minimization algorithms, as
  described in the following sections.  Some experimentation with the
  \code{cost}, \code{m.max} and \code{method} arguments will often help
  to resolve estimation failures and may result in a considerably better
  goodness-of-fit.

}

\section{Cost Functions}{

  The following cost functions are available and can be selected with
  the \code{cost} argument.  All functions are based on the differences
  between observed and expected values for vocabulary size and the first
  elements of the frequency spectrum (\eqn{V_1, \ldots, V_m}, where
  \eqn{m} is given by the \code{m.max} argument):

  \describe{

    \item{\code{gof}:}{the multivariate chi-squared statistic used for
      goodness-of-fit testing (\code{\link{lnre.goodness.of.fit}}).
      This cost function corresponds (almost) to maximum-likelihood
      parameter estimation and is used by default.}
    
    \item{\code{chisq}:}{cost function based on a simplified version of
      the multivariate chi-squared test for goodness-of-fit (assuming
      independence between the random variables \eqn{V_m}).
      % This cost function usually achieves the best results in the goodness-of-fit evaluation and is used by default.
    }
      
    \item{\code{linear}:}{linear cost function, which sums over the
      absolute differences between observed and expected values.  This
      cost function puts more weight on fitting the vocabulary size and
      the first few elements of the frequency spectrum (where absolute
      differences are much larger than for higher spectrum elements).}

    \item{\code{smooth.linear}:}{modified version of the linear cost
      function, which smoothes the kink of the absolute value function
      for a difference of \eqn{0} (since non-differentiable cost
      functions might be problematic for gradient-base minimization
      algorithms)}

    \item{\code{mse}:}{mean squared error cost function, averaging over
      the squares of differences between observed and expected values.
      This cost function penalizes large absolute differences more
      heavily than linear cost (and therefore puts even greater weight
      on fitting vocabulary size and the first spectrum elements).}

    \item{\code{exact}:}{this "virtual" cost function attempts to match
      the observed vocabulary size and first spectrum elements exactly,
      ignoring differences for all higher spectrum elements.  This is
      achieved by adjusting the value of \code{m.max} automatically,
      depending on the number of free parameters that are estimated (in
      general, the number of constraints that can be satisfied by
      estimating parameters is the same as the number of free
      parameters).  Having adjusted \code{m.max}, the \code{mse} cost
      function is used to determined parameter values, so that the
      estimation procedure will not fail even if the constraints cannot
      be matched exactly.}

  }
  
  Alternatively a user-defined cost function can be passed as a function object
  with signature `cost(model, spc, m.max)`, which compares the LNRE model `model`
  against the observed frequency spectrum `spc` and returns a cost value
  (i.e. lower cost indicates a better fit).  User-defined cost functions are also
  convenient for setting model parameters based on implicit constraints (such as 
  a desired population diversity \eqn{S}).  In this case, pass \code{spc=NULL}
  explicitly as a dummy frequency spectrum, skipping the final goodness-of-fit test.
  
}

\section{Minimization Algorithms}{

  Several different minimization algorithms can be used for parmeter
  estimation and are selected with the \code{method} argument:

  \describe{

    \item{\code{Nelder-Mead}:}{the Nelder-Mead algorithm, implemented by
      the \code{optim} function, performs minimization without using
      derivatives.  Parameter estimation is therefore very robust, while
      almost as fast and accurate as the \code{NLM} method. 
      \code{Nelder-Mead} is the default algorithm and is also used
      internally by most custom minimization procedures (see below).}

    \item{\code{NLM}:}{a standard Newton-type algorithm for nonlinear
      minimization, implemented by the \code{\link{nlm}} function, which
      makes use of numerical derivatives of the cost function.
      \code{NLM} minimization converges quickly and obtains very precise
      parameter estimates (for a local minimum of the cost function),
      but it is not very stable and may cause parameter estimation to
      fail altogether.}

    \item{\code{SANN}:}{minimization by simulated annealing, also provided by the
      \code{optim} function.  Like \code{Nelder-Mead}, this algorithm is
      very robust because it avoids numerical derivatives, but
      convergence is extremely slow.  In some cases, \code{SANN} might
      produce a better fit than \code{Nelder-Mead} (if the latter
      converges to a suboptimal local minimum).}

    \item{\code{BFGS}:}{a quasi-Newton method developed by Broyden, Fletcher,
      Goldfarb and Shanno. This minimization algorithm is efficient, but
      should be applied with care as it will often overshoot the valid range of
      parameter values.}
    
    \item{\code{Custom}:}{a custom estimation procedure provided 
      for certain types of LNRE model, which may exploit special
      mathematical properties of the model in order to calculate one or
      more of the parameter values directly.  For example, one parameter
      of the ZM and fZM models can easily be determined from the
      constraint \eqn{E[V] = V} (but note that this additional
      constraint leads to a different fit than is obtained by plain
      minimization of the cost function!).  Custom estimation might also
      apply special configuration settings to improve convergence of the
      minimization process, based on knowledge about the valid ranges
      and "behaviour" of model parameters.  If no custom estimation
      procedure has been implemented for the selected LNRE model,
      \code{lnre} falls back on the \code{Nelder-Mead} or \code{NLM}
      algorithm.}

  }

  See the \code{\link{nlm}} and \code{\link{optim}} manpages for more
  information about the minimization algorithms used and key references.
  
}

\seealso{

  Detailed descriptions of the different LNRE models provided by
  \code{zipfR} and their parameters can be found on the manpages
  \code{\link{lnre.zm}}, \code{\link{lnre.fzm}} and
  \code{\link{lnre.gigp}}.

  Useful methods for trained models are \code{\link{lnre.spc}},
  \code{\link{lnre.vgc}}, \code{\link{EV}}, \code{\link{EVm}},
  \code{\link{VV}}, \code{\link{VVm}}.  Suitable implementations of the
  \code{\link{print}} and \code{\link{summary}} methods are also
  provided (see \code{\link{print.lnre}} for details), as well as for
  plotting (see \code{\link{plot.lnre}}).  Note that the
  methods \code{\link{N}}, \code{\link{V}} and \code{\link{Vm}} can be
  applied to LNRE models with estimated parameters and return
  information about the observed frequency spectrum used for parameter
  estimation.
  
  If bootstrapping samples have been generated (\code{bootstrap > 0}), 
  confidence intervals for the model parameters can be determined with
  \code{\link{confint.lnre}}.  See \code{\link{lnre.bootstrap}} for 
  more information on the bootstrapping procedure and implementation.

  The \code{\link{lnre.details}} manpage gives details about the
  implementation of LNRE models and the internal structure of
  \code{lnre} objects, while \code{\link{estimate.model}} has more
  information on the parameter estimation procedure (both manpages are
  intended for developers).

  See \code{\link{lnre.goodness.of.fit}} for a complete description of
  the goodness-of-fit test that is automatically performed after
  parameter estimation (and which is reported in the \code{summary} of
  the LNRE model).  This function can also be used to evaluate the
  predictions of the LNRE model on a different data set than the one
  used for parameter estimation.
    
}

\keyword{ distribution }
\keyword{ models }

\examples{
## load Dickens dataset
data(Dickens.spc)

## estimate parameters of GIGP model and show summary
m <- lnre("gigp", Dickens.spc)
m
\dontshow{
stopifnot( m$gof$X2 < 1000 ) # should achieve this goodness-of-fit
}

## N, V and V1 of spectrum used to compute model
## (should be the same as for Dickens.spc)
N(m)
V(m)
Vm(m,1)
\dontshow{
stopifnot( N(m) == N(Dickens.spc) ) 
stopifnot( V(m) == V(Dickens.spc) ) 
stopifnot( all(Vm(m,1:10) == Vm(Dickens.spc,1:10)) )
}

## expected V and V_m and their variances for arbitrary N 
EV(m,100e6)
VV(m,100e6)
EVm(m,1,100e6)
VVm(m,1,100e6)

## use only 10 instead of 15 spectrum elements to estimate model
## (note how fit improves for V and V1)
m.10 <- lnre("gigp", Dickens.spc, m.max=10)
m.10

## experiment with different cost functions
m.mse <- lnre("gigp", Dickens.spc, cost="mse")
m.mse
m.exact <- lnre("gigp", Dickens.spc, cost="exact")
m.exact
\dontshow{
stopifnot( abs(V(m.exact) - EV(m.exact, N(m.exact))) < .5 )
stopifnot( abs(Vm(m.exact, 1) - EVm(m.exact, 1, N(m.exact))) < .5 ) 
}

## NLM minimization algorithm is faster but less robust
m.nlm <- lnre("gigp", Dickens.spc, method="NLM")
m.nlm

## ZM and fZM LNRE models have special estimation algorithms
m.zm <- lnre("zm", Dickens.spc)
m.zm
m.fzm <- lnre("fzm", Dickens.spc)
m.fzm
\dontshow{
stopifnot( m.fzm$gof$X2 < 1500 ) # fit is not as good as for GIGP model
}

## estimation is much faster if approximations are allowed
m.approx <- lnre("fzm", Dickens.spc, exact=FALSE)
m.approx
\dontshow{
## differences between exact and approximate model shouldn't be too large
stopifnot( abs(m.fzm$param$alpha - m.approx$param$alpha) < .1 )
}

## specify parameters of LNRE models directly
m <- lnre("zm", alpha=.5, B=.01)
lnre.spc(m, N=1000, m.max=10)

m <- lnre("fzm", alpha=.5, A=1e-6, B=.01)
lnre.spc(m, N=1000, m.max=10)

m <- lnre("gigp", gamma=-.5, B=.01, C=.01)
lnre.spc(m, N=1000, m.max=10)

## bootstrapped confidence intervals for model parameters
\dontrun{
model <- lnre("fzm", spc=BrownAdj.spc, bootstrap=40)
confint(model, "alpha") # Zipf slope
confint(model, "S")     # population diversity
confint(model, "S", method="normal") # Gaussian approx works well in this case

## speed up with parallelisation (see ?lnre.bootstrap for more information)
model <- lnre("fzm", spc=BrownAdj.spc, bootstrap=40, 
              parallel=8) # on Linux / MacOS with 8 available cores}
}

