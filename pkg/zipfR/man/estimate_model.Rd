\name{estimate.model}
\alias{estimate.model}
\title{Estimate LNRE Model Parameters (zipfR)}

\description{

  \bold{Internal function:} Generic method for estimation of LNRE model
  parameters.  Based on the class of its first argument, the method
  dispatches to a suitable implementation of the estimation procedure.

  Unless you are a developer working on the \code{zipfR} source code,
  you are probably looking for the \code{\link{lnre}} manpage.
  
}

\usage{

  estimate.model(model, spc, param.names,
                 method, cost.function, m.max=15,
                 debug=FALSE, ...)

}

\arguments{

  \item{model}{LNRE model object of the appropriate class (a subclass of
    \code{lnre}).  All parameters of the LNRE model that are not listed
    in \code{param.names} must have been initialized to their
    prespecified values in the \code{model} object.}

  \item{spc}{an observed frequency spectrum, i.e. an object of class
    \code{spc}.  The values of the missing parameters will be estimated
    from this frequency spectrum.}

  \item{param.names}{a character vector giving the names of parameters
    for which values have to be estimated ("missing" parameters)}

  \item{method}{name of the minimization algorithm used for parameter
    estimation (see \code{\link{lnre}} for details)}

  \item{cost.function}{cost function to be minimized (see
    \code{\link{lnre}} for details).  \bold{NB:} this is a direct
    reference to the function object rather than just the name of the
    cost function.  Look-up of the appropriate cost function
    implementation is performed in the \code{lnre} constructor.}
  
  \item{m.max}{number of spectrum elements that will be used to compute
    the cost function (passed on to \code{cost.function})}

  \item{debug}{if \code{TRUE}, some debugging and progress information
    will be printed during the estimation procedure}

  \item{...}{additional arguments are passed on and may be used by some
    implementations}

}
  
\value{
  
  A modified version of \code{model}, where the missing parameters
  listed in \code{param.names} have been estimated from the observed
  frequency spectrum \code{spc}.  In addition, goodness-of-fit
  information is added to the object.
  
}

\details{

  By default, \code{estimate.model} dispatches to a generic
  implementation of the estimation procedure that can be used with all
  types of LNRE models (\code{estimate.model.lnre}). 

  This generic implementation can be overridden for specific LNRE
  models, e.g. to calculate better init values or improve the estimation
  procedure in some other way.  To provide a custom implementation for
  Zipf-Mandelbrot models (of class \code{lnre.zm}), for instance, it is
  sufficient to define the corresponding method implementation
  \code{estimate.model.lnre.zm}.  If no custom implementation is
  provided but the user has selected the \code{Custom} method (which is
  the default), \code{estimate.model} falls back on \code{Nelder-Mead}
  for multi-dimensional minimization and \code{NLM} for one-dimensional
  minimization (where Nelder-Mead is considered to be unreliable).

  Parmeter estimation is performed by minimization of the cost function
  passed in the \code{cost.function} argument (see \code{\link{lnre}}
  for details).  Depending on the \code{method} argument, a range of
  different minimization algorithms can be used (see \code{\link{lnre}}
  for a complete listing).  The minimization algorithm always operates
  on \emph{transformed} parameter values, making use of the
  \code{transform} utility provided by LNRE models (see
  \code{\link{lnre.details}} for more information about utility
  functions).  All parameters are initialized to 0 in the transformed
  scale, which should translate to sensible starting points.

  Note that the \code{estimate.model} implementations \emph{do not
  perform any error checking}.  It is the responsibility of the caller
  to make sure that the arguments are sensible and complete.  In
  particular, all model parameters that will not be estimated (i.e. are
  not listed in \code{param.names}) must have been initialized to
  their prespecified values in the \code{model} passed to the function.
  
}

\seealso{

  The user-level function for estimating LNRE models is
  \code{\link{lnre}}.  Its manpage also lists available cost functions
  and minimization algorithms.

  The internal structure of \code{lnre} objects (representing LNRE
  models) is described on the \code{\link{lnre.details}} manpage, which
  also outlines the necessary steps for implementing a new LNRE model.

  The minimization algorithms used are described in detail on the
  \code{\link{nlm}} and \code{\link{optim}} manpages from \R's standard
  library.
  
}

\keyword{ optimize }
\keyword{ models }
  

