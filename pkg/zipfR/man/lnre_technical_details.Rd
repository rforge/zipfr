\name{lnre.details}
\alias{lnre.details}
\alias{lnre.technical.details}
\title{Technical Details of LNRE Model Objects (zipfR)}
\description{

  This manpage describes technical details of LNRE models and parameter
  estimation.  It is intended developers who want to implement new LNRE
  models, improve the parameter estimation algorithms, or work directly
  with the internals of \code{lnre} objects.  All information required
  for standard applications of LNRE models can be found on the
  \code{\link{lnre}} manpage.
  
}

\details{

  Most operations on LNRE models (in particular, computation of expected
  values and variances, distribution function and type distribution,
  random sampling, etc.) are realized as S3 methods, so they are
  automatically dispatched to appropriate implementations for the
  various types of LNRE models (e.g., \code{EV.lnre.zm},
  \code{EV.lnre.fzm} and \code{EV.lnre.gigp} for the \code{EV} method).
  For some methods (e.g. estimated variances \code{VV} and \code{VVm}),
  a single generic implementation can be used for all model types,
  provided through the base class (\code{VV.lnre} and \code{VVm.lnre}
  for variances).

  If you want to implement new LNRE models, have a look at "Implementing
  LNRE Models" below.

  \bold{Important note:} LNRE model parameters can be passed as named
  arguments to the \code{lnre} constructor function when they are not
  estimated automatically from an observed frequency spectrum.  For this
  reason, parameter names must be carefully chosen so that they do not
  clash with other arguments of the \code{lnre} function.  Note that
  because of \R's argument matching rules, any parameter name that is a
  \emph{prefix} of a standard argument name will lead to such a clash.
  In particular, single-letter parameters (such as \eqn{b} and \eqn{c}
  for the GIGP model) should always be written in uppercase (\code{B}
  and \code{C} in \code{lnre.gigp}).
  
}

\value{

  A LNRE model with estimated (or manually specified) parameter values
  is represented by an object belonging to a suitable subclass of
  \code{lnre}.  The specific class depends on the type of LNRE model, as
  specified in the \code{type} argument to the \code{lnre} constructor
  function (e.g. \code{lnre.fzm} for a fZM model selected with
  \code{type="fzm"}).

  All subtypes of \code{lnre} object share the same data format, viz. a
  list with the following components:

  \item{type}{a character string specifying the class of LNRE model,
    e.g. \code{"fzm"} for a finite Zipf-Mandelbrot model}

  \item{name}{a character string specifying a human-readable name for
    the LNRE model, e.g. \code{"finite Zipf-Mandelbrot"}}
    
  \item{param}{list of named model parameters, e.g. \code{(alpha=.8,
      B=.01)} for a ZM model}

  \item{param2}{a list of "secondary" parameters, i.e. constants that
    can be determined from the model parameters but are frequently used
    in the formulae for expected values, variances, etc.;
    e.g. \code{(C=.5)} for the ZM model above}

  \item{S}{population size, i.e. number of types in the population
    described by the LNRE model (may be \code{Inf}, e.g. for a ZM
    model)}

  \item{exact}{whether approximations are allowed when calculating
    expectations and variances (\code{FALSE}) or not (\code{TRUE})}

  \item{multinomial}{whether to use equations for multionmial sampling
    (\code{TRUE}) or independent Poisson sampling (\code{FALSE})}
    
  \item{spc}{an object of class \code{spc}, the observed frequency
    spectrum from which the model parameters have been estimated (only
    if the LNRE model is based on empirical data)}

  \item{gof}{an object of class \code{lnre.gof} with goodness-of-fit
    information for the estimated LNRE model (only if based on empirical
    data, i.e. if the \code{spc} component is also present)}

  \item{util}{a set of utility functions, given as a list with the
    following components:

    \describe{

      \item{\code{update}:}{function with signature \code{(self, param,
	transformed=FALSE)}, which updates the parameters of the LNRE
	model \code{self} with the values in \code{param}, checks that
	their values are in the allowed range, and re-calculates
	"secondary" parameters and lexicon size if necessary.  If
	\code{transformed=TRUE}, the specified parameters are translated
	back to normal scale before the update (see below).  Of course,
	\code{self} should be the object from which the utility function
	was called.  \code{update} returns a modified version of the
	object \code{self}.}
      
      \item{\code{transform}:}{function with signature \code{(param,
	inverse=FALSE)}, which transform model parameters (given as a
	list in the argument \code{param}) to an unbounded range
	centered at 0, and back (with option \code{inverse=TRUE}).  The
	transformed model parameters are used for parameter estimation,
	so that unconstrained minimization algorithms can be applied.
	The link function for the transformation depends on the LNRE
	model and the "distribution" of each parameter.  A felicitous
	choice can be crucial for robust and quick parameter estimation,
	especially with Newton-like gradient algorithms.  Note that
	setting all transformed parameters to 0 should provide a
	reasonable starting point for the parameter estimation.}

      \item{\code{print}:}{partial print method for this subclass of
	LNRE model, which displays the name of the model, its
	parameters, and optionally some additional information (invoked
	internally by \code{print.lnre} and \code{summary.lnre})}
	
    }

  }    
  
}

\section{Implementing LNRE Models}{

  In order to implement a new class of LNRE models, the following steps
  are necessary (illustrated on the example of a lognormal type density
  function, introducing the new LNRE class \code{lnre.lognormal}):

  \itemize{

    \item Provide a constructor function for LNRE models of this type
    (here, \code{lnre.lognormal}), which must accept the parameters of
    the LNRE model as named arguments with reasonable default values (or
    alternatively as a list passed in the \code{param} argument).  The
    constructor must return a partially initialized object of an
    appropriate subclass of \code{lnre} (\code{lnre.lognormal} in our
    example), and make sure that this object also inherits from the
    \code{lnre} class.

    \item Provide the \code{update}, \code{transform} and \code{print}
    utility functions for the LNRE model, which must be returned in the
    \code{util} field of the LNRE model object (see "Value" above).

    \item Add the new type of LNRE model to the \code{type} argument of
    the generic \code{lnre} constructor, and insert the new constructor
    function (\code{lnre.lognormal}) in the \code{switch} call in the
    body of \code{lnre}.

    \item As a minimum requirement, implementations of the \code{EV} and
    \code{EVm} methods must be provided for the new LNRE model (in our
    example, they will be named \code{EV.lnre.lognormal} and
    \code{EVm.lnre.lognormal}).

    \item If possible, provide equations for the type density,
    probability density, type distribution and distribution function of
    the new LNRE model, as implementations of the \code{tdlnre},
    \code{dlnre}, \code{tplnre}/\code{tqlnre} and
    \code{plnre}/\code{qlnre} methods for the new LNRE model class.  If
    all these functions are defined, log-scaled densities and random
    number generation are automatically handled by generic
    implementations.

    \item Optionally, provide a custom function for parameter estimation
    of the new LNRE model, as an implementation of the
    \code{estimate.model} method (here,
    \code{estimate.model.lnre.lognormal}).  Custom parameter estimation
    can considerably improve convergence and goodness-of-fit if it is
    possible to obtain direct estimates for one or more of the
    parameters, e.g. from the condition \eqn{E[V] = V}.  However, the
    default Nelder-Mead algorithm is robust and produces satisfactory
    results, as long as the LNRE model defines an appropriate parameter
    transformation mapping.  It is thus often more profitable to
    optimize the \code{transform} utility than to spend a lot of time
    implementing a complicated parameter estimation function.

  }

  The best way to get started is to take a look at one of the existing
  implementations of LNRE models.  The GIGP model represents a "minimum"
  implementation (without custom parameter estimation and distribution
  functions), whereas ZM and fZM provide good examples of custom
  parameter estimation functions.
  
}

\seealso{

  User-level information about LNRE models and parameter estimation can
  be found on the \code{\link{lnre}} manpage.
  
  Descriptions of the different LNRE models implemented in \code{zipfR}
  and their parameters are given on separate manpages
  \code{\link{lnre.zm}}, \code{\link{lnre.fzm}} and
  \code{\link{lnre.gigp}}.  These descriptions are intended for
  interested end users, but are not required for standard applications
  of the models.

  The \code{\link{estimate.model}} manpage explains details of the
  parameter estimation procedure (intended for developers).
  
  See \code{\link{lnre.goodness.of.fit}} for a description of the
  goodness-of-fit test performed after parameter estimation of an LNRE
  model.  This function can also be used to evaluate the predictions of
  the model on a different data set.
    
}

\keyword{ programming }
\keyword{ models }


