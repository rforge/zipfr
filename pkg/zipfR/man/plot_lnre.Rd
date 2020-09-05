\name{plot.lnre}
\alias{plot.lnre}
\title{Plot LNRE Population Distribution (zipfR)}

\description{

  Visualisation of LNRE population distribution, showing either the (log-transformed)
  type density function or the cumulative probability distribution function.

}

\usage{

\method{plot}{lnre}(x, y, ...,
     type=c("density", "cumulative"),
     xlim=c(1e-9, 1), ylim=NULL, steps=200,
     xlab=NULL, ylab=NULL, legend=NULL, grid=FALSE,
     main="LNRE Population Distribution",
     lty=NULL, lwd=NULL, col=NULL, bw=zipfR.par("bw"))

}

\arguments{

  \item{x, y, ...}{one or more objects of class \code{lnre}, containing trained
    LNRE models describing the population(s) to be plotted}

  \item{type}{what type of plot should be drawn, \code{"density"} for the log-transformed
    type density function and \code{"cumulative"} for the cumulative probability distribution.}

  \item{xlim, ylim}{visible range on x- and y-axis.  The default \code{ylim} is \eqn{[0, 1]}
    for \code{type="cumulative"} and automatically chosen to fit the selected density curves  
    for \code{type="density"}.  Note that the x-axis is always logarithmic and \code{xlim}
    should be chosen accordingly.}

  \item{steps}{number of steps for drawing curves (increase for extra smoothness)}

  \item{xlab, ylab}{labels for the x-axis and y-axis (with suitable defaults depending on \code{type})}

  \item{legend}{optional vector of character strings or expressions
    specifying labels for a legend box, which will be drawn in the upper
    right-hand or left-hand corner of the screen.  If \code{legend=TRUE},
    labels showing model type and parameters are automatically generated.}

  \item{grid}{whether to display a suitable grid in the background of the plot}

  \item{main}{a character string or expression specifying a main title for the plot}

  \item{lty, lwd, col}{style vectors that can be used to
    override the global styles defined by \code{\link{zipfR.par}}.  If
    these vectors are specified, they must contain at least as many
    elements as the number of populations shown in the plot:
    the values are \emph{not} automatically recycled.}

  \item{bw}{if \code{TRUE}, draw plot in B/W style (default is the
    global \code{\link{zipfR.par}} setting)}

}

\details{

  There are two useful ways of visualising a LNRE population distribution, selected with the 
  \code{type} argument:

  \describe{
    \item{\code{density}}{
      A plot of the type density function \eqn{g(\pi)} over the type probability \eqn{\pi}
      on a log-transformed scale (so that the number of types corresponds to an integral over
      \eqn{\log_{10} \pi}, see \code{\link{ltdlnre}}). 
      The log transformation is essential so that the density function
      remains in a reasonable range; a logarithmic y-axis would be very counter-intuitive.
    }
    \item{\code{cumulative}}{
      A plot of the cumulative probability distribution, i.e. the distribution function 
      \eqn{F(\rho) = P(\pi \le \rho)} showing the total probability mass of types with 
      type probability \eqn{\pi \le \rho}.  The x-axis shows \eqn{\rho} on a logarithmic scale
      (but is labelled more intuitively with \eqn{\pi} by default).  No special transformations
      are required because \eqn{0 \le F(\rho) \le 1}.
    }
  }
  
  Line styles are defined globally through \code{zipfR.par},
  but can be overridden with the optional parameters
  \code{lty}, \code{lwd} and \code{col}.  In most cases, it is more advisable to
  change the global settings temporarily for a sequence of plots, though.

  The \code{bw} parameter is used to switch between B/W and colour
  modes.  It can also be set globally with \code{zipfR.par}.

  Other standard graphics parameters (such as \code{cex} or \code{mar}) cannot
  be passed to the plot function an need to be set up with \code{\link{par}}
  in advance.

}

\seealso{

  \code{\link{lnre}}, \code{\link{ltdlnre}}, \code{\link{plnre}}
  \code{\link{zipfR.par}}, \code{\link{zipfR.plotutils}}
  
  \code{\link{plot.tfl}} offers a different visualisation of the LNRE population distribution,
  in the form of a Zipf-Mandelbrot law rather than type density.

}

\examples{
 
  models <- lapply(c("zm", "fzm", "gigp"), lnre, Dickens.spc)

  plot(models[[1]], models[[2]], models[[3]], type="density", 
       xlim=c(1e-8, 1e-2), ylim=c(0, 7.5e4), legend=TRUE)

  plot(models[[1]], models[[2]], type="cumulative", grid=TRUE,
       xlim=c(1e-8, 1e-2), legend=c("ZM", "fZM"))

  models <- lapply(seq(.1, .9, .2), function (x) lnre("zm", alpha=x, B=.1))
  do.call(plot.lnre, models)

}

\keyword{ hplot }
