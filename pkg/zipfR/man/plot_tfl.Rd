\name{plot.tfl}
\alias{plot.tfl}
\title{Plot Type-Frequency List / Zipf Ranking (zipfR)}

\description{

  Zipf ranking plot of a type-frequency list, or comparison of several Zipf rankings,
  on linear or logarithmic scale.

}

\usage{

\method{plot}{tfl}(x, y, ...,
     min.rank=1, max.rank=NA, log="", 
     type=c("p","l","b","c","o","h","s"),
     bw=zipfR.par("bw"), cex=1,
     xlim=NULL, ylim=NULL, freq=TRUE,
     xlab="rank", ylab="frequency", legend=NULL,
     main="Type-Frequency List (Zipf ranking)",
     pch=NULL, lty=NULL, lwd=NULL, col=NULL)

}

\arguments{

  \item{x, y, ...}{one or more objects of class \code{tfl}, containing the
    type frequency list(s) to be plotted}

  \item{min.rank, max.rank}{range of Zipf ranks to be plotted for each type-frequency list.
    By default, all ranks are shown.}

  \item{log}{a character string specifying the axis or axes for which
    logarithmic scale is to be used (\code{"x"}, \code{"y"}, or
    \code{"xy"}), similar to the \code{log} argument of
    \code{\link{plot.default}}.}

  \item{bw}{if \code{TRUE}, draw plot in B/W style (default is the
    global \code{\link{zipfR.par}} setting)}
    
  \item{type}{what type of plot should be drawn.  See \code{\link{plot.default}} for details.}

  \item{cex}{scaling factor for plot symbols (types \code{"p"}, \code{"b"} and \code{"o"}).
    This scaling factor is \emph{not} applied to other text elements in the plot;
    use \code{\link{par}} for this purpose.}

  \item{xlim, ylim}{visible range on x- and y-axis.  The default values
    are automatically determined to fit the selected data in the plot.}

  \item{freq}{if \code{freq=FALSE}, plot relative frequency (per million words) instead of
    absolute frequency on the y-axis.}

  \item{xlab, ylab}{labels for the x-axis and y-axis.}

  \item{legend}{optional vector of character strings or expressions,
    specifying labels for a legend box, which will be drawn in the upper
    right-hand corner of the screen.  If \code{legend} is given, its
    length must correspond to the number of type-frequeny lists in the
    plot.}

  \item{main}{a character string or expression specifying a main title for the plot}

  \item{pch, lty, lwd, col}{style vectors that can be used to
    override the global styles defined by \code{\link{zipfR.par}}.  If
    these vectors are specified, they must contain at least as many
    elements as the number of type-frequency lists shown in the plot:
    the values are \emph{not} automatically recycled.}

}

\details{

  The type-frequency lists are shown as Zipf plots, i.e. scatterplots of
  the Zipf-ranked frequencies on a linear or logarithmic scale.  The same
  plotting styles are available as in \code{\link{plot.default}}.

  Line and point styles are defined globally through \code{zipfR.par},
  but can be overridden with the optional parameters \code{pch},
  \code{lty}, \code{lwd} and \code{col}.  In most cases, it is more advisable to
  change the global settings temporarily for a sequence of plots, though.

  The \code{bw} parameter is used to switch between B/W and colour
  modes.  It can also be set globally with \code{zipfR.par}.

}

\seealso{

  \code{\link{tfl}}, \code{\link{vec2tfl}}, \code{\link{rlnre}}, \code{\link{spc2tfl}}, 
  \code{\link{plot.spc}}, \code{\link{plot.vgc}},
  \code{\link{zipfR.par}}, \code{\link{zipfR.plotutils}}

}

\examples{
 
  ## TODO ##

}

\keyword{ hplot }
