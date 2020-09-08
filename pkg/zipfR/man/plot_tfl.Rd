\name{plot.tfl}
\alias{plot.tfl}
\title{Plot Type-Frequency List / Zipf Ranking (zipfR)}

\description{

  Zipf ranking plot of a type-frequency list, or comparison of several Zipf rankings,
  on linear or logarithmic scale.

}

\usage{
\method{plot}{tfl}(x, y, ...,
     min.rank=1, max.rank=NULL, log="", 
     type=c("p", "l", "b", "o", "s"),
     xlim=NULL, ylim=NULL, freq=TRUE,
     xlab="rank", ylab="frequency", legend=NULL, grid=FALSE,
     main="Type-Frequency List (Zipf ranking)",
     bw=zipfR.par("bw"), cex=1, steps=200,
     pch=NULL, lty=NULL, lwd=NULL, col=NULL)

}

\arguments{

  \item{x, y, ...}{one or more objects of class \code{tfl}, containing the
    type frequency list(s) to be plotted.  LNRE models of class \code{lnre}
    can also be specified to display the corresponding population Zipf rankings
    (see \sQuote{Details} for more information).  It is also possible to pass
    all objects as a list in argument \code{x}, but the method needs to be called
    explicitly in this case (see \sQuote{Examples}).}

  \item{min.rank, max.rank}{range of Zipf ranks to be plotted for each type-frequency list.
    By default, all ranks are shown.}

  \item{log}{a character string specifying the axis or axes for which
    logarithmic scale is to be used (\code{"x"}, \code{"y"}, or
    \code{"xy"}), similar to the \code{log} argument of
    \code{\link{plot.default}}.}

  \item{type}{what type of plot should be drawn. Types \code{p} (points),
    \code{l} (lines), \code{b} (both) and \code{o} (points over lines) are the
    same as in \code{\link{plot.default}}. Type \code{s} plots a step function 
    with type frequencies corresponding to right upper corners (i.e.
    type \code{S} in \code{\link{plot}}); it is recommended for plotting full
    type-frequency lists and can be much more efficient than the other types.
    See \sQuote{Details} below.}

  \item{xlim, ylim}{visible range on x- and y-axis.  The default values
    are automatically determined to fit the selected data in the plot.}

  \item{freq}{if \code{freq=FALSE}, plot relative frequency (per million words) instead of
    absolute frequency on the y-axis.  This is useful for comparing type-frequency lists
    with different sample size and is required for plotting LNRE populations.}

  \item{xlab, ylab}{labels for the x-axis and y-axis.}

  \item{legend}{optional vector of character strings or expressions,
    specifying labels for a legend box, which will be drawn in the upper
    right-hand corner of the screen.  If \code{legend} is given, its
    length must correspond to the number of type-frequeny lists in the
    plot.}

  \item{grid}{whether to display a suitable grid in the background of the plot 
    (only for logarithmic axis)}

  \item{main}{a character string or expression specifying a main title for the plot}

  \item{bw}{if \code{TRUE}, draw plot in B/W style (default is the
    global \code{\link{zipfR.par}} setting)}
    
  \item{cex}{scaling factor for plot symbols (types \code{"p"}, \code{"b"} and \code{"o"}).
    This scaling factor is \emph{not} applied to other text elements in the plot;
    use \code{\link{par}} for this purpose.}

  \item{steps}{number of steps for drawing population Zipf rankings of LNRE models. These
    are always drawn as lines (regardless of \code{type}) and are not aligned with integer
    type ranks (because the LNRE models are actually continuous approximations).}

  \item{pch, lty, lwd, col}{style vectors that can be used to
    override the global styles defined by \code{\link{zipfR.par}}.  If
    these vectors are specified, they must contain at least as many
    elements as the number of type-frequency lists shown in the plot:
    the values are \emph{not} automatically recycled.}

}

\details{

  The type-frequency lists are shown as Zipf plots, i.e. scatterplots of
  the Zipf-ranked frequencies on a linear or logarithmic scale.  Only a 
  sensible subset of the default plotting styles described in \code{\link{plot}}
  are supported: \code{p} (points), \code{l} (lines), \code{b} (both, with a margin around points), 
  \code{o} (both overplotted) and \code{s} (stair steps, but actually of type \code{S}).
  
  For plotting complete type-frequency lists from larger samples, type \code{s} is
  strongly recommended.  It aggregates all types with the same frequency and is thus
  much more efficient than the other plot types.  Note that the points shown by the 
  other plot types coincide with the the right upper corners of the stair steps.

  Trained LNRE models can also be included in the plot, but only with \code{freq=FALSE}.
  In this case, the corresponding
  population Zipf rankings are displayed as lines (i.e. always type \code{l}, regardless
  of the \code{type} parameter). The lines are intended to be smooth and are not aligned
  with integer type ranks in order to highlight the fact that LNRE models are continuous
  approximations of the discrete population. 

  Line and point styles are defined globally through \code{zipfR.par},
  but can be overridden with the optional parameters \code{pch},
  \code{lty}, \code{lwd} and \code{col}.  In most cases, it is more advisable to
  change the global settings temporarily for a sequence of plots, though.

  The \code{bw} parameter is used to switch between B/W and colour
  modes.  It can also be set globally with \code{zipfR.par}.

}

\seealso{

  \code{\link{tfl}}, \code{\link{vec2tfl}}, \code{\link{rlnre}}, \code{\link{spc2tfl}}, 
  \code{\link{plot.spc}}, \code{\link{plot.vgc}}, \code{\link{plot.lnre}},
  \code{\link{zipfR.par}}, \code{\link{zipfR.plotutils}}

}

\examples{
## plot tiny type-frequency lists (N = 100) for illustration
tfl1 <- vec2tfl(EvertLuedeling2001$bar[1:100])
tfl2 <- vec2tfl(EvertLuedeling2001$lein[1:100])
plot(tfl1, type="b")
plot(tfl1, type="b", log="xy")
plot(tfl1, tfl2, legend=c("bar", "lein"))

## realistic type-frequency lists (type="s" recommended for efficiency)
tfl1 <- spc2tfl(BrownImag.spc)
tfl2 <- spc2tfl(BrownInform.spc)
plot(tfl1, tfl2, log="xy", type="s", 
     legend=c("fiction", "non-fiction"), grid=TRUE)
## always use freq=FALSE to compare samples of different size
plot(tfl1, tfl2, log="xy", type="s", freq=FALSE,
     legend=c("fiction", "non-fiction"), grid=TRUE)

## show Zipf-Mandelbrot law fitted to low end of frequency spectrum
m1 <- lnre("zm", BrownInform.spc)
m2 <- lnre("fzm", BrownInform.spc)
plot(tfl1, tfl2, m1, m2, log="xy", type="s", freq=FALSE, grid=TRUE,
     legend=c("fiction", "non-fiction", "ZM", "fZM"))

## call plot.tfl explicitly if only LNRE populations are displayed
plot.tfl(m1, m2, max.rank=1e5, freq=FALSE, log="xy")

## first argument can then also be a list of TFLs and/or LNRE models
plot.tfl(lapply(EvertLuedeling2001, vec2tfl), log="xy", type="s", freq=FALSE, 
         legend=names(EvertLuedeling2001))
}

\keyword{ hplot }
