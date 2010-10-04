\name{plot.spc}
\alias{plot.spc}
\title{Plot Word Frequency Spectra (zipfR)}

\description{

  Plot a word frequency spectrum, or a comparison of several word
  frequency spectra, either as a side-by-side barplot or as points and
  lines on various logarithmic scales.

}

\usage{

  \method{plot}{spc}(x, y, ...,
       m.max=if (log=="") 15 else 50, 
       log="", conf.level=.95,
       bw=zipfR.par("bw"), points=TRUE,
       xlim=NULL, ylim=NULL,
       xlab="m", ylab="V_m", legend=NULL,
       main="Frequency Spectrum",
       barcol=NULL, pch=NULL, lty=NULL, lwd=NULL, col=NULL)

}

\arguments{

  \item{x, y, ...}{one or more objects of class \code{spc}, representing
    observed or expected frequency spectra to be plotted}

  \item{m.max}{number of frequency classes that will be shown in plot.
    The default is 15 on linear scale and 50 when using any type of
    logarithmic scale.}

  \item{log}{a character string specifying the axis or axes for which
    logarithmic scale is to be used (\code{"x"}, \code{"y"}, or
    \code{"xy"}), similar to the \code{log} argument of
    \code{\link{plot.default}}.  By default, a barplot on linear scale
    is displayed.  Use \code{log=""} to show non-logarithmic
    points-and-lines plot (also see "Details" below).}

  \item{conf.level}{confidence level for confidence intervals in
    logarithmic plots (see "Details" below).  The default value of
    \eqn{.95} produces 95\%-confidence intervals.  Set to \code{NA}
    in order to suppress confidence interval markers.}
    
  \item{bw}{if \code{TRUE}, draw plot in B/W style (default is the
    global \code{\link{zipfR.par}} setting)}

  \item{points}{if \code{TRUE}, spectrum plots on any type of
    logarithmic scale are drawn as overplotted lines and points
    (default).  Otherwise, they are drawn as lines with different
    styles.}

  \item{xlim, ylim}{visible range on x- and y-axis.  The default values
    are automatically determined to fit the selected data in the plot.}

  \item{xlab, ylab}{labels for the x-axis and y-axis.  The default
    values nicely typeset mathematical expressions.  The y-axis label
    also distinguishes between observed and expected frequency spectra.}

  \item{main}{a character string or expression specifying a main title
    for the plot}

  \item{legend}{optional vector of character strings or expressions,
    specifying labels for a legend box, which will be drawn in the upper
    right-hand corner of the screen.  If \code{legend} is given, its
    length must correspond to the number of frequency spectra in the
    plot.}

  \item{barcol, pch, lty, lwd, col}{style vectors that can be used to
    override the global styles defined by \code{\link{zipfR.par}}.  If
    these vectors are specified, they must contain at least as many
    elements as there are frequency spectra in the plot: the values are
    \emph{not} automatically recycled.}

}

\details{

  By default, the frequency spectrum or spectra are represented as a
  barplot, with both axes using linear scale.  If the \code{log}
  parameter is given, the spectra are shown either as lines in different
  styles (\code{points=FALSE}) or as overplotted points and lines
  (\code{point=TRUE}).  The value of \code{log} specifies which axes
  should use logarithmic scale (specify \code{log=""} for a
  points-and-lines plot on linear scale).

  In y-logarithmic plots, frequency classes with \eqn{V_m = 0} are drawn
  outside the plot region (below the bottom margin) rather than skipped.

  In all logarithmic plots, confidence intervals are indicated for
  expected frequency spectra with variance data (by vertical lines with
  T-shaped hooks at both ends).  The size of the confidence intervals is
  controlled by the \code{conf.level} parameter (default: 95\%).  Set
  \code{conf.level=NA} in order to suppress the confidence interval
  indicators.
  
  Line and point styles, as well as bar colours in the barplot, can be
  defined globally with \code{zipfR.par}.  They can be overridden
  locally with the optional parameters \code{barcol}, \code{pch},
  \code{lty}, \code{lwd} and \code{col}, but this should only be used
  when absolutely necessary.  In most cases, it is more advisable to
  change the global settings temporarily for a sequence of plots.

  The \code{bw} parameter is used to switch between B/W and colour
  modes.  It can also be set globally with \code{zipfR.par}.

}

\seealso{

  \code{\link{spc}}, \code{\link{lnre}}, \code{\link{lnre.spc}},
  \code{\link{plot.tfl}}, \code{\link{plot.vgc}}, \code{\link{zipfR.par}},
  \code{\link{zipfR.plotutils}}

}

\examples{
 
## load Italian ultra- prefix data
data(ItaUltra.spc)

## plot spectrum
plot(ItaUltra.spc)

## logarithmic scale for m (more elements are plotted)
plot(ItaUltra.spc,log="x")

## just lines
plot(ItaUltra.spc,log="x",points=FALSE)

## just the first five elements, then the first 100
plot(ItaUltra.spc,m.max=5)
plot(ItaUltra.spc,m.max=100,log="x")

## compute zm model and expeccted spectrum
zm <- lnre("zm",ItaUltra.spc)
zm.spc <- lnre.spc(zm,N(ItaUltra.spc))

## compare observed and expected spectra (also
## in black and white to print on papers)
plot(ItaUltra.spc,zm.spc,legend=c("observed","expected"))
plot(ItaUltra.spc,zm.spc,legend=c("observed","expected"),bw=TRUE)
plot(ItaUltra.spc,zm.spc,legend=c("observed","expected"),log="x")
plot(ItaUltra.spc,zm.spc,legend=c("observed","expected"),log="x",bw=TRUE)

## re-generate expected spectrum with variances
zm.spc <- lnre.spc(zm,N(ItaUltra.spc),variances=TRUE)

## now 95% ci is shown in log plot
plot(zm.spc,log="x")

## different title and labels
plot(zm.spc,log="x",main="Expected Spectrum with Confidence Interval",xlab="spectrum elements",ylab="expected type counts")

}

\keyword{ hplot }
