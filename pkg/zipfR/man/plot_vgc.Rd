\name{plot.vgc}
\alias{plot.vgc}
\title{Plot Vocabulary Growth Curves (zipfR)}

\description{

  Plot a vocabulary growth curve (i.e., \eqn{V(N)} or \eqn{V_m(N)}
  against \eqn{N}), or a comparison of several vocabulary growth curves.

}

\usage{

  \method{plot}{vgc}(x, y, ...,
       m=NA, add.m=NULL, N0=NULL,
       conf.level=.95, conf.style=c("ticks", "lines"),
       log=c("", "x", "y", "xy"),
       bw=zipfR.par("bw"), 
       xlim=NULL, ylim=NULL,
       xlab="N", ylab="V(N)", legend=NULL,
       main="Vocabulary Growth",
       lty=NULL, lwd=NULL, col=NULL)

}

\arguments{

  \item{x, y, ...}{one or more objects of class \code{vgc}, representing
    observed or expected vocabulary growth curves to be plotted}

  \item{m}{a single integer \eqn{m} in the range \eqn{1 \ldots 9}.  If
    specified, graphs will be plotted for \eqn{V_m(N)} instead of
    \eqn{V(N)} (the default).  Note that all \code{vgc} objects to be
    plotted must contain the necessary data in this case.}

  \item{add.m}{a vector of integers in the range \eqn{1 \ldots 9}.  If
    specified, graphs for \eqn{V_m(N)} will be added as thin lines to
    the default \eqn{V(N)} curve, for all specified frequency classes
    \eqn{m}.  This option cannot be combined with the \code{m} option
    above.  See "Details" below.}

  \item{N0}{if specified, draw a dashed vertical line at \eqn{N=N_0},
      indicating the sample size where a LNRE model has been estimated
      (this is never done automatically)}
    
  \item{log}{a character string specifying the axis or axes for which
    logarithmic scale is to be used (\code{"x"}, \code{"y"}, or
    \code{"xy"}), similar to the \code{log} argument of
    \code{\link{plot.default}}.  By default, both axes use linear scale
    (also see "Details" below).}

  \item{conf.level}{confidence level for confidence intervals around
    expected vocabulary growth curves (see "Details" below).  The
    default value of \eqn{.95} produces 95\%-confidence intervals.
    Set to \code{NA} in order to suppress confidence interval
    markers.}

  \item{conf.style}{if \code{"ticks"}, confidence intervals are
    indicated by vertical lines at each data point in the \code{vgc}
    object (default).  If \code{"lines"}, confidence intervals are
    indicated by thin curves above and below the VGC (which may be
    difficult to see when plotting multiple VGCs).  Notice that
    confidence intervals might be so narrow as to be invisible in
    plots (one way to visualize them in such case might be to set an
    extremely conservative confidence level, such as \eqn{.9999}).}
    
  \item{bw}{if \code{TRUE}, draw plot in B/W style (default is the
    global \code{\link{zipfR.par}} setting)}

  \item{xlim, ylim}{visible range on x- and y-axis.  The default values
    are automatically determined to fit the selected data in the plot.}

  \item{xlab, ylab}{labels for the x-axis and y-axis.  The default
    values nicely typeset mathematical expressions.  The y-axis label
    also distinguishes between observed and expected vocabulary growth
    curves, as well as between \eqn{V(N)} and \eqn{V_m(N)}.}

  \item{main}{a character string or expression specifying a main title
    for the plot}

  \item{legend}{optional vector of character strings or expressions,
    specifying labels for a legend box, which will be drawn in the lower
    right-hand corner of the screen.  If \code{legend} is given, its
    length must correspond to the number of VGCs in the plot.}

  \item{lty, lwd, col}{style vectors that can be used to override the
    global styles defined by \code{\link{zipfR.par}}.  If these vectors
    are specified, they must contain at least as many elements as there
    are VGCs in the plot: the values are \emph{not} automatically
    recycled.}

}

\details{

  By default, standard vocabulary growth curves are plotted for all
  specified \code{vgc} objects, i.e. graphs of \eqn{V(N)} against
  \eqn{N}.  If \code{m} is specified, growth curves for hapax legomena
  or other frequency classes are shown instead, i.e. graphs of
  \eqn{V_m(N)} against \eqn{N}.  In this case, all \code{vgc} objects
  must contain the necessary data for \eqn{V_m(N)}.

  Alternatively, the option \code{add.m} can be used to display growth
  curves for one or more spectrum elements \emph{in addition} to the
  standard VGCs.  These growth curves are plotted as thinner lines,
  otherwise matching the styles of the main curves.  Since such plots
  can become fairly confusing and there is no finer control over the
  styles of the additional curves, it is generally not recommended to
  make use of the \code{add.m} option.
  
  Confidence intervals are indicated for expected vocabulary growth
  curves with variance data, either by short vertical lines
  (\code{conf.style="ticks"}, the default) or by thin curves above and
  below the main growth curve (\code{conf.style="lines"}). The size of
  the confidence intervals is controlled by the \code{conf.level}
  parameter (default: 95\%).  Set \code{conf.level=NA} in order to
  suppress the confidence interval indicators.
  
  In y-logarithmic plots, data points with \eqn{V(N) = 0} or \eqn{V_m(N)
  = 0} are drawn outside the plot region (below the bottom margin)
  rather than skipped.

  Line and point styles can be defined globally with \code{zipfR.par}.
  They can be overridden locally with the optional parameters
  \code{lty}, \code{lwd} and \code{col}, but this should only be used
  when absolutely necessary.  In most cases, it is more advisable to
  change the global settings temporarily for a sequence of plots.

  The \code{bw} parameter is used to switch between B/W and color
  modes.  It can also be set globally with \code{zipfR.par}.

}

\seealso{

  \code{\link{vgc}}, \code{\link{lnre}}, \code{\link{lnre.vgc}},
  \code{\link{plot.tfl}}, \code{\link{plot.spc}}, \code{\link{zipfR.par}},
  \code{\link{zipfR.plotutils}}

}

\keyword{ hplot }


  
\examples{

## load Our Mutual Friend spectrum and empirical vgc
data(DickensOurMutualFriend.emp.vgc)
data(DickensOurMutualFriend.spc)

## plot empirical V and V1 growth
plot(DickensOurMutualFriend.emp.vgc,add.m=1)

## use log scale for y-axis
plot(DickensOurMutualFriend.emp.vgc,add.m=1,log="y")

## binomially interpolated vgc at same points as
## empirical vgc
omf.bin.vgc <- vgc.interp(DickensOurMutualFriend.spc,N(DickensOurMutualFriend.emp.vgc))

## compare empirical and interpolated vgc, also with
## thinner lines, and in black and white
plot(DickensOurMutualFriend.emp.vgc,omf.bin.vgc,legend=c("observed","interpolated"))
plot(DickensOurMutualFriend.emp.vgc,omf.bin.vgc,legend=c("observed","interpolated"),lwd=c(1,1)) 
plot(DickensOurMutualFriend.emp.vgc,omf.bin.vgc,legend=c("observed","interpolated"),bw=TRUE)


## load Great Expectations spectrum and use it to
## compute ZM model
data(DickensGreatExpectations.spc)
ge.zm <- lnre("zm",DickensGreatExpectations.spc)

## expected V of Great Expectations at sample
## sizes of OMF's interpolated vgc
ge.zm.vgc <- lnre.vgc(ge.zm,N(omf.bin.vgc))

## compare interpolated OMF Vs and inter/extra-polated
## GE Vs, with a vertical line at sample size
## used to compute GE model
plot(omf.bin.vgc,ge.zm.vgc,N0=N(ge.zm),legend=c("OMF","GE"))


## load Italian ultra- prefix data and compute zm model
data(ItaUltra.spc)
ultra.zm <- lnre("zm",ItaUltra.spc)

## compute vgc up to about twice the sample size
## with variance of V
ultra.zm.vgc <- lnre.vgc(ultra.zm,(1:100)*70, variances=TRUE)

## plot with confidence intervals derived from variance in
## vgc (with larger datasets, ci will typically be almost
## invisible)
plot(ultra.zm.vgc)

## use more conservative confidence level, and plot 
## the intervals as lines
plot(ultra.zm.vgc,conf.level=.99,conf.style="lines")

## suppress ci plotting, and insert different title and labels
plot(ultra.zm.vgc,conf.level=NA,main="ultra-",xlab="sample sizes",ylab="types")

## load Brown adjective spectrum
## (about 80k tokens) 
data(BrownAdj.spc)

## binomially interpolated curve of V and V_1 to V_5
BrownAdj.bin.vgc <- vgc.interp(BrownAdj.spc,(1:100)*800,m.max=5)

## plot with V and 5 spectrum elements
plot(BrownAdj.bin.vgc,add.m=c(1:5))

}
