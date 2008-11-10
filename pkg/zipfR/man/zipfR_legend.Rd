\name{zipfR.legend}
\alias{zipfR.legend}
\title{Draw a Legend Box in one of the Corners (zipfR)}

\description{

  \code{zipfR.legend} is a thin wrapper around the standard
  \code{\link{legend}} function, helping to place the legend box in one
  of the corners of the screen without knowing the precise coordinate
  ranges in use.

  Fine control over the placement of the legend box is possible with the
  optional arguments \code{margin.x} and \code{margin.y}.
  \code{zipfR.legend} is used internally by the high-level plotting
  functions \code{\link{plot.spc}} and \code{\link{plot.vgc}}.
  
}

\usage{

  zipfR.legend(corner, margin.x=.05, margin.y=margin.x,
               legend=NULL, bg="white", \dots)

}

\arguments{

  \item{corner}{an integer specifying the corner in which to place the
    legend box (1 = top left, 2 = top right, 3 = bottom right, and 4 =
    bottom left)}

  \item{margin.x}{distance of the legend box from the border of the
    plotting region along the x-axis, as a fraction of the width of the
    plotting region (default: 5\%)}

  \item{margin.y}{distance of the legend box from the border of the
    plotting region along the y-axis, as a fraction of the height of the
    plotting region.  Defaults to the same value as \code{margin.x}, so
    in most cases it is sufficient to specify \code{margin.x} in order
    to move the legend box towards a corner or away from the corner.}

  \item{legend}{a character or expression vector specifying the legend
    text (passed on to \code{legend}}

  \item{bg}{background colour of the legend box.  Defaults to
    \code{"white"}, so parts of the plot behind the box are hidden.}

  \item{\dots}{further arguments are passed on to the \code{legend}
    function and can be used to specify line styles, plot symbols and/or
    fill styles for the legend entries}
  
}


\seealso{

  \code{\link{legend}} for details on how to specify legend entries.
  Most of the options documented there can also be used with
  \code{zipfR.legend}.
  
  \code{\link{plot.spc}}, \code{\link{plot.vgc}} and
  \code{\link{zipfR.plotutils}} for more information about graphics
  functions in \code{zipfR}

}

\examples{

## zipfR.legend() can be used with all standard plots as well

\dontrun{plot(sin, 0, 2*pi)}
\dontrun{zipfR.legend(2, margin.x=.1, legend="Some silly legend")}

}

\keyword{ aplot }
