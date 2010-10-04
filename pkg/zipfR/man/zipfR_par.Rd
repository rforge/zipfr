\name{zipfR.par}
\alias{zipfR.par}
\title{Set or Query Graphics Parameters (zipfR)}

\description{

  Set default graphics parameters for \code{zipfR} high-level plots and
  plot utilities, similar to \code{par} for general graphics parameters.
  The current parameter values are queried by giving their names as
  character strings.  The values can be set by specifying them as
  arguments in \code{name=value} form, or by passing a single list of
  named values.

  \bold{NB:} This is an advanced function to fine-tune zipfR
  plots. For basic plotting options (that are likely to be sufficient
  for most purposes) see \code{\link{plot.spc}} and
  \code{\link{plot.vgc}} instead.

}

\usage{

  zipfR.par(..., bw.mode=FALSE)

}

\arguments{

  \item{...}{either character strings (or vectors) specifying the names
    of parameters to be queried, or parameters to be set in
    \code{name=value} form, or a single list of named values.  A listing
    of valid parameter names is given below.}

  \item{bw.mode}{if \code{TRUE} and parameter values are queried, then
    return the corresponding parameters for B/W mode if possible (e.g.,
    \code{zipfR.par("col",bw.mode=TRUE)} returns the value of the
    \code{col.bw} parameter).  Note that \code{bw.mode} cannot be
    abbreviated in the function call!}

}

\value{

  When parameters are set, their former values are returned in an
  invisible named list.  Such a list can be passed as a single argument
  to \code{zipfR.par} to restore the parameter values.

  When a single parameter is queried, its value is returned directly.
  When two or more parameters are queried, the result is a named list.

  Note the inconsistency, which is the same as for \code{par}: setting
  one parameter returns a list, but querying one parameter returns a
  vector (or a scalar, i.e. a vector of length 1).
  
}

\details{

  Parameters are set by specifying their names and the new values as
  \code{name=value} pairs.  Such a list can also be passed as a single
  argument to \code{zipfR.par}, which is typically used to restore previous
  parameter values (that have been saved in a list variable).

  Most of the default values can be manually overridden in the
  high-level plots.
  
  \code{zipfR.par()} shows all parameters with their current values, and
  \code{names(zipfR.par())} produces a listing of valid parameter names.
  
}

\section{zipfR Graphics Parameters}{

  \describe{

    \item{\code{col}}{a character or integer vector specifying up to 10
      line colours (see the \code{\link{par}} manpage for
      details).  Values of shorter vectors are recycled as necessary.}
    
    \item{\code{lty}}{a character or integer vector specifying up to 10
      line styles (see the \code{\link{par}} manpage for
      details).  Values of shorter vectors are recycled as necessary.}
    
    \item{\code{lwd}}{a numeric vector specifying up to 10 line widths
      (see the \code{par} manpage for details).  Values of shorter
      vectors are recycled as necessary.}

    \item{\code{pch}}{a character or integer vector specifying up to 10
      plot symbols.  Values of shorter vectors are recycled as
      necessary.}

    \item{\code{barcol}}{a character or integer vector specifying up to
      10 colours for the bars in non-logarithmic spectrum plots.  Values
      of shorter vectors are recycled as necessary.}
    
    \item{\code{col.bw}}{the line colours used in B/W mode
      (\code{bw=TRUE})}
    
    \item{\code{lty.bw}}{the line styles used in B/W mode
      (\code{bw=TRUE})}
    
    \item{\code{lwd.bw}}{the line widths used in B/W mode
      (\code{bw=TRUE})}

    \item{\code{pch.bw}}{the plot symbols used in B/W mode
      (\code{bw=TRUE})}

    \item{\code{barcol.bw}}{the bar colours used in B/W mode
      (\code{bw=TRUE})}

    \item{\code{bw}}{if \code{TRUE}, plots are drawn in B/W mode unless
      specified otherwise (default: \code{FALSE}, i.e. colour mode}

    \item{\code{device}}{plot device used by the \code{zipfR} plotutils
      (see \code{\link{zipfR.begin.plot}} for details).  Currently
      supported devices are \code{x11} (default on most platforms), \code{eps},
      \code{pdf}, as well as \code{png} and \code{quartz} where
      available (default on Mac OS X).}

    \item{\code{init.par}}{list of named graphics parameters passed to
      the \code{par} function whenever a new viewport is created with
      \code{\link{zipfR.begin.plot}}}

    \item{\code{width}, \code{height}}{default width and height of the
      plotting window opened by \code{zipfR.begin.plot}}
    
  }

}

\seealso{

  \code{\link{plot.spc}}, \code{\link{plot.vgc}},
  \code{\link{zipfR.begin.plot}}, \code{\link{zipfR.end.plot}}

}

\examples{

print(names(zipfR.par()))         # list available parameters

zipfR.par("col", "lty", "lwd")    # the default line styles
zipfR.par(c("col", "lty", "lwd")) # works as well

## temporary changes to graphics paramters:
par.save <- zipfR.par(bw=TRUE, lwd.bw=2)
## plots use the modified parameters here
zipfR.par(par.save)		  # restore previous values

}

\keyword{ iplot }
