\name{zipfR.plotutils}
\alias{zipfR.plotutils}
\alias{zipfR.begin.plot}
\alias{zipfR.end.plot}
\alias{zipfR.pick.device}
\title{Plotting Utilities (zipfR)}

\description{

  \bold{These functions are deprecated and should not be used in new code.}

  Conveniently create plots with different layout and in different
  output formats (both on-screen and various graphics file formats).

  Each plot is wrapped in a pair of \code{zipfR.begin.plot} and
  \code{zipfR.end.plot} commands, which make sure that a suitable
  plotting window / image file is opened and closed as required.  Format
  and dimensions of the plots are controlled by global settings made
  with \code{zipfR.par}, but can be overridden in the
  \code{zipfR.begin.plot} call.

  \code{zipfR.pick.device} automatically selects a default device by
  scanning the specified vector for strings of the form \code{--pdf},
  \code{--eps}, etc.
  
  \bold{NB:} These are advanced functions intended to make it easier
  to produce plots in different formats. Most users will only need
  the basic plotting functionalities provided by \code{\link{plot.tfl}}, 
  \code{\link{plot.spc}} and \code{\link{plot.vgc}}.

}

\usage{

zipfR.pick.device(args=commandArgs())

zipfR.begin.plot(device=zipfR.par("device"), filename="",
                 width=zipfR.par("width"), height=zipfR.par("height"),
                 bg=zipfR.par("bg"), pointsize=zipfR.par("pointsize"))

## plotting commands go here

zipfR.end.plot(shutdown=FALSE)

}

\arguments{

  \item{args}{a character vector, which will be scanned for strings of
    the form \code{--pdf}, \code{--eps}, etc.  If \code{args} is not
    specified, the command-line arguments supplied to \R will be
    examined.}
    

    \item{device}{name of plotting device to be used (see "Devices"
    below)}

  \item{filename}{for graphics file devices, \emph{basename} of the
    output file.  A suitable extension for the selected file format will
    be added automatically to \code{filename}.  This parameter is
    ignored for screen devices.}

  \item{width, height}{width and height of the plotting window or image,
    in inches}

  \item{bg}{background colour of the plotting window or image (use
    \code{"transparent"} for images with transparent background)}

  \item{pointsize}{default point size for text in the plot}

  \item{shutdown}{if set to FALSE (the default), on-screen plot devices
    will be kept open for re-use in the next plot.  Specify \code{shutdown=TRUE}
    to ensure that the screen device is closed after a series of related plots.} 
}

\details{

  \code{zipfR.begin.plot} opens a new plotting window or image file of
  the specified dimensions (\code{width}, \code{height}), using the
  selected graphics device (\code{device}).  Background colour
  (\code{bg}) and default point size (\code{pointsize}) are set as
  requested.  Then, any global graphics parameter settings (defined with
  the \code{init.par} option of \code{zipfR.par}) are applied.
  See the \code{\link{zipfR.par}} manpage for the "factory default"
  settings of these options.
  
  \code{zipfR.end.plot} finalizes the current plot.  For image file
  devices, the device will be closed, writing the generated file to
  disk.  For screen devices, the plotting window remains visible until a
  new plot is started (which will close and re-open the plotting
  window).

  The main purpose of the \code{zipfR} plotting utilities is to make it
  easier to draw plots that are both shown on screen (for interactive
  work) and saved to image files in various formats.  If an \R script
  specifies \code{filename}s in all \code{zipfR.begin.plot} commands, a
  single global parameter setting at the start of the script is
  sufficient to switch from screen graphics to EPS files, or any other
  supported file format.

  On-screen plotting devices are platform-dependent, and there may be
  different devices available depending on which version of R is used.
  For this reason, \code{zipfR.begin.plot} no longer allows users to
  pick an on-screen device explicitly, but rather opens a default device
  with \code{\link{dev.new}}. Note that this default device may write
  output to a graphics file, but is usually set to a suitable on-screen
  device in an interactive R session. In any case, users can change the
  default by setting \code{options(device=...)}. For backwards-compatibility,
  the device name \code{x11} (and \code{quartz} on macOS is accepted
  for the default graphics device.
  
  The \code{png} bitmap device may not be available on all platforms,
  and may also require access to an X server.  Since the width and
  height of a PNG device have to be specified in pixels rather than
  inches, \code{zipfR.begin.plot} translates the \code{width} and
  \code{height} settings, assuming a resolution of 150 dpi.  Use of
  the \code{png} device is strongly discouraged.  A better way of
  producing high-quality bitmaps is to generate EPS image (with the
  \code{eps} device) and convert them to PNG or JPEG format with the
  external \code{pstoimg} program (part of the \code{latex2html}
  distribution).

  \code{zipfR.pick.device} will issue a warning if multiple flags
  matching supported graphics devices are found.  However, it is not an
  error to find no matching flag, and all unrecognized strings are
  silently ignored.
  
}

\value{

  \code{zipfR.begin.plot} invisibly returns the ID of the active plot device.
  
}

\section{Devices}{

  Currently, the following devices are supported (and can be used in the
  \code{device} argument).

  \emph{Screen devices:}
  
  \describe{

    \item{\code{x11}}{opens the default graphic device set by
      \code{\link{getOption}("device")}. In an interactive R sessions,
      this will usually be a suitable on-screen device.}

    \item{\code{quartz}}{accepted as an alias for \code{x11} on
      macOS platforms}

  }

  \emph{Graphics file devices:}

  \describe{

    \item{\code{eps}}{Encapsulated PostScript (EPS) output (using
      \code{postscript} device with appropriate settings)}

    \item{\code{pdf}}{PDF output}

    \item{\code{png}}{PNG bitmap file (may not be available on all
      platforms)}

  }
  
}

\seealso{

  \code{\link{zipfR.par}}, \code{\link{par}}

  \link{Devices}, \code{\link{dev.new}}, \code{\link{postscript}},
  \code{\link{pdf}} and \code{png} for more information about the
  supported graphics devices

  \code{zipfR}-specific plotting commands are \code{\link{plot.spc}}, 
  \code{\link{plot.spc}} and \code{\link{plot.vgc}}

}

\examples{
\dontrun{
## these graphics parameters will be set for every new plot
zipfR.par(init.par=list(bg="lightblue", cex=1.3))
zipfR.par(width=12, height=9)

## will be shown on screen or saved to specified file, depending on
## selected device (eps -> "myplot.eps", pdf -> "myplot.pdf", etc.)

zipfR.begin.plot(filename="myplot")
plot.spc(Brown100k.spc)
zipfR.end.plot()

## By starting an R script "myplots.R" with this command, you can
## display plots on screen when stepping through the script in an
## interactive session, or save them to disk files in various
## graphics formats with "R --no-save --args --pdf < myplots.R" etc.
zipfR.pick.device()
}
}

\keyword{ device }
