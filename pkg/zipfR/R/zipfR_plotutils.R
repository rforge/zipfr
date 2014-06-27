zipfR.begin.plot <- function (device=zipfR.par("device"), filename="",
                              width=zipfR.par("width"), height=zipfR.par("height"),
                              bg=zipfR.par("bg"), pointsize=zipfR.par("pointsize"))
{
  on.screen <- device %in% c("x11", "quartz") # whether this is an on-screen device
  if (!on.screen) {
    if (device %in% c("png", "eps", "pdf")) {
      if (missing(filename) || filename=="") stop("filename= is required for ",device," plot device")
    }
    else {
      stop("unsupported plot device '",device,"'")
    }
  }

  already.open <- FALSE # on-screen device may already be open
 
  if (.PLOT$id > 0 && .PLOT$id %in% dev.list()) {
    if (on.screen) {
      if (device == .PLOT$config$device # we can reuse open screen device unless parameters have changed
          && width == .PLOT$config$width
          && height == .PLOT$config$height
          && bg == .PLOT$config$bg
          && pointsize == .PLOT$config$pointsize) {
        dev.set(.PLOT$id) # in case user has switched to a different device
        .PLOT$device <- device # indicate device is open
        already.open <- TRUE
      }
    } else {
      zipfR.end.plot() # close active file plot before starting new device
    }
  }

  if (!already.open) {
    ## open new device of specified type with specified parameters
    png.res <- 150                        # default resolution for PNG files is 150 dpi
    switch(device,
           x11 = X11(width=width, height=height, bg=bg, pointsize=pointsize),
           quartz = quartz(width=width, height=height, bg=bg, pointsize=pointsize),
           png = png(filename=paste(filename, "png", sep="."), width=width*png.res, height=height*png.res, res=png.res, bg=bg, pointsize=pointsize),
           eps = postscript(file=paste(filename, "eps", sep="."), width=width, height=height, bg=bg, pointsize=pointsize, onefile=FALSE, horizontal=FALSE, paper="special"),
           pdf = pdf(file=paste(filename, "pdf", sep="."), width=width, height=height, bg=bg, pointsize=pointsize, onefile=FALSE, paper="special")
           )

    ## record information about active device in private .PLOT environment
    .PLOT$device <- device
    .PLOT$id <- dev.cur()
    .PLOT$config <- list(device=device, width=width, height=height, bg=bg, pointsize=pointsize)
  }
  
  ## initialize graphics parameters
  init.par <- zipfR.par("init.par")
  if (!is.null(init.par) && length(init.par) > 0) do.call(par, init.par)

  invisible(.PLOT$id)
}

zipfR.end.plot <- function (shutdown=FALSE)
{
  if (.PLOT$id <= 0 || .PLOT$device == "") stop("no graphics device active at the moment")
  if (.PLOT$device %in% c("x11", "quartz") && !shutdown) {
    ## don't close screen device when plot is finished (only when starting new plot with different geometry)
    .PLOT$device <- ""
  }
  else {
    dev.off(.PLOT$id)
    .PLOT$id <- 0
    .PLOT$device <- ""
    .PLOT$config <- list(device="", width=0, height=0, bg="", pointsize=0)
  }
}

zipfR.pick.device <- function(args=commandArgs())
{
  known.devices <- c("x11", "quartz", "eps", "pdf", "png")
  flags <- c(paste("-", known.devices, sep=""), paste("--", known.devices, sep=""))
  devices <- rep(known.devices, 2)
  found <- match(args, flags)           # either pointer to recognized flag or NA
  idx <- !is.na(found)                  # these are the recognized flags
  if (sum(idx) > 1)
    stop("multiple graphics devices specified (", paste(flags[found[idx]], collapse=", "),")")
  if (sum(idx) > 0) 
    zipfR.par(device=devices[found[idx]])
}

## private environment for information about current plot device
.PLOT <- new.env()

.PLOT$id <- 0
.PLOT$device <- ""
.PLOT$config <- list(device="", width=0, height=0, bg="", pointsize=0)
