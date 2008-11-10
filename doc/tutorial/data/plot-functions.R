##    Author: Stefan Evert
##   Purpose: Plot interpolation/extrapolation of vocabulary growth (from CL 2005)
##   Created: Mon Jun 20 21:04:44 2005
##  Modified: Wed Jun 14 14:15:06 2006 (severt)   

##
## vgc.mode(colour=FALSE, bw=TRUE)
##   line styles and colours for VGC plots are stored in a global variable (.vgcrc),
##   so we can easily switch between b/w and colour plots 
##
vgc.mode <- function (colour=FALSE, bw=!colour) {
  if (bw) {
    .vgcrc$lty <<- c("solid", "dashed", "12", "solid")
    .vgcrc$lwd <<- c(2,2,3,1)
    .vgcrc$col <<- c("grey30", "black", "black", "black")
  }
  else {
    .vgcrc$lty <<- c("solid", "solid", "33", "solid", "1232")
    .vgcrc$lwd <<- c(3,4,4,4,4)
    .vgcrc$col <<- c("black", "#FF0000", "#00AA00", "#4444FF", "black")
  }
}
.vgcrc <- list()
vgc.mode(bw=TRUE)


##
##  fake plot.vgc() function (modified version from CL 2005 plots)
##
vgc.plot <- function (N, V.obs, V.exp, N0=0, y.max=0,
                      main="Vocabulary Growth Curves", legend=NULL) {
  x.max <- max(N)
  if (missing(y.max)) y.max <- 1.2 * max(V.obs) # leaves some room for overestimation
  x.axis <- get.scale(x.max, "tokens")
  x.scale <- x.axis$scale
  x.label <- paste("N",x.axis$label, sep=" ")
  y.axis <- get.scale(y.max, "types")
  y.scale <- y.axis$scale
  y.label <- paste("V",y.axis$label, sep=" ")
  ## set up plot with axis labels and main title
  plot(0, 0, type="n", xlim=c(0, x.max/x.scale), ylim=c(0, y.max/y.scale),
       xlab=x.label, ylab=y.label, main=main)
  x <- N / x.scale
  y.obs <- V.obs / y.scale
  ## now plot expected VGCs (using the current line styles in .vgcrc)
  lty.vector <- .vgcrc$lty
  lwd.vector <- .vgcrc$lwd
  col.vector <- .vgcrc$col
  if (! is.list(V.exp)) V.exp <- list(V.exp)
  i.vec <- 1:length(V.exp)             # indices for models vector
  for (i in i.vec) {
    y.exp <- V.exp[[i]] / y.scale
    lines(x, y.exp, lty=lty.vector[i+1], lwd=lwd.vector[i+1], col=col.vector[i+1])
  }
  ## mark estimation size N0 by thin vertical line
  if (N0 > 0) abline(v=N0/x.scale, lwd=1)
  ## plot observed VGC last so that it overplots the other curves
  lines(x, y.obs, lty=lty.vector[1], lwd=lwd.vector[1], col=col.vector[1])
  ## finally, draw a legend box in bottom right corner (where it's least likely to obtrude)
  if (missing(legend)) legend <- c("observed", rep("expected", length(V.exp)))
  legend(x.max/x.scale, 0, xjust=1, yjust=0, bg="white", legend=legend,
         lty=lty.vector[c(1,1+i.vec)], lwd=lwd.vector[c(1,1+i.vec)], col=col.vector[c(1,1+i.vec)])
}

# adjust scale on x- or y-axis to M tokens/type or k tokens/types
get.scale <- function (values, unit="tokens") { 
  max.value <- max(values)
  if (max.value >= 1.5e6) {
    return(list(scale=1e6, label=paste("(M ",unit,")", sep="")))
  }
  else if (max.value >= 50e3) {
    return(list(scale=1e3, label=paste("(k ",unit,")", sep="")))
  }
  else {
    return(list(scale=1, label=paste("(",unit,")", sep="")))
  }
}

