## (mostly) internal function: draw a legend box in one of the four corners of a plot
## 1 = top left, 2 = top right, 3 = bottom right, 4 = bottom left
zipfR.legend <- function(corner, margin.x=.05, margin.y=margin.x,
                         legend=NULL, bg="white", ...)
{
  if (missing(legend)) stop("legend text missing")
  
  ## determine coordinates of view port and log/linear scales
  xlim <- par("usr")[1:2]
  ylim <- par("usr")[3:4]
  x.log <- par("xlog")
  y.log <- par("ylog")

  ## calculate relative and absolute position of outer corner of the legend box
  offset.x <- switch(corner, margin.x, 1-margin.x, 1-margin.x, margin.x) 
  offset.y <- switch(corner, 1-margin.y, 1-margin.y, margin.y, margin.y)
  x <- xlim[1] + diff(xlim) * offset.x
  y <- ylim[1] + diff(ylim) * offset.y

  ## translate coordinate to logarithmic scale if necessary
  if (x.log) x <- 10^x
  if (y.log) y <- 10^y

  ## alignment of legend box; (x,y) are coordinates of "outer" corner of the box
  xjust <- switch(corner, 0, 1, 1, 0)
  yjust <- switch(corner, 1, 1, 0, 0)
  legend(x=x, y=y, xjust=xjust, yjust=yjust, legend=legend, bg=bg, ...)
 }
