##
## Illustrations for talk on morphological productivity
##

library(zipfR)
source("R/utilities.R")

EL <- EvertLuedeling2001
EL.tfl <- lapply(EL, vec2tfl)
EL.spc <- lapply(EL.tfl, tfl2spc)
EL.vgc <- lapply(EL, vec2vgc, steps=1000, m.max=3)

## total token and type counts
EL.tfl$bar
EL.tfl$sam
EL.tfl$oes

## a first vocabulary growth curve
dev.new(width=8, height=5, noRStudioGD=TRUE)
WidePlot <- dev.cur()
par(mar=c(4, 4, 2, 1)+.1)

plot(EL.vgc$bar, EL.vgc$sam, EL.vgc$oes, legend=c("-bar", "-sam", "-ös"), main="Vocabulary Growth Curves")
dev.copy2pdf(file="plots/EL2001_vgc_bar_sam_oes.pdf", out.type="cairo")

plot(EL.vgc$bar, EL.vgc$sam, EL.vgc$oes, legend=c("-bar", "-sam", "-ös"), main="Vocabulary Growth Curves", log="xy")
dev.copy2pdf(file="plots/EL2001_vgc_bar_sam_oes_log.pdf", out.type="cairo")

## and frequency spectrum
dev.new(width=6, height=6, noRStudioGD=TRUE)
SqPlot <- dev.cur()
par(mar=c(4, 4, 2, 2)+.1)

plot(EL.spc$bar, main="Frequency spectrum of -bar", m.max=14)
dev.copy2pdf(file="plots/EL2001_spc_bar.pdf", out.type="cairo")

plot(EL.spc$sam, EL.spc$oes, ylim=c(0,20), m.max=25, legend=c("-sam", "-ös"), main="Frequency spectrum")
dev.copy2pdf(file="plots/EL2001_spc_sam_oes.pdf", out.type="cairo")

## extrapolate/interpolate with LNRE models
Models <- lapply(EL.spc[c("bar", "oes", "sam")], lnre, type="fzm")

dev.set(WidePlot)
plot(EL.vgc$bar, EL.vgc$sam, EL.vgc$oes, legend=c("-bar", "-sam", "-ös"), main="Vocabulary Growth Curves")
N <- seq(22500, 40000, length.out=50)
lines(N, EV(Models$sam, N), lty="22", lwd=4, col=six.colors[2])
N <- seq(4000, 40000, length.out=100)
lines(N, EV(Models$oes, N), lty="22", lwd=4, col=six.colors[3])
N <- seq(0, 40000, length.out=200)
lines(N, EV(Models$bar, N), lty="solid", lwd=4, col=six.colors[6])
dev.copy2pdf(file="plots/EL2001_vgc_bar_sam_oes_extrap.pdf", out.type="cairo")


## illustration of productivity measures
dev.set(WidePlot)
productivity.plot <- function (measure, ylim=NULL, xlog=FALSE, xmax=36000, Nmin=10,
                               measure.name=measure, main=measure.name) {
  xs <- lapply(EL.vgc[qw("bar sam oes")], N)
  if (measure == "theta") {
    ## Somers's theta not yet implemented in zipfR
    ys <- lapply(EL.vgc[qw("bar sam oes")], function (a) log(log(V(a))) / log(log(N(a))))
  } else {
    ys <- lapply(EL.vgc[qw("bar sam oes")], productivity.measures, measures=measure, data.frame=FALSE)
  }
  if (is.null(ylim)) ylim <- extendrange(do.call(c, ys))
  xlim <- if (xlog) c(100, xmax) else c(0, xmax)
  plot(1, 1, type="n", xlim=xlim, ylim=ylim, log=if (xlog) "x" else "", 
       xlab="N", ylab=measure.name, main=main)
  for (i in 1:3) {
    idx <- xs[[i]] >= Nmin
    lines(xs[[i]][idx], ys[[i]][idx], lwd=4, col=six.colors[i])
  }
  legend("topright", inset=.02, bg="white",
         legend=qw("-bar -sam -ös"), col=six.colors, lwd=4)
}

productivity.plot("TTR", ylim=c(0, .5), xlog=TRUE)
dev.copy2pdf(file="plots/EL2001_productivity_TTR.pdf", out.type="cairo")

productivity.plot("C", ylim=c(0, 1), xlog=TRUE, main="Herdan C")
dev.copy2pdf(file="plots/EL2001_productivity_C.pdf", out.type="cairo")

productivity.plot("k", ylim=c(0, 4), xlog=TRUE, main="Dugast k")
dev.copy2pdf(file="plots/EL2001_productivity_k.pdf", out.type="cairo")

productivity.plot("theta", ylim=c(0, 1.4), xlog=TRUE, measure.name=expression(theta), main=expression(paste(bold("Somers "), theta)))
dev.copy2pdf(file="plots/EL2001_productivity_theta.pdf", out.type="cairo")

productivity.plot("R", ylim=c(0, 6), xlog=TRUE, main="Guiraud R")
dev.copy2pdf(file="plots/EL2001_productivity_R.pdf", out.type="cairo")

productivity.plot("P", ylim=c(0, .3), xlog=TRUE, main="Baayen P")
dev.copy2pdf(file="plots/EL2001_productivity_P.pdf", out.type="cairo")

productivity.plot("H", ylim=c(0, 2000), xlog=TRUE, main="Honoré H")
dev.copy2pdf(file="plots/EL2001_productivity_H.pdf", out.type="cairo")

productivity.plot("S", ylim=c(0, .5), xlog=TRUE, main="Sichel S")
dev.copy2pdf(file="plots/EL2001_productivity_S.pdf", out.type="cairo")

productivity.plot("alpha2", ylim=c(0, 1), xlog=TRUE, measure.name=expression(alpha[2]), main=expression(paste(bold("Stefan's "), alpha[2])))
dev.copy2pdf(file="plots/EL2001_productivity_alpha2.pdf", out.type="cairo")
