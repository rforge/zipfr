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

