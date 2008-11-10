##    Author: Stefan Evert
##   Purpose: Some illustrations for the tutorial
##   Created: Mon Jun 12 06:09:36 2006
##  Modified: Wed Jun 14 14:53:57 2006 (severt)   

source('/Users/severt/UCS/System/R/lib/ucs.R')	 # in-place edit by ucs-config
ucs.library("lexstats")
ucs.library("zm")
ucs.library("fzm")

library("zipfR")

source("plot-functions.R")
vgc.mode(bw=TRUE)

source("wflsplit.R")

x11()
par(cex=1.3, mar=c(4.2,4.2,2.1,2.1))

ri.spc <- read.spc("ri.in.rep.spc")
print(spc.N(ri.spc))
print(spc.V(ri.spc))

## use plot.default() to get "plain" plot of spectrum data
plot.default(ri.spc, main="Frequency Spectrum")
dev.copy2eps(file="img/ri-spc-default.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

## the plot.spc() method tries to do something more sensible:
## a) a barplot with linear scale, by default up to m=15
spectrum.plot(list(spc.vector(ri.spc, 1, 15)), main="Frequency spectrum")
dev.copy2eps(file="img/ri-spc-linear.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

## b) a logarithmic plot of the spectrum up to m=50 if log=TRUE (or specify the axes you want to be logarithmic)
spectrum.plot(list(spc.vector(ri.spc, 1, 50)), y.min=1, log="x", main="Frequency spectrum (logarithmic scale)")
dev.copy2eps(file="img/ri-spc-log-x.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
## this may not work for you because of a bug in UCS/R - to fix it, you need to locate the definition
## of spectrum.plot() in UCS/System/R/lib/lexstats.R and move the line
##   if (is.character(log)) { log.type <- log; log <- TRUE } else log.type <- "y"
## to the start of the function body


ri.emp.vgc <- read.delim("ri.in.rep.order.vgc")
ri.N <- ri.emp.vgc$N
ri.V <- ri.emp.vgc$V
ri.V1 <- ri.emp.vgc$V1

## vocabulary growth curves can only be done separately for V and V1 (in zipfR),
## but my current fake implementation can only do comparisons of exactly two curves,
## so please try to make do with that in the tutorial
vgc.plot(ri.N, ri.V, ri.V1, legend=expression(V,V[1]))
dev.copy2eps(file="img/ri-vgc-v-v1.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

## binomial interpolation vs. observed VGC
ri.bin.EV <- binomint.EV(ri.spc, ri.N)
ri.bin.EV1 <- binomint.EVm(ri.spc, 1, ri.N)
vgc.plot(ri.N, ri.V, ri.bin.EV, legend=c("observed", "interpolation"))
dev.copy2eps(file="img/ri-vgc-binomial.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

## fZM model for ri- spectrum
ri.fzm <- fzm(N=spc.N(ri.spc), V=spc.V(ri.spc), spc=spc.vector(ri.spc, 1, 15))
print(ri.fzm)  # will be summary(ri.fzm) in zipfR and include g-o-f as well
lnre.goodness.of.fit(ri.fzm)

## compare expected and observed spectrum
spectrum.plot(list(spc.vector(ri.spc,1,15), EVm(ri.fzm, 1:15, spc.N(ri.spc))), legend=c("observed", "fZM"), main="Frequency spectrum")
dev.copy2eps(file="img/ri-spc-fzm.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

## as well as expected VGC including extrapolation
fzm.sample.sizes <- c(ri.N, (14000:28000) * 100)
ri.fzm.EV <- EV(ri.fzm, fzm.sample.sizes)
ri.V.padded <- c(ri.V, rep(NA, 14001))
vgc.plot(fzm.sample.sizes, ri.V.padded, ri.fzm.EV, y.max=1600, N0=spc.N(ri.spc))
dev.copy2eps(file="img/ri-vgc-fzm.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
## phew, that was a bit tricky

## now for testing extrapolation quality (we can just use ri.sub.spc() to sample a random spectrum for N=700k tokens)
ri.sub.spc <- sample.spectrum(ri.spc, 700000)[[1]]$spc
class(ri.sub.spc) <- c("zipfR.spc", "data.frame") # cheat :-)
ri.sub.fzm <- fzm(N=spc.N(ri.sub.spc), V=spc.V(ri.sub.spc), spc=spc.vector(ri.sub.spc,1,15))
print(ri.sub.fzm)
lnre.goodness.of.fit(ri.sub.fzm)

vgc.plot(ri.N, ri.bin.EV, EV(ri.sub.fzm, ri.N), N0=spc.N(ri.sub.spc))
dev.copy2eps(file="img/ri-vgc-fzm-extrapolation-quality.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
## don't know why you're complaining about extrapolation quality -- perhaps Perl didn't randomize as good as R? ;-)
