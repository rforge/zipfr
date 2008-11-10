##    Author: Stefan Evert
##   Purpose: Some illustrations for conference slides
##   Created: Mon Jun 12 06:09:36 2006
##  Modified: Wed Jun 14 13:06:52 2006 (severt)   

source('/Users/severt/UCS/System/R/lib/ucs.R')	 # in-place edit by ucs-config
ucs.library("lexstats")
ucs.library("zm")
ucs.library("fzm")

source("plot-functions.R")
vgc.mode(bw=FALSE)

x11()
par(cex=1.3, mar=c(4.2,4.2,2.1,2.1))

brown.200K.spc <- read.spectrum("brown.200K.spc")
N.200K <- brown.200K.spc$N
V.200K <- brown.200K.spc$V
spc.200K <- brown.200K.spc$spc

spectrum.plot(list(spc.200K), m.max=15,
              ylab=expression(V[m]), main="Frequency spectrum at N = 200k")
dev.copy2eps(file="img/brown-spc-200k.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
spectrum.plot(list(spc.200K), m.max=50, y.min=5, log=TRUE,
              ylab=expression(V[m]), main="Frequency spectrum at N = 200k")
dev.copy2eps(file="img/brown-spc-200k-log.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

model <- zm(N=N.200K, V=V.200K, spc=spc.200K)
print(model)
print(lnre.goodness.of.fit(model))

model.fzm <- fzm(N=N.200K, V=V.200K, spc=spc.200K)
print(model.fzm)
print(lnre.goodness.of.fit(model.fzm))

vgc.obs <- read.delim("brown.obsi")
N.obs <- vgc.obs$N
V.obs <- vgc.obs$V
V.exp <- EV(model, N.obs)

vgc.plot(N.obs, V.obs, V.exp, N0=N.200K)
dev.copy2eps(file="img/brown-vgc-zm.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

spc.200K.exp <- EVm(model, 1:15, 1e5)
spectrum.plot(list(spc.200K, spc.200K.exp), m.max=10, col=c("black","red"),
              legend=c("observed", "ZM model"), main="Frequency spectrum at N = 200k")
dev.copy2eps(file="img/brown-spc-zm-200k.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

brown.1M.spc <- read.spectrum("brown.1000K.spc")
spc.1M <- brown.1M.spc$spc
spc.1M.exp <- EVm(model, 1:15, 1e6)
spectrum.plot(list(spc.1M, spc.1M.exp), m.max=10, col=c("black", "red"),
              legend=c("observed", "ZM model"), main="Frequency spectrum at N = 1M")
dev.copy2eps(file="img/brown-spc-zm-1M.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")

## comparison of ZM and fZM predictions
V.exp.fzm <- EV(model.fzm, N.obs)
vgc.plot(N.obs, V.obs, list(V.exp, V.exp.fzm), N0=N.200K)
spc.200K.exp.fzm <- EVm(model.fzm, 1:15, 1e5)
dev.copy2eps(file="img/brown-vgc-zm-fzm.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
spectrum.plot(list(spc.200K, spc.200K.exp, spc.200K.exp.fzm), m.max=10, col=c("black","red","blue"),
              legend=c("observed", "ZM model", "fZM model"), main="Frequency spectrum at N = 200k")
spc.1M.exp.fzm <- EVm(model.fzm, 1:15, 1e6)
spectrum.plot(list(spc.1M, spc.1M.exp, spc.1M.exp.fzm), m.max=10, col=c("black", "red", "blue"),
              legend=c("observed", "ZM model", "fZM model"), main="Frequency spectrum at N = 1M")

## distribution function and type distribution of ZM model
model.sim <- zm(alpha=.8, N=1e6, V=2e5)
print(model.sim)
alpha <- model.sim$alpha
C <- model.sim$C
B <- ((1 - alpha) / C)^(1 / (1 - alpha)) # upper cutoff point for distribution

log.x <- seq(-10, -2, .05)
x <- 10^log.x
type.dist <- (C / alpha) / x - (1 - alpha) / (B * alpha)
dist.func <- ifelse(x >= B, 1, (C / (1 - alpha)) * x ^ (1 - alpha))
log.tdf <- C * log(10) * 10 ^ (-alpha * log.x)
log.pdf <- ifelse(x >= B, 0, C * log(10) * 10 ^ ((1 - alpha) * log.x))

par(xaxs="i", yaxs="i")
plot(x, type.dist/1e6, type="l", lwd=3, log="x", ylim=c(-.5,100.5), 
     xlab=expression(rho), ylab=expression(paste(G(rho), " [million types]")), main="Type distribution (ZM model)")
dev.copy2eps(file="img/zm-td.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
plot(x, dist.func, type="l", lwd=3, log="x", ylim=c(-.005,1.005), 
     xlab=expression(rho), ylab=expression(F(rho)), main="Distribution function (ZM model)")
dev.copy2eps(file="img/zm-df.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
plot(x, log.tdf/1e6, type="l", lwd=3, log="x", ylim=c(-.05, 11),
     xlab=expression(pi), ylab=expression(paste(g(pi), " [", log[10], "-transformed, million types]")), main="Type density (ZM model)")
dev.copy2eps(file="img/zm-tdf.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
plot(x, log.pdf, type="l", lwd=3, log="x", ylim=c(-.005, 0.5),
     xlab=expression(pi), ylab=expression(paste(f(pi), " [", log[10], "-transformed]")), main="Probability density (ZM model)")
dev.copy2eps(file="img/zm-pdf.eps", onefile=FALSE, bg="white", horizontal=FALSE, paper="special")
