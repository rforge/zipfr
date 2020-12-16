##
## Illustrations for tutorial
##

library(zipfR)
source("R/utilities.R")

dev.new(width=8, height=5, noRStudioGD=TRUE)
WidePlot <- dev.cur()
par(mar=c(4, 4, 2, 1)+.1, cex=1.2)

dev.new(width=6, height=6, noRStudioGD=TRUE)
SqPlot <- dev.cur()
par(mar=c(4, 4, 2, 2)+.1, cex=1.2)


##
## Basic notions and descriptive statistics based on artificial example data
## 

## adverbs in American academic writing (similar to Brown corpus)
adv <- qw("recently very not otherwise much very very merely not now very much merely not very")
cat(paste(adv, collapse=", "), "\n")
cat(paste(sprintf("\\TC{%s}", adv), collapse=", "), "\n")
cat(paste(sprintf("\\TC{%s}", unique(adv)), collapse=", "), "\n")

adv.tfl <- vec2tfl(adv)
N(adv.tfl)
V(adv.tfl)
adv.tfl

dev.set(SqPlot)
plot(adv.tfl, ylim=c(0, 10), main="Zipf ranking: adverbs")
dev.copy2pdf(file="plots/tutorial_tfl.pdf", out.type="cairo")

adv.spc <- tfl2spc(adv.tfl)
adv.spc

plot(adv.spc, m.max=7, ylim=c(0, 10), main="frequency spectrum: adverbs")
dev.copy2pdf(file="plots/tutorial_spc.pdf", out.type="cairo")

adv.vgc <- vec2vgc(adv, m.max=1)
adv.vgc

plot(adv.vgc, add.m=1, ylim=c(0, 10), main="vocabulary growth curve: adverbs")
legend("topleft", inset=.02, legend=expression(V(N), V[1](N)), col="black", lwd=c(4, 2))
dev.copy2pdf(file="plots/tutorial_vgc.pdf", out.type="cairo")


## compare with realistically sized data from Brown corpus
dev.set(SqPlot)

plot(Brown.tfl, log="", main="Zipf ranking: Brown corpus")
dev.copy(png, file="plots/tutorial_brown_tfl.png", width=6, height=6, units="in", res=300, type="cairo"); dev.off()

plot(Brown.tfl, log="y", main="Zipf ranking: Brown corpus")
dev.copy(png, file="plots/tutorial_brown_tfl_log.png", width=6, height=6, units="in", res=300, type="cairo"); dev.off()

plot(Brown.tfl, log="xy", main="Zipf ranking: Brown corpus")
dev.copy(png, file="plots/tutorial_brown_tfl_loglog.png", width=6, height=6, units="in", res=300, type="cairo"); dev.off()

plot(Brown.spc, main="frequency spectrum: Brown corpus")
dev.copy2pdf(file="plots/tutorial_brown_spc.pdf", out.type="cairo")

dev.set(WidePlot)
plot(Brown.emp.vgc, add.m=1, ylim=c(0, 50000), main="vocabulary growth curve: Brown corpus")
legend("topleft", inset=.02, legend=expression(V(N), V[1](N)), col="black", lwd=c(4, 2))
dev.copy2pdf(file="plots/tutorial_brown_vgc.pdf", out.type="cairo")


##
## First steps with zipfR
##

adv <- readLines("data/brown_adverbs.txt", encoding="UTF-8")

adv.tfl <- vec2tfl(adv)
adv.tfl

dev.set(WidePlot)
plot(adv.tfl, log="xy") # plot Zipf ranking
dev.copy2pdf(file="plots/brown_adverbs_tfl.pdf", out.type="cairo")

adv.spc <- tfl2spc(adv.tfl)
adv.spc

plot(adv.spc)
dev.copy2pdf(file="plots/brown_adverbs_spc.pdf", out.type="cairo")

adv.vgc <- vec2vgc(adv, m.max = 2)
adv.vgc

plot(adv.vgc, add.m = 1:2)
dev.copy2pdf(file="plots/brown_adverbs_vgc.pdf", out.type="cairo")


##
## Type density function of LNRE models
##

## estimate from observed spectrum so we can determine corresponding fZM and GIGP models
dick.fzm <- lnre("fzm", spc=Dickens.spc)
dick.fzm
dick.zm <- lnre("zm", spc=Dickens.spc)
dick.zm
dick.gigp <- lnre("gigp", spc=Dickens.spc)  
dick.gigp

## type density function
dev.set(WidePlot)

pi.vec <- 10^seq(-9, -2, .01)
plot(pi.vec, ltdlnre(dick.zm, pi.vec), type="l", col=six.colors[2], lwd=3, log="x", 
     xaxs="i", yaxs="i", ylim=c(0, 80000),
     main="Type density of LNRE model", xlab=expression("occurrence probability " * pi), ylab=expression("type density " * g(pi)))
legend("topright", inset=.02, legend="ZM", col=six.colors[2], lwd=3)
dev.copy2pdf(file="plots/lnre_tdf_zm.pdf", out.type="cairo")

plot(pi.vec, ltdlnre(dick.fzm, pi.vec), type="l", col=six.colors[2], lwd=3, log="x", 
     xaxs="i", yaxs="i", ylim=c(0, 80000),
     main="Type density of LNRE model", xlab=expression("occurrence probability " * pi), ylab=expression("type density " * g(pi)))
legend("topright", inset=.02, legend="fZM", col=six.colors[2], lwd=3)
dev.copy2pdf(file="plots/lnre_tdf_fzm.pdf", out.type="cairo")

plot(pi.vec, ltdlnre(dick.gigp, pi.vec), type="l", col=six.colors[2], lwd=3, log="x", 
     xaxs="i", yaxs="i", ylim=c(0, 80000),
     main="Type density of LNRE model", xlab=expression("occurrence probability " * pi), ylab=expression("type density " * g(pi)))
legend("topright", inset=.02, legend="GIGP", col=six.colors[2], lwd=3)
dev.copy2pdf(file="plots/lnre_tdf_gigp.pdf", out.type="cairo")

plot(pi.vec, ltdlnre(dick.fzm, pi.vec), type="l", col=six.colors[2], lwd=3, log="x", 
     xaxs="i", yaxs="i", ylim=c(0, 80000),
     main="Type density of LNRE model", xlab=expression("occurrence probability " * pi), ylab=expression("type density " * g(pi)))
lines(pi.vec, ltdlnre(dick.gigp, pi.vec), col=six.colors[3], lwd=3)
legend("topright", inset=.02, legend=qw("fZM GIGP"), col=six.colors[2:3], lwd=3)
dev.copy2pdf(file="plots/lnre_tdf_fzm_gigp.pdf", out.type="cairo")

## illustrate correspondence with true discrete probabilities of most frequent types;
## we need an artificial example with low density (so that types are "smeared out")
my.zm <- lnre("zm", alpha=.2, B=.5)
my.zm


pi.vec <- seq(0, .6, .001)
td.vec <- tdlnre(my.zm, pi.vec)
pd.vec <- dlnre(my.zm, pi.vec)

## type density, marking regions corresponding to the first four types
my.breaks <- tqlnre(my.zm, 0:10)
plot(0, 0, type="n", xlim=c(0, .55), ylim=c(0, 40), xaxs="i", yaxs="i",
     main="Type density as continuous approximation", xlab=expression("occurrence probability " * pi), ylab=expression("type density " * g(pi)))
for (i in 1:10) {
  j <- ((i-1) %% 5) + 2
  idx <- which(my.breaks[i] >= pi.vec & pi.vec >= my.breaks[i+1])
  polygon(c(pi.vec[idx], rev(pi.vec[idx])), c(td.vec[idx], rep(0, length(idx))), 
          col=paste0(six.colors[j], "44"), border=NA)
  if (i < 5) text(mean(pi.vec[idx]), 2, bquote(w[.(i)]), col=six.colors[j], cex=1.5)
}
lines(pi.vec, td.vec, col=six.colors[1], lwd=3)
dev.copy2pdf(file="plots/lnre_tdf_vs_prob.pdf", out.type="cairo")

plot(0, 0, type="n", xlim=c(0, .55), ylim=c(0, 4), xaxs="i", yaxs="i",
     main="Probability density vs. type probabilities", xlab=expression("occurrence probability " * pi), ylab=expression("probability density " * f(pi)))
for (i in 1:10) {
  j <- ((i-1) %% 5) + 2
  idx <- which(my.breaks[i] >= pi.vec & pi.vec >= my.breaks[i+1])
  polygon(c(pi.vec[idx], rev(pi.vec[idx])), c(pd.vec[idx], rep(0, length(idx))), 
          col=paste0(six.colors[j], "44"), border=NA)
  if (i < 5) {
    pi.i <- diff(plnre(my.zm, my.breaks[c(i+1, i)])) # probability of type i
    text(mean(pi.vec[idx]), .2, bquote(pi[.(i)] == .(sprintf(".%03.0f", 1000 * pi.i))), col=six.colors[j], 
         cex=1.5, srt=90, adj=c(0, 0.5))
  }
}
lines(pi.vec, pd.vec, col=six.colors[1], lwd=3)
dev.copy2pdf(file="plots/lnre_pdf_vs_prob.pdf", out.type="cairo")
