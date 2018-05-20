##
## Parameter estimation & cost surfaces
##

library(zipfR)
source("R/utilities.R")

dev.new(width=6, height=6, noRStudioGD=TRUE)
par(mar=c(4, 4, 2, 2)+.1, cex=1)


## plot minimization surface for different cost functions
## (using ZM model so we have a two-dimensional cost function)
compute.cost <- function (spc, cost, alpha=alpha.vec, logB=logB.vec, m.max=15) {
  cost.mat <- outer(alpha, logB, function (alpha, logB) {
    sapply(seq_along(alpha), function (i) {
      model <- lnre("zm", alpha=alpha[i], B=10^logB[i])
      cost(model, spc, m.max=m.max)
    })
  })
}

cost.plot <- function(spc, alpha.lim=c(0,1), logB.lim=c(-4,1), cost=zipfR:::lnre.cost.mse, m.max=15, steps=150, steps.alpha=steps, steps.logB=steps, log.cost=TRUE, bdry=NULL, mark=TRUE, ...) {
  alpha.vec <- seq(alpha.lim[1], alpha.lim[2], length.out=steps.alpha)
  alpha.vec <- alpha.vec[alpha.vec > 0 & alpha.vec < 1]
  logB.vec <- seq(logB.lim[1], logB.lim[2], length.out=steps.logB)
  cost.mat <- compute.cost(spc, cost, alpha.vec, logB.vec, m.max)
  if (log.cost) cost.mat <- log10(cost.mat)
  image(alpha.vec, logB.vec, cost.mat, col=terrain.colors(255, alpha=.7),
        xlim=alpha.lim, ylim=logB.lim, xlab=expression(alpha), ylab=expression(log[10](B)), ...)
  contour(alpha.vec, logB.vec, cost.mat, add=TRUE)
  if (!is.null(bdry)) contour(alpha.vec, logB.vec, cost.mat, levels=bdry, lwd=4, col=six.colors[2], drawlabels=FALSE, add=TRUE)
  if (mark) {
    idx <- which(cost.mat == min(cost.mat), arr.ind=TRUE)
    x.min <- alpha.vec[idx[, 1]]
    y.min <- logB.vec[idx[, 2]]
    points(x.min, y.min, pch=8, col="#DD0000")
    text(x.min, y.min, pos=3, offset=2, labels=sprintf("(%.2f, %.2f)", x.min, y.min), col="#DD0000")
  }
  ## persp(alpha.vec, logB.vec, log(cost.mat), theta=-45, phi=-30, shade=.7, col="#AAFFAA", ticktype="detailed", xlab=expression(alpha), ylab=expression(log10(B)), border=NA)
}

EL.spc <- lapply(EvertLuedeling2001, vec2spc)

## plot minimization surfaces for different affixes and cost functions
cost.funcs <- list(linear=zipfR:::lnre.cost.linear, mse=zipfR:::lnre.cost.mse, "chi-square"=zipfR:::lnre.cost.chisq, MLE=zipfR:::lnre.cost.gof)
for (A. in qw("bar chen lein sam")) {
  for (C. in names(cost.funcs)) {
    for (M. in c(5, 10)) {
      if (M. == 10 && A. == "lein") next
      spc. <- EL.spc[[A.]]
      title. <- sprintf("-%s (N=%d, V=%d) | cost: %s (m = 1…%d)", A., N(spc.), V(spc.), C., M.)
      cost.plot(spc., cost=cost.funcs[[C.]], main=title., m.max=M.)
      dev.copy2pdf(file=sprintf("plots/cost_%s_%s_m%02d.pdf", A., C., M.), out.type="cairo")
    }
  }
}

## which parameter sets are consistent with the observed data?
## --> visualize p-value of goodness-of-fit test (with suitable negative log transform)
log.pv <- function (p) { lp <- -log10(p); ifelse(lp > 10, 10 + .8 * tanh(lp - 10), lp) } # soft cutoff after p = 1e-10
pv.cost <- function (model, spc, m.max) log.pv(lnre.goodness.of.fit(model, spc, m.max=m.max, n.estimated=0)$p)

cost.plot(EL.spc$lein, cost=pv.cost, mark=FALSE, log.cost=FALSE, bdry=-log10(.05), m.max=5, 
          main=sprintf("-lein (N=%d, V=%d) | goodness-of-fit (m = 1…5)", N(EL.spc$lein), V(EL.spc$lein)))
legend("bottomright", inset=.02, col=six.colors[2], lwd=4, legend=expression(p > .05))
dev.copy2pdf(file=sprintf("plots/gof_lein_m05.pdf"), out.type="cairo")

cost.plot(EL.spc$chen, cost=pv.cost, mark=FALSE, log.cost=FALSE, bdry=-log10(.05), m.max=5, 
          main=sprintf("-chen (N=%d, V=%d) | goodness-of-fit (m = 1…5)", N(EL.spc$chen), V(EL.spc$chen)))
legend("bottomright", inset=.02, col=six.colors[2], lwd=4, legend=expression(p > .05))
dev.copy2pdf(file=sprintf("plots/gof_chen_m05.pdf"), out.type="cairo")

cost.plot(EL.spc$bar, cost=pv.cost, mark=FALSE, log.cost=FALSE, bdry=-log10(.05), m.max=5, 
          main=sprintf("-bar (N=%d, V=%d) | goodness-of-fit (m = 1…5)", N(EL.spc$bar), V(EL.spc$bar)))
legend("bottomright", inset=.02, col=six.colors[2], lwd=4, legend=expression(p > .05))
dev.copy2pdf(file=sprintf("plots/gof_bar_m05.pdf"), out.type="cairo")

cost.plot(EL.spc$sam, cost=pv.cost, mark=FALSE, log.cost=FALSE, bdry=-log10(.05), m.max=5, 
          main=sprintf("-sam (N=%d, V=%d) | goodness-of-fit (m = 1…5)", N(EL.spc$sam), V(EL.spc$sam)))
legend("bottomright", inset=.02, col=six.colors[2], lwd=4, legend=expression(p > .05))
dev.copy2pdf(file=sprintf("plots/gof_sam_m05.pdf"), out.type="cairo")

## more sensible for samples from a suitable LNRE population (so g-o-f tests parameters rather than fit)
## --> ideal case: expected frequency spectrum
my.zm <- lnre("zm", alpha=.5, B=.1) # roughly similar to -lein distribution

for (N. in c(1e6, 50000, 10000, 5000, 2000, 1000, 500, 200)) {
  my.spc <- lnre.spc(my.zm, N=N., m.max=15)
  cost.plot(my.spc, cost=pv.cost, mark=FALSE, log.cost=FALSE, bdry=-log10(.05), m.max=5,
            main=bquote("ZM sample (" * list(alpha == .5, B == .1) * ") with " * N == .(N.)))
  legend("bottomright", inset=.02, col=six.colors[2], lwd=4, legend=expression(p > .05))
  dev.copy2pdf(file=sprintf("plots/gof_zm_spc_%d.pdf", N.), out.type="cairo")
}
