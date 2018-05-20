##
## Examples and illustrations based on morphological productivity data of Evert & Luedeling (2001)
##

library(zipfR)

EL <- EvertLuedeling2001
EL.tfl <- lapply(EL, vec2tfl)
EL.spc <- lapply(EL.tfl, tfl2spc)
EL.vgc <- lapply(EL, vec2vgc, steps=500, m.max=3)

sapply(EL.spc, function (x) c(N=N(x), V=V(x)))

sapply(EL.spc, function (x) lnre("fzm", spc=x)$gof)

x <- lnre("fzm", spc=EL.spc$lich, method="Custom")
x$gof

lnre("fzm", spc=EL.spc$bar, method="Custom", runs=1)
lnre("fzm", spc=EL.spc$bar, cost="exact", runs=5)

lnre("fzm", spc=EL.spc$lich, method="Custom")
lnre("fzm", spc=EL.spc$lich, method="Nelder-Mead", runs=3, cost="gof")

res <- lnre("fzm", spc=EL.spc$lein, bootstrap=200)
confint(res, "B", plot=TRUE, method="empirical")

res <- lnre("zm", spc=EL.spc$lein, bootstrap=200)
confint(res, "alpha", plot=TRUE, method="empirical")

x <- sapply(res$bootstrap, function (M.) M.$param$alpha)
hist(x, breaks="Sturges")

res <- lnre("fzm", spc=EL.spc$oes)
lnre.bootstrap(res, N(res), lnre, function (x) x$gof, replicates=20, verbose=TRUE, type="fzm", m.max=10)

res.list <- lnre.bootstrap(res, N(res), lnre, identity, replicates=2, verbose=TRUE, type="fzm", m.max=10, simplify=FALSE)
length(res.list)
str(res.list[[2]])
attributes(res.list)

model <- lnre("fzm", spc=BrownAdj.spc, bootstrap=20)
confint(model, "alpha", method="mad") # Zipf slope
confint(model, "S", method="mad")     # population diversity
confint(model, "S") # default normal approximation works well in this case
