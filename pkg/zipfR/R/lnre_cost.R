##
## internal: various cost functions for parameter estimation of LNRE models
##

## common signature: lnre.cost(model, spc, m.max=15)

## LIKELIHOOD / GOODNESS-OF-FIT (lnre.cost.gof)
## compute goodness-of-fit statistic from multivariate chi-squared test
##  - under the multivariate normal approximation, likelihood is a monotonic function of this statistic
##  - no automatic adjustment of m.max to avoid inaccurate normal approximation for small E[Vm] and V[Vm]
##  - see lnre.goodness.of.fit for explanation of the computation and references
lnre.cost.gof <- function (model, spc, m.max=15)
{
  N <- N(spc)
  V <- V(spc)
  m.vec <- seq_len(m.max)
  Vm <- Vm(spc, m.vec)
  
  E.Vm <- EVm(model, m.vec, N)
  E.V <- EV(model, N)
  E.Vm.2N <- EVm(model, seq_len(2 * m.max), 2 * N)
  E.V.2N <- EV(model, 2 * N)
 
  err.vec <- c(V, Vm) - c(E.V, E.Vm)
  cov.Vm.Vk <- diag(E.Vm, nrow=m.max) -
    outer(m.vec, m.vec, function (m, k) choose(m+k, m) * E.Vm.2N[m+k] / 2^(m+k))
  cov.Vm.V <- E.Vm.2N[m.vec] / 2^(1:m.max)
  R <- rbind(c(VV(model, N), cov.Vm.V),
             cbind(cov.Vm.V, cov.Vm.Vk))
  t(err.vec) %*% solve(R, err.vec)
}


## CHI-SQUARED COST FUNCTION (lnre.cost.chisq)
## compute standard chi-squared statistic for observed data wrt. model
##  - observations are V_1, ..., V_m.max and V+ = V - (V_1 + ... + V_m.max)
##  - statistic assumes independence of these random variables
##    (!= multivariate chi-squared used for g.o.f.)
##  - means and standard deviations of variables are obtained from model
##  - s.d. is clamped to >= 3 to avoid scaling up small differences
##    for which normal approximation is unsuitable
lnre.cost.chisq <- function (model, spc, m.max=15)
{
  N <- N(spc)
  
  E.Vm <- EVm(model, 1:m.max, N)        # expected values according to LNRE model
  E.V <- EV(model, N)
  E.Vplus <- E.V - sum(E.Vm)

  V.Vm <- VVm(model, 1:m.max, N)        # variances according to LNRE model
  V.V <- VV(model, N)
  V.Vplus <- V.V - sum(V.Vm)            # since we assume independence of the V_k

  V.Vm <- pmax(V.Vm, 9)                 # clamp variance to >= 9 (i.e., s.d. >= 3)
  V.Vplus <- max(V.Vplus, 9)


  O.Vm <- Vm(spc, 1:m.max)              # observed values
  O.V <- V(spc)
  O.Vplus <- O.V - sum(O.Vm)

  X2 <- sum( (O.Vm - E.Vm)^2 / V.Vm ) + (O.Vplus - E.Vplus)^2 / V.Vplus
  X2
}

## LINEAR COST FUNCTION (lnre.cost.linear)
## sum of absolute values of differences between observed and expected values
##  - for vocabulary size V and spectrum elements V_1, ... , V_m.max
lnre.cost.linear <- function (model, spc, m.max=15)
{
  N <- N(spc)
  
  E.Vm <- EVm(model, 1:m.max, N)
  E.V <- EV(model, N)

  O.Vm <- Vm(spc, 1:m.max)
  O.V <- V(spc)

  abs(E.V - O.V) + sum( abs(E.Vm - O.Vm) )
}

## SMOOTH LINEAR COST FUNCTION (lnre.cost.smooth.linear)
## smoothed version of linear cost function which avoids kink of abs(x) at x=0
##  - sum of sqrt(1 + (O-E)^2) (min. cost = 1, approximates abs(O-E) for larger differences)
##  - for vocabulary size V and spectrum elements V_1, ... , V_m.max
lnre.cost.smooth.linear <- function (model, spc, m.max=15)
{
  N <- N(spc)
  
  E.Vm <- EVm(model, 1:m.max, N)
  E.V <- EV(model, N)

  O.Vm <- Vm(spc, 1:m.max)
  O.V <- V(spc)

  sqrt(1 + (E.V - O.V)^2) + sum( sqrt(1 + (E.Vm - O.Vm)^2) )
}

## MEAN SQUARED ERROR (MSE) COST FUNCTION (lnre.cost.mse)
## sum (or average) of squared differences between observed and expected values
##  - for vocabulary size V and spectrum elements V_1, ... , V_m.max
lnre.cost.mse <- function (model, spc, m.max=15)
{
  N <- N(spc)
  
  E.Vm <- EVm(model, 1:m.max, N)
  E.V <- EV(model, N)

  O.Vm <- Vm(spc, 1:m.max)
  O.V <- V(spc)

  ( (E.V - O.V)^2 + sum((E.Vm - O.Vm)^2) ) / (1 + m.max)
}


