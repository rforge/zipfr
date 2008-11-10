##    Author: Stefan Evert
##   Purpose: Split complete word form list into random subcorpora
##   Created: Mon May 16 15:34:20 2005
##  Modified: Mon Jun 20 21:09:03 2005 (evert)   

## The purpose of this function is to simulate word frequency distributions
## for random subcorpora of a given corpus, by directly sampling from a
## type frequency list (.wfl file in lexstats) rather than the full token
## vector. Depending on the language and clean-up filters, this should reduce
## memory complexity by around two orders of magnitude and reduce processing
## time by a similar amount.

## Usage:  wfl.1 <- sample.wfl(wfl, N.1)
##
## Given a type frequency list wfl (= vector of frequencies) for a corpus of
## N tokens, the corresponding type frequency list for a random subcorpus of
## N.1 tokens is returned (wfl.1). This vector has the same length as wfl
## and will usually contain entries with frequency 0. You can easily compute
## the type frequency list of the remaining tokens (i.e. for the complement
## of the sample, with a total of N.2 = N - N.1 tokens) with the command
##    wfl.1C <- wfl - wfl.1
## Apply the function sample.wfl() iteratively to the complement in order
## to create incremental random samples, corresponding to a random reordering
## of the token sequence:
##    wfl.2 <- wfl.1 + sample.wfl(wfl.1C, N.1)
##    wfl.2C <- wfl - wfl.2
##    wfl.3 <- wfl.2 + sample.wfl(wfl.2C, N.1)
##    wfl.3C <- wfl - wfl.3
##    etc.
## Note that frequency spectra can easily be computed from the type frequency
## lists with the built-in table() function (you may want to remove 0 frequencies
## from the vectors first).  It is probably most efficient to apply sample.wfl
## to a type frequency vector sorted in descending order (but don't re-sort
## intermediate results for incremental samples!).

sample.wfl <- function (wfl, N.1) {
  N <- sum(wfl)
  V <- length(wfl)
  remain.sample <- N.1
  remain.total <- N
  wfl.1 <- rep(0, V)
  for (i in 1:V) {
    wfl.1[i] <- rhyper(1, remain.sample, remain.total-remain.sample, wfl[i])
    remain.sample <- remain.sample - wfl.1[i]
    remain.total <- remain.total - wfl[i]
  }
  return(wfl.1)
}

## Usage:  data <- sample.spectrum(spc, N.vec)
##
## Given a frequency spectrum spc (a data table with columns m and Vm),
## and a vector N.vec of sample sizes, this function simulates a randomized
## version of the corresponding corpus and computes frequency spectra for
## the sample sizes listed in N.vec.  The returned data structure is a list
## of lists for each sample size.  The components of this list are named
## according to the sample sizes (but converted to strings!) and are themselves
## lists with the following components:
##   N = sample size
##   V = vocabulary size
##   spc = frequency spectrum (data table corresponding to input spc)
sample.spectrum <- function (spc, N.vec) {
  V <- sum(spc$Vm)
  N <- sum(spc$m * spc$Vm)
  wfl <- rep(spc$m, spc$Vm)             # compute type frequency list
  N.vec <- sort(N.vec)                  # to be on the safe side
  out.of.range <- N.vec[N.vec > N | N.vec < 0]
  if (length(out.of.range) > 0) stop("requested sample sizes ", paste(out.of.range, collapse=", "), "are out of range")
  result <- list()
  remain <- sort(wfl, decreasing=TRUE)  # type frequency vector for remaining tokens
  sample <- rep(0, length(remain))      # parallel frequency vector for random sample
  delta.N <- c(N.vec[1], diff(N.vec))   # number of tokens to be added to sample in each step
  N.control <- numeric(0)               # compare N for generated samples with requested sample sizes
  for (delta in delta.N) {
    delta.wfl <- sample.wfl(remain, delta)
    sample <- sample + delta.wfl
    remain <- remain - delta.wfl
    x <- ordered(sample)
    y <- as.numeric(x)
    temp <- table(ordered(sample, exclude=0))    # exclude types with f=0 from spectrum calculation
    sample.spc <- data.frame(m=as.numeric(names(temp)), Vm=as.numeric(temp))
    colnames(sample.spc) <- c("m", "Vm")
    sample.V <- sum(sample.spc$Vm)
    sample.N <- sum(sample.spc$m * sample.spc$Vm)
    result <- c(result, list(list(N=sample.N, V=sample.V, spc=sample.spc)))
    N.control <- c(N.control, sample.N)
  }
  if (any(N.control != N.vec)) stop("inconsistencies detected in samples")
  names(result) <- N.vec
  return(result)
}

## Usage:  N <- spclist.N(spc.list)
##         V <- spclist.V(spc.list)
##         Vm <- spclist.Vm(spc.list, m)
##         spc <- spclist.spc(spc.list, N)
## 
## Convenience functions to extract vectors of N, V and Vm from list of frequency
## spectra as returned by sample.spectrum() function; and to extract the frequency
## spectrum for a specific sample size N
spclist.N <- function (spc.list) {
  sapply(spc.list, function (item) item$N)
}
spclist.V <- function (spc.list) {
  sapply(spc.list, function (item) item$V)
}
spclist.Vm <- function (spc.list, m) {
  sapply(spc.list, function (item) {
    spc <- item$spc
    idx <- spc$m == m
    if (sum(idx) == 1) spc$Vm[idx] else 0
  })
}
spclist.spc <- function (spc.list, N) {
  for (item in spc.list) {
    if (item$N == N) {
      return(item$spc)
    }
  }
  stop("requested sample size ", N, " not found in list")
}

## Usage:  V.vec <- binomint.EV(spc, N.vec)
##         Vm.vec <- binomint.EVm(spc, m, N.vec)
##
## Compute E[V] and E[Vm] for selected sample sizes using binomial interpolation
## according to Baayen (2001).  Takes a frequency spectrum spc and a vector of
## sample sizes N, and returns a parallel vector of E[V(N)] or E[Vm(N)].  This
## is a naive implementation of Baayen's equations (2.41) and (2.43), and ignores
## all problems of numerical accuracy.
binomint.EVm <- function (spc, m, N.vec) {
  N0 <- sum(spc$m * spc$Vm)
  out.of.range <- N.vec[N.vec > N0 | N.vec < 0]
  if (length(out.of.range) > 0) stop("requested sample sizes ", paste(out.of.range, collapse=", "), "are out of range")
  result <- numeric(0)
  for (N in N.vec) {
    idx <- spc$m >= m
    EVm <- sum( spc$Vm[idx] * dbinom(m, spc$m[idx], N/N0) )
    result <- append(result, EVm)
  }
  return(result)
}
binomint.EV <- function (spc, N.vec) {
  N0 <- sum(spc$m * spc$Vm)
  V0 <- sum(spc$Vm)
  out.of.range <- N.vec[N.vec > N0 | N.vec < 0]
  if (length(out.of.range) > 0) stop("requested sample sizes ", paste(out.of.range, collapse=", "), "are out of range")
  result <- numeric(0)
  for (N in N.vec) {
    EV <- V0 - sum( spc$Vm * (1 - N/N0) ** spc$m ) # this is critical ...
    result <- append(result, EV)
  }
  return(result)
}

