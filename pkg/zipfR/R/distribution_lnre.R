ltdlnre.lnre <- function (model, x, base=10, log.x=FALSE, ...)
{
  if (! inherits(model, "lnre")) stop("first argument must be object of class 'lnre'")

  if (log.x) x <- base ^ x
  log(base) * x * tdlnre(model, x)
}


ldlnre.lnre <- function (model, x, base=10, log.x=FALSE, ...)
{
  if (! inherits(model, "lnre")) stop("first argument must be object of class 'lnre'")

  if (log.x) x <- base ^ x
  log(base) * x * dlnre(model, x)
}


rlnre.lnre <- function (model, n, what=c("tokens", "tfl"), debug=FALSE, ...)
{
  if (! inherits(model, "lnre")) stop("first argument must be object of class 'lnre'")
  what <- match.arg(what)
  if (length(n) != 1) n <- length(n)

  if (n == 0) {
    if (what == "tokens") factor() else vec2tfl(factor()) # return empty sample
  }
  else {
    if (what == "tokens") {
      ## generate token vector
      x <- runif(n)
      y <- 1 + floor( tplnre(model, qlnre(model, x)) ) # 1 + floor( G( F^-1(x) ) )
      factor(y) # type numbers are converted to strings
    }
    else {
      ## generate type-frequency list directly as mixed sample:

      ## A) types with expected frequency >= 1: sample from multinomial distribution
      V.multi <- ceiling(tplnre(model, 1 / n))         # number of types with pi >= 1/n i.e. E[f] >= 1
      pi0 <- tqlnre(model, V.multi)                    # corresponding probability threshold
      p.multi <- plnre(model, pi0, lower.tail=FALSE)   # total probability mass P of these types
      n.multi <- rbinom(1, n, p.multi)                 # number of tokens to be sampled
      if (n.multi < 1) {
        if (debug) cat(sprintf(" - falling back on sampling %d tokens\n", n))
        return(vec2tfl(rlnre(model, n, what="tokens", ...))) # multinomial part is empty
      }
      pi.multi <- -diff(plnre(model, tqlnre(model, 0:V.multi))) # F( G^-1(k) ) = 1 - (pi_1 + pi_2 + ... + pi_k)
      ## TODO: compute such differences with improved accuracy (avoiding cancellation)
      ##       (note that 1-F already gives more accurate differences than F for small values of pi_k)
      if (debug) cat(sprintf(" - multinomial sample N = %d, V = %d, P = %.5f = %.5f\n", n.multi, V.multi, p.multi, sum(pi.multi)))
      f.multi <- drop(rmultinom(1, n.multi, pi.multi))
      
      ## B) types with expected frequency <= 1: sample token vector, then count
      n.tokens <- n - n.multi
      p.tokens <- plnre(model, pi0)                    # total probability mass 1-P of types from which tokens are sampled
      if (debug) cat(sprintf(" - token sample N = %d, P = %.5f\n", n.tokens, p.tokens))
      x <- runif(n.tokens, 0, p.tokens)                # sample from remaining range (0, 1-P) of distribution function F
      y <- 1 + floor( tplnre(model, qlnre(model, x)) ) # guarantees that sampled type IDs are > n.multi
      f.tokens <- table(y)
      
      ## combine A) and B) into type-frequency list
      tfl(f=c(f.multi, f.tokens), type=c(seq_len(V.multi), names(f.tokens)), delete.zeros=TRUE)
    }
  }
}
