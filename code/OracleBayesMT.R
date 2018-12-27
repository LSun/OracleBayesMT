MTLR.fun <- function (x, theta, s) {
  MTLR <- mapply(FUN = function (X, S) {mean(dnorm(X, theta[theta != 0], S))}, x, s) / dnorm(x, 0, s)
  return(MTLR)
}

olfdr.fun <- function (x, theta, s) {
  N <- length(theta)
  N0 <- sum(theta == 0)
  olfdr <- 1 / (1 + (N - N0) / N0 * MTLR.fun(x, theta, s))
  return(olfdr)
}

ODP.fun <- function (x, theta, s) {
  ODP <- sapply(x, FUN = function (X) {sum(dnorm(X, theta[theta != 0], s[theta != 0])) / sum(dnorm(X, 0, s[theta == 0]))})
  return(ODP)
}

FDP.fun <- function (q, qvalue, theta) {
  D <- sum(qvalue <= q)
  FD <- sum(theta[qvalue <= q] == 0)
  FDP <- FD / max(D, 1)
  return(FDP)
}
