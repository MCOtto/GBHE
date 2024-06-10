##' Calculate capture-recapture likelihood
#'
#' @param parm numeric vector of length 2 for the front and rear observers
#' @param X numeric vector of double observer capture histories: x01 x10 x11 
#' @detail Need to determine the best N for the given probabilities because
#' N can only be an integer.
#' @return numeric log-likelihood
#' @export
#'
#' @examples
#' N <- 200
#' pf <- 0.95
#' pr <- 0.05
#' 
#' X.Smp <- round(c(
#'   x01 = 0,
#'   # N * (1 - pf) * pr,
#'   x10 = N * pf * (1 - pr),
#'   x11 = N * pf * pr
#' ))
#' parm <- c(pf = pr, pr = pf)
#' 
#' CRC(c(pf = 0.95, pr = 0.05), X = X.Smp)
#' 
#' Est <- optim(
#'   par = parm,
#'   fn = CRC,
#'   X = X.Smp,
#'   method =
#'     # "CG",
#'     # "Nelder-Mead",
#'     # "SANN",
#'     "L-BFGS-B",
#'   lower = c(0.0001, 0.0001),
#'   upper = c(0.9999, 0.9999),
#'   control = list(maxit = 10000
#'                  #    reltol = 1e-10
#'   ),
#'   hessian = TRUE
#' )
#' 
#' Est
#' 
#' pHat  <- Est$par[-3]
#' CRC(c(pHat, N = 500), X.Smp)
#' 
#' CRC.N <- function(x, p = pHat, X = X.Smp)
#'   CRC(c(pHat, N = x), X)
#' CRC.N(400)
#' 
#' curve(CRC.N(x, pHat, X.Smp), 50, 1000)
#' 
#' x <- seq(50, 1000, by = 5)
#' y <- vector("double", length = length(x))
#' 
#' for (i in 1:length(y)) {
#'   y[i] <- CRC.N(x[i])
#' }
#' 
#' plot(x, y, type = "l")
#' 
CRC <- function(parm, X = X.Smp) {
  pf <- parm[1]
  pr <- parm[2]
  
  # Generate possible N values.
  N <- (sum(X):(10 * sum(X) * ceiling(1 / (1 - (
    1 - pf
  ) * (
    1 - pr
  )))))
  
  LnLkhd <- dbinom(X[1], N, (1 - pf) * pr, log = TRUE) +
    dbinom(X[2], N, pf * (1 - pr), log = TRUE) +
    dbinom(X[3], N, pf * pr, log = TRUE)
  
  # Find the maximum log-likelihood and the N that generated it
  Idx <- min(which(LnLkhd == max(LnLkhd)), na.rm = TRUE)
  LnLkhd <- LnLkhd[Idx]
  # Set the N value in the overall environment, so it can be used.
  currentN <<- N[Idx]
  
  print(paste(pf, pr, currentN, LnLkhd))
  LnLkhd <- if (is.na(LnLkhd)) {
    -100000
  } else
    LnLkhd
  return(-LnLkhd)
}
