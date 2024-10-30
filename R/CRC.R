##' Calculate capture-recapture likelihood
#'
#' @param parm numeric vector of length 2 * number of categories for the front and rear observers
#' @param X numeric vector or matrix of double observer capture histories: x01 x10 x11 
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
  Parm <- matrix(parm, nrow = 2)
  nCat <- ncol(Parm)
  cFront <- 1
  cRear <- 2
  
  LnLkhd <- double(nCat)
  currentN <<- LnLkhd
  
  for (iCat in 1:nCat) {
    # Generate possible N values.
    sumXi <- sum(X[, iCat])
    N <- (sumXi:(10 * sumXi * ceiling(1 / (
      1 - (1 - Parm[cFront, iCat]) * (1 - Parm[cRear, iCat])
    ))))
    
    # Calculates a binomial likelihood for each N
    LnLkhdi <- dbinom(X[1, iCat], N, (1 - Parm[cFront, iCat]) * Parm[cRear, iCat], log = TRUE) +
      dbinom(X[2, iCat], N, Parm[cFront, iCat] * (1 - Parm[cRear, iCat]), log = TRUE) +
      dbinom(X[3, iCat], N, Parm[cFront, iCat] * Parm[cRear, iCat], log = TRUE)
    
    # Find the maximum log-likelihood and the N that generated it
    Idx <- min(which(LnLkhdi == max(LnLkhdi)), na.rm = TRUE)
    MxLnLkhdi <- LnLkhdi[Idx]
    
    # Set the N value in the overall environment, so it can be used.
    currentN[iCat] <<- N[Idx]
    
    if (is.na(MxLnLkhdi)) {
      LnLkhd <- -100000
      break
    } else
      LnLkhd[iCat] <- MxLnLkhdi
  }
  
  return(-sum(LnLkhd))
}
