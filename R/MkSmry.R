#' Make a summary
#'
#' @param Smry RV or MCMC variable
#' 
#' @details Summarizes an RV or MCMC variable.  
#'  Use instead of the summary.rv or summary.mcmc.
#'
#' @return dataframe of mean, SE, and quantiles
#' @import rv
#' @export
#'
MkSmry <- function(Smry) {
  if (class(Smry) == "rv") {
    Tot <- sum(Smry)
    Sim <- c(Smry, Tot)
    Smry <- RVSmry(Names = cStr, Sim, probs = Probs)
    Smry <- data.frame(Smry, row.names = 1)
  } else {
    Tot <- c(sum(Smry$Mean), sqrt(sum(Smry$SD ^ 2)))
    Smry <- rbind(Smry, Tot)
    Q <- t(apply(Smry, 1, function(x)
      qnorm(Probs, x[1], x[2])))
    Smry <- data.frame(Smry, Q, row.names = cStr)
  }
  colnames(Smry) <- c("Mean", "SD", "Median", "LC95", "UC95")
  return(Smry)
}
