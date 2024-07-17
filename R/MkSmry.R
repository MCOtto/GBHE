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
    Tot1 <- sum(Smry[1:nStr])
    Tot2 <- sum(Smry[nStr + 1:nStr])
    Sim <- c(Smry[1:nStr], Tot1, Smry[nStr + 1:nStr], Tot2)
    Smry <- RVSmry(Series = Sim, probs = Probs)
    Smry <- bind_cols(YrStrNames, Smry)
  } else {
    Tot1 <- c(sum(Smry[1:nStr, "Mean"]), sqrt(sum(Smry[1:nStr, "SD"] ^ 2)))
    Tot2 <- c(sum(Smry[nStr + 1:nStr, "Mean"]), sqrt(sum(Smry[nStr + 1:nStr, "SD"] ^ 2)))
    # We assume the quantiles are gamma, more realistic than normal
    TotGamma1 <- Gamma(Mean = Tot1[1], SD = Tot1[2])
    TotGamma2 <- Gamma(Mean = Tot2[1], SD = Tot2[2])
    
    Smry <- bind_cols(YrStrNames, bind_rows(Smry[1:nStr, ], c(
      TotGamma1,
      qgamma(
        p = Probs,
        shape = TotGamma1[1, 1],
        rate = TotGamma1[1, 2]
      )
    ), Smry[nStr + 1:nStr], c(
      TotGamma2,
      qgamma(
        p = Probs,
        shape = TotGamma2[1, 1],
        rate = TotGamma2[1, 2]
      )
    )))
  }
  colnames(Smry) <- c("Year", "Stratum", "Mean", "SD", "Median", "LC95", "UC95")
  return(Smry)
}
