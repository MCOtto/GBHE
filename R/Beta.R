#' Create a new beta distribution
#' 
#' Define a new beta distribution by its successes, \eqn{nu} and events,
#' \eqn{nu'} or its first two moments, the mean and standard deviation
#' 
#' If the \code{Mean} and \code{SD} are used, the method of moments is used to
#' calculate the beta distribution.
#' 
#' @param nu The number of successes, \eqn{nu}
#' @param nup The number of events, \eqn{nu'}
#' @param Mean Real value for the mean, between 0 and 1
#' @param SD Real value for the SD, \eqn{<p(1-p)}
#' @return A \code{Dist} object that has the parmeters of the Beta distribution
#' and also the mean and SD.  If the mean is either 0 or 1 or the SD=0, it will
#' be a trivial distribution with that mean.
#' @author Mark Otto, \email{Mark_Otto@FWS.Gov}
#' @seealso See Also as \code{\link{Gamma}} and \code{\link{Quant2Beta}}
#' @references \url{https://en.wikipedia.org/wiki/Beta_distribution}
#' @keywords distribution univar
#' @import dplyr
#' @examples
#' 
#' 	Beta(1,1)
#' 	Beta(Mean=0.5,SD=sqrt(1/12))  # Should be a \code{beta(1,1)}
#' 	
#' # Non-informed prior with variable p.  Pick any p
#'   p <- 0.2
#'   Beta(2*p,2*(1-p))
#' 
#' @export Beta
Beta <- function(nu = NULL,
                 nup = NULL,
                 Mean = nu / (nu + nup),
                 SD = sqrt(nu * nup / (nu + nup + 1)) / (nu + nup)) {
  nu <- if (is.null(nu)) {
    ifelse(SD == 0, ifelse(Mean == 0, 0, ifelse(Mean == 1, # is arbitrary
                                                Inf, # Mean is not 0 or 1 but is still trivial
                                                NA)), Mean * (Mean * (1 - Mean) / SD ^ 2 - 1))
  } else {
    nu
  }
  
  nup <- if (is.null(nup)) {
    ifelse(SD == 0,
           ifelse(Mean == 0, Inf, ifelse(Mean == 1, # is arbitrary
                                         0, # Mean is not 0 or 1 but is still trivial
                                         NA)),
           (1 - Mean) * (Mean * (1 - Mean) / SD ^ 2 - 1))
  } else {
    nup
  }
  
  tBeta <- bind_cols(nu = nu, nup = nup)
  attr(tBeta, "Dist") <- "Beta"
  attr(tBeta, "Mean") <- Mean
  attr(tBeta, "SD") <- SD
  class(tBeta) <- "Dist"
  return(tBeta)
}
