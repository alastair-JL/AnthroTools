#' cultFST
#'
#' Calculate cultural FST betwen two groups.
#' @usage cultFST(ni, nj, xi, xj)
#' @param ni This is the umber of individuals in group i.
#' @param nj This is the number of individuals in group j.
#' @param xi This is the frequency of target trait in group i.
#' @param xj This is the fFrequency of target train in group j.
#' @return This function returns the Cultural FST value for two groups. If all of the variance lies between the groups, FST = 1. If all of the variance exists within groups, FST = 0.
#' @keywords cultFST
#' @note This function is only to be applied to two groups. Use the fstmatrix() function for multiple groups.  
#' @export
#' @examples
#' # Miscellaneous examples
#' cultFST(100, 100, 100, 0) # n = 100 for both groups, i exhibits 100, j exhibits 0
#' cultFST(100, 100, 50, 50) # n = 100 for both groups, i exhibits 50, j exhibits 50
#' cultFST(100, 100, 0, 100) # n = 100 for both groups, i exhibits 0, j exhibits 100
#' cultFST(100, 100, 46, 2) # n = 100 for both groups, i exhibits 46, j exhibits 2

#' # Plot values of CF_ST across levels of xj and xi.
#' par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
#' plot(NA, xlim = c(0, 100), ylim = c(0, 1),
#'      xlab = expression(italic('x'['j'])), ylab = expression(italic('CF'['ST'])))
#' lines(cultFST(100, 100, 0, 1:100), lty = 1, lwd = 1.5)
#' lines(cultFST(100, 100, 25, 1:100), lty = 2, lwd = 1.5)
#' lines(cultFST(100, 100, 50, 1:100), lty = 3, lwd = 1.5)
#' lines(cultFST(100, 100, 75, 1:100), lty = 4, lwd = 1.5)
#' lines(cultFST(100, 100, 100, 1:100), lty = 5, lwd = 1.5)
#' legend(37, 1, c("0", "25", "50", "75", "100"), title = expression(italic('x'['i'])*' ='), 
#'        lty = 1:5, cex = .9)
#'

cultFST <- function(ni, nj, xi, xj){ 
  pi <- xi/ni
  pj <- xj/nj
  pbar <- (xi + xj)/(ni + nj)
  num <- (ni/(ni + nj))*(pi - pbar)^2 + (nj/(ni + nj))*(pj - pbar)^2
  denom <- pbar*(1 - pbar)
  FST <- num/denom
  return(FST)
}