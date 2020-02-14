#' Circle
#'
#' Make a circle.
#' @usage circle <- function(xorig, yorig, radius, add, ...)
#' @param x This is the output of a SalienceByCode analysis (with Smith's S).
#' @param y This is the domain name you want with "quotes" around it (e.g., "Fruits").
#' @return Returned will be a Flower Plot organized in a clockwise fashion where the top item is the most salient and the 
#' @keywords circle
#' @note This is a contingency function taken from https://aurelienmadouasse.wordpress.com/2012/04/28/r-code-how-to-draw-a-circle/.

#' @export
#' @examples
#' 

circle <- function(xorig, yorig, radius, add, ...){ 
  x <- seq(-radius, radius, length.out = 1000)
  y <- sapply(x, function(z) sqrt(radius^2 - z^2))
  if(add == TRUE){
    lines(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
          type = "l", ...)
  } else {
    plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
         type = "l", ...)
  }
}
