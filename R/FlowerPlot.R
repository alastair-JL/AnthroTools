#' FlowerPlot
#'
#' Make a flower plot (aka "BlumePlot" or "BlomsterPlot" or "ChechekPlot") automatically. If you can't contort it in-screen, then you'll have to manually do it (see code at https://anthrotools.wordpress.com). Sometimes your items will be too long to fit in the circles. Either change the data or--probably easier--is to rename the cells in the input table.
#' @usage FlowerPlot(x, y)
#' @param x This is the output of a SalienceByCode analysis (with Smith's S).
#' @param y This is the domain name you want with "quotes" around it (e.g., "Fruits").
#' @return Returned will be a Flower Plot where the top item is the most salient item and Smith's S descends in a clockwise fashion. Connection weights' width correspond to Smith's S.
#' @keywords FlowerPlot
#' @note This graph will not be customizable without using code. Sometimes, items' names will be too long. You'll have to adjust accordingly if so. If you need to manually make one of these plots, try the code available at https://anthrotools.wordpress.com. This also uses a circle function that we found here: https://aurelienmadouasse.wordpress.com/2012/04/28/r-code-how-to-draw-a-circle/.

#' @export
#' @examples
#' data(FruitList)
#' FL.S <- CalculateSalience(FruitList)
#' model <- SalienceByCode(FL.S, CODE = "CODE", Subj = "Subj", Salience = "Salience", dealWithDoubles = "MAX")
#' par(mar=c(0,0,0,0))
#' FlowerPlot(model, "Fruits")
#'

FlowerPlot <- function(x, y){# x is output from SalienceByCode, y is domain name with "quotes" around it
  sort <- x[order(-x$SmithsS),]
  plot(c(-110, 110), c(-110, 110), type = "n", xlab = "", ylab = "", axes = FALSE, asp = 1, family = "Calibri")
  rad <- 30 # predefined radius
  notch <- 50 # length for vertical and horizontal lines
  nitch <- 21.5 # length for diagonals
  natch <- 38.5 # length for diagonals
  m1 <- sort[1:8,4]
  cen <- m1[1]*10
  top <- cen
  tor <- m1[2]*10
  rig <- m1[3]*10
  lor <- m1[4]*10
  bot <- m1[5]*10
  lol <- m1[6]*10
  lef <- m1[7]*10
  tol <- m1[8]*10
  circle(0, 0, rad, add = TRUE, col = "black", lwd = cen) # domain circle
  circle(0, 80, rad, add = TRUE, col = "black", lwd = top) # top circle
  circle(60, 60, rad, add = TRUE, col = "black", lwd = tor) # top-right
  circle(80, 0, rad, add = TRUE, col = "black", lwd = rig) # right circle
  circle(60, -60, rad, add = TRUE, col = "black", lwd = lor) # lower right
  circle(0, -80, rad, add = TRUE, col = "black", lwd = bot) # bottom circle
  circle(-60, -60, rad, add = TRUE, col = "black", lwd = lol) # lower left
  circle(-80, 0, rad, add = TRUE, col = "black", lwd = lef) # left circle
  circle(-60, 60, rad, add = TRUE, col = "black",  lwd = tol) # top-left
  segments(0, rad, 0, notch, lwd = top) # top
  segments(nitch, nitch, natch, natch, lwd = tor) # upper right
  segments(rad, 0, notch, 0, lwd = rig) # right
  segments(nitch, -nitch, natch, -natch, lwd = lor) # lower right
  segments(0, -rad, 0, -notch, lwd = bot) # bottom
  segments(-nitch, -nitch, -natch, -natch, lwd = lol) # lower left
  segments(-rad, 0, -notch, 0, lwd = lef) # left
  segments(-nitch, nitch, -natch, natch, lwd = tol) # upper left
  text(0, 0, labels = y, font = 2) # center
  text(0, 80, labels = sort[1,1], font = 2) # top
  text(60, 60, labels = sort[2,1], font = 2) # 2 o'clock
  text(80, 0, labels = sort[3,1], font = 2) # right
  text(60, -60, labels = sort[4,1], font = 2) # 4 o'clock
  text(0, -80, labels = sort[5,1], font = 2) # bottom
  text(-60, -60, labels = sort[6,1], font = 2) # 8 o'clock
  text(-80, 0, labels = sort[7,1], font = 2) # 9 o'clock
  text(-60, 60, labels = sort[8,1], font = 2) # 10 o'clock
  xxx <- formatC(round(m1, 2), format = 'f', digits = 2 )
  text(10, rad+10, labels = xxx[1], font = 2) # top
  text(35, 25, labels = xxx[2], font = 2) # top right
  text(rad+10, -5, labels = xxx[3], font = 2) # right
  text(25, -35, labels = xxx[4], font = 2) # 4
  text(-10, -rad-10, labels = xxx[5], font = 2) # bottom
  text(-35, -25, labels = xxx[6], font = 2) # 7
  text(-rad-10, 5, labels = xxx[7], font = 2) # left
  text(-22, 35, labels = xxx[8], font = 2) # 10
}

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
