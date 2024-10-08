#' @title Scatter histogram
#'
#' @param x x variable as vector
#' @param y y variable as vector
#' @param xlab x label
#' @param ylab y label
#'
#'
#' @return A sophisticated plot
#' @export
#'
#' @examples
#' \dontrun{scatterhist(ddt$LENGTH, ddt$WEIGHT, xlab = "Length")}
scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  graphics::layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = graphics::hist(x, plot=FALSE)
  yhist = graphics::hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  graphics::plot(x,y)
  par(mar=c(0,3,1,1))
  graphics::barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  graphics::barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  graphics::mtext(xlab, side=1, line=1, outer=TRUE, adj=0,
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  graphics::mtext(ylab, side=2, line=1, outer=TRUE, adj=0,
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}
