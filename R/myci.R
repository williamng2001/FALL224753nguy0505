#' @title Confidence Interval Function
#'
#' @param x sample
#' @param alpha type 1 error
#'
#' @return
#' @export
#' @importFrom stats qt sd
#'
#' @examples
#'  \dontrun{myci(x=sam, alpha = 0.05)}
myci=function(x, alpha){
  std <- sd(x) # sample standard deviation
  l = length(x)
  t<-qt(1-alpha/2, l-1) # t multiplier
  mp <- c(-1,1) # -/+
  mean(x)+ mp*t*std/sqrt(l) # formula
}
