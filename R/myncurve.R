#' @title Normal Lower Tail Value
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value in normal distribution
#' @param
#'
#' @return Shaded Region of Lower Tail Value
#' @importFrom stats dnorm pbinom pnorm
#' @importFrom graphics polygon text curve par
#' @export
#'
#' @examples
#' \dontrun{myncurve(0.5, 0, 1)}
myncurve = function(a, mu, sigma){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  return(list(mu = mu, sigma = sigma, a = a))
  xcurve=seq(a, mu-3*sigma,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(a,xcurve,a),c(0,ycurve,0),col="Red")
  prob=pnorm(a, mean = mu, sd = sigma)
  prob=round(prob,4)
  text(x = (a/2), y = 0.5*dnorm(a/2, mean = mu , sd = sigma), paste0("Area =", prob))
}
