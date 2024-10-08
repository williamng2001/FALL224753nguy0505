#' @title Overbooking Ticket Function
#'
#' @param N mean
#' @param gamma probability plane will be overbooked
#' @param p probability of a show
#'
#' @return Binomial solution with nd as binomial answer to number of seats to sell with a plot along with a Normal approximation answer
#' @importFrom graphics abline curve par
#' @export
#'
#' @examples
#' \dontrun{ntickets(200, 0.02, 0.95)}
ntickets <-function(N, gamma, p){
  n <- seq(N, floor(N + N/10), by = 1)
  tmp <- 1- gamma - pbinom(q = N,
                           size = n,
                           prob = p)
  ind <- which.min(abs(tmp))

  nn <- seq(N, N +N/10, by =.001)
  tmt <- 1-gamma - pnorm(N, nn*p, sqrt(nn*p*(1-p)))
  indc <- which.min(abs(tmt))
  indc

  f <- function(nn){
    1-gamma-pnorm(N, nn*p, sqrt(nn*p*(1-p)))
  }

  par(mfrow = c(2, 1))

  plot(n, tmp, type = "b", pch = 16, cex = 0.8,
       ylab = "Objective Function",
       xlab = "Number of Tickets (n)",
       main = "Discrete Case (Binomial)")
  abline(v = n[ind], h = tmp[ind])
  curve(f, xlim= c(N, floor(N+N/10)),
        ylab = "Objective Function",
        xlab = "Number of Tickets (n)",
        main = "Normal Approximation")
  abline(v=nn[indc], h = tmt[indc])

  par(mfrow = c(1, 1))

  list(nd = n[ind], nc  =nn[indc], N=N, p=p, gamma = gamma)
}
