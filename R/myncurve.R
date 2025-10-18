#' Normal Curve Function
#'
#' @param a a value of P(X<=a)
#' @param mu mean
#' @param sigma sd
#'
#' @returns plot of normal distribution w/ shaded area of P(X<=a) and displays the probability in the command line
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#'
#' @export
#'
#' @examples myncurve(10,10,4)
myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma))
  xcurve <- seq(mu-3*sigma, a, length=2000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col='Pink')
  prob <- round(pnorm(a,mean=mu,sd=sigma), 4)
  list(mu = mu, sigma = sigma, a=a, Probability=prob)
}
