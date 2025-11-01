#' Amount of tickets bought on the plane (n)
#'
#' @param N number of seats total
#' @param g gamma or pain (more people than seats)
#' @param p probability people will show up
#'
#' @returns A list of variables (total tickets bought using discrete (nd)
#' and continuous (nc) methods, N, g, and p) and tables of Objective vs n
#' for discrete and continuous
#'
#' @importFrom graphics abline
#' @importFrom stats optimize pbinom
#'
#' @export
#'
#' @examples ntickets(N=200, g=0.02, p=0.95)
ntickets <- function(N, g, p){
  #getting probability of no show (q)
  q <- 1-p

  #finding n using discrete method
  #function of f(n)=0
  fdis <- function(n, N, p, g){
    obj <- pbinom(N, n, p)-1+g
    abs(obj)
  }
  #making a range of possible tickets bought
  xx <- seq(N, N+20, by=1)
  #find the minimum
  index <- which.min(fdis(xx, N=N, p=p, g=g))
  #the n variable by finding the lowest value (how many seats bought)
  nd <- xx[index]

  #finding n using continuous method (normal approx)
  #function of f(n)
  fcon <- function(n, N, p, g){
    #finding q
    q <- 1-p
    #f(n)=0 for continuous
    obj <- pnorm(N+0.5, n*p, sqrt(n*p*q))-1+g
    abs(obj)
  }
  #using optimize to find n for continuous (the minimum is n)
  nc <- optimize(fcon, interval=c(N, N+20), N=N, p=p, g=g)$min

  #plots of Objective Vs n
  #redoing discrete function to make plot
  fdpl <- function(n, N, p, g){
    1-g-pbinom(N, n, p)
  }
  #discrete plot
  plot(xx,fdpl(xx, N=N, p=p, g=g), type='o', xlim= c(N, N+20),
       #bg ifelse colors the n variable
       ylim=c(0,1), pch =21, bg = ifelse(xx != nd, "blue", "red"),
       main=paste('Objective Vs n to find optimal tickets sold \n (',
                  nd,') gamma=',g,'N=',N, 'discrete'),
       xlab='n', ylab='Objective')
  #draws line where n is at 0
  abline(v=nd, h=0, col='red', lw=2)

  #redoing continuous function for plot
  fcpl <- function(n, N, p, g){
    #finding q
    q <- 1-p
    #f(n)=0 for continuous
    1-g-pnorm(N+0.5, n*p, sqrt(n*p*q))
  }
  #continuous plot
  plot(xx, fcpl(xx, N=N, p=p, g=g), type='l', xlim= c(N, N+20),
       ylim=c(0,1),
       main=paste('Objective Vs n to find optimal tickets sold \n (',
                  nc,') gamma=',g,'N=',N, 'continuous'),
       xlab='n', ylab='Objective')
  #draws line where n is at 0
  abline(v=nc, h=0, col='blue', lw=2)

  #list if variables
  list(nd=nd, nc=nc, N=N, p=p, gamma=g)

}
