#' My Central Limit Theorem (CLT) Uniform Function
#'
#' @param n size of sample
#' @param iter iterations
#'
#' @returns a histogram of the CLT uniform
#' @importFrom stats runif
#' @export
#'
#' @examples myclt(n=10, iter=10000)
#'
myclt=function(n,iter){
  y=runif(n*iter,0,5) #A
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  hist(sm)
  sm
}
