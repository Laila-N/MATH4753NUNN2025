
#' My Binomial that shows a graph and table
#'
#' @param iter iterations or trials
#' @param n size or range of numbers ex. n=5 5 numbers show up on x axis of plot
#' @param p probability
#'
#' @returns a barplot showing the amount of times a number shows up over a
#' certain amount of iterations also shows a table for the amount of success
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @examples
#' mybin(iter=100, n=10, p=0.5)
mybin=function(iter=100,n=10, p=0.5){
  # makes matrix to hold samples with NA values
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1),
          main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
