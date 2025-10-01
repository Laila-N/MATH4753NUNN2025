#' Birthday function
#'
#' @param x class size
#'
#' @returns probability of people with the same bdays from a class w/ x amnt of students
#' @export
#'
#' @examples
#' birthday(1:10)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
