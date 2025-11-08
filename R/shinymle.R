#' SHINY MLE
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{shinymle()}
shinymle <- function(){
  shiny::runApp(system.file("SHINY", package = "MATH4753NUNN2025"),
                launch.browser = TRUE)
}
