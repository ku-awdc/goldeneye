#' Launch shiny application
#'
#' @import shiny
#' @import shinydashboard
#'
#' @export
run_app <- function(){

  runApp(system.file("shiny", "file_encryption", package="goldeneye"))

}

