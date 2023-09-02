#' Launch shiny application
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyMatrix
#'
#' @export
app_launch <- function(){

  runApp(system.file("shiny","mainapp", package="sdubcontrol"))

}

