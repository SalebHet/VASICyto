#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#run_app <- function(...) {
#  with_golem_options(
#    app = shinyApp(ui = app_ui, server = app_server), 
#    golem_opts = list(...)
#  )
#}

run_app <- function(...){
  shiny::runApp(system.file("app",package = "VASICyto"))
}