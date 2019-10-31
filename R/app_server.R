#' @import shiny
#' @import shinydashboard
#' 
library(shinydashboard)
app_server <- function(input, output,session) {
  # List the first level callModules here
  ##Total cytokine boxplot module
  totalCytoBox <- totalCytoExpr(input, output)
  individualCytoBox <- multiCytoExpr(input, output)
  polyfuncCharts <- polyfuncExpr(input, output)
}
