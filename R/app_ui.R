#' @import shiny
#' @import shinydashboard

library(shinydashboard)
app_ui <- function() {
  # Leave this function for adding external resources
  golem_add_external_resources()
  #browser()
  shinydashboard::dashboardPage(
    # List the first level UI elements here 
     shinydashboard::dashboardHeader(title = "Flow Dashboard"),
     shinydashboard::dashboardSidebar(
       shinydashboard::sidebarMenu(id = "sidebarmenu",
                    shinydashboard::menuItem("Total cytokine expression", tabName = "BoxPlots", selected = TRUE),
                    conditionalPanel("input.sidebarmenu === 'BoxPlots'",
                                     tagList(
                                       selectInput("population", "Select Cellular Population To Display", 
                                                   choices=list("CD4 cells"="CD4", "CD8 cells"="CD8"), selected="CD4"),
                                       radioButtons("treatment", "Select group", choices=list("Placebo"="Placebo", "Vaccin"="Vaccin"), selected="Placebo")
                                     )
                    ),
                    
                    shinydashboard::menuItem("Individual cytokine expression", tabName = "MultiBox", selected = FALSE),
                    conditionalPanel("input.sidebarmenu === 'MultiBox'",
                                     tagList(
                                       selectInput("population1", "Select Cellular Population To Display", 
                                                   choices=list("CD4 cells"="CD4", "CD8 cells"="CD8"), selected="CD4"),
                                       radioButtons("treatment1", "Select group", choices=list("Placebo"="Placebo", "Vaccin"="Vaccin"), selected="Placebo")
                                     )
                    ),
                    
                    shinydashboard::menuItem("Polyfunctionnality", tabName="Polyfunc", selected=FALSE),
                    conditionalPanel("input.sidebarmenu === 'Polyfunc'",
                                     tagList(
                                       selectInput("population2", "Select Cellular Population To Display", 
                                                   choices=list("CD4 cells"="CD4", "CD8 cells"="CD8"), selected="CD4"),
                                       radioButtons("SelectStat", "Select statistics", 
                                                    choices=list("Mean"="mean", "Median"="median"), selected="mean")
                                     )
                    )
        )),
     shinydashboard::dashboardBody(
       shinydashboard::tabItems(
         shinydashboard::tabItem(tabName="BoxPlots", shinydashboard::box(totalBoxUI(), width=12), selected=TRUE),
         shinydashboard::tabItem(tabName="MultiBox", shinydashboard::box(multiBoxUI(), width=12)),
         shinydashboard::tabItem(tabName="Polyfunc", shinydashboard::box(polyfuncUI(), width=12))
        )
      )
    )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'VASICyto')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
