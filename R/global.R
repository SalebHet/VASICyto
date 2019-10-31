library(tidyverse)
library(beeswarm)
library(DT)
library(plotly)
library(rlist)
library(tidyr)

#' Displays the dashboard part allowing to compute boxplots over total of grouped cytokines results
#'
#' @param label 
totalBoxUI <- function(label="TotalCytoBoxplot") {
  	tagList(
	  	selectInput("stimulation", "Select stimulation", choices=c("GAG", "NEF", "POL ENV", "Any Stimulation"),
	                selected = "GAG"),
	    tabsetPanel(
	        tabPanel("Dataset", DT::dataTableOutput("datasetVisu_totalCyto")), 
	        tabPanel("Chart", plotOutput("boxplotVisu_totalCyto"))
	        # tabPanel("Interactive_Chart", plotlyOutput("interactive_scatter"))
	    )    
    )
}

#' Displays the dashboard part allowing to compute analyses over each cytokines results
#'
#' @param label 
multiBoxUI <- function(label="MultiCytoBoxplot"){
	tagList(
	  	selectInput("stimulation1", "Select stimulation", 
	  		choices=c("GAG"="GAG", "NEF"="NEF", "POL ENV"="POL ENV", "Any Stimulation"="Any Stimulation"), selected = "GAG"),	  	
	  	selectInput("timepoint", "Select timepoint", choices=c("W0"="W0", "W28"="W28")),
		tabsetPanel(
	        tabPanel("Dataset", DT::dataTableOutput("datasetVisu_MultiCyto")), 
	        tabPanel("Chart",
	        	# actionButton("show", "Show settings dialog"), 
	        	plotOutput("boxplotVisu_MultiCyto"))
	    )    
    )
}

#' Displays the dashboard part allowing to compute polyfunctionnality analyses
#'
#' @param label 
polyfuncUI <- function(label="PolyfunctionChart"){
	tagList(
	  	tabsetPanel(
	        tabPanel("Dataset", DT::dataTableOutput("Polyfunc_Dataset")), 
	        tabPanel("Chart",
	        	fluidRow(
            		column(6, shinydashboard::box(title="Histogram", status="primary", plotOutput("Polyfunc_Barplot",  width="500px", height=500), width=12)),
                		column(6,
            				shinydashboard::box(title="PieCharts", status="primary", 
		            		fluidRow(
		            			column(6, plotly::plotlyOutput("Polyfunc_Pieplot1")),
		            			column(6, plotly::plotlyOutput("Polyfunc_Pieplot2"))
		            			),
		            		fluidRow(
		            			column(6, plotly::plotlyOutput("Polyfunc_Pieplot3")),
		            			column(6, plotly::plotlyOutput("Polyfunc_Pieplot4"))
		            			),
		            		width=12
		            		)
	            		)
         			)
         		)
	    	)    
    	)
}

#' run functions needed to compute stats and create graphs associated to the totalBoxUI function
#'
#' @param input input from the shiny dashboard
#' @param output object to e displayed towards the shiny UI output
totalCytoExpr <- function(input, output){
	dataTable <- reactive({
		#load(paste0(system.file("", package = "icsDataViz"), "popCells.RData"))
		#load("data/popCells.RData")
	  data("popCells")
		plot_data <- popCells[[paste0(input$population, ".totalCYTO")]]		
	    dataOut <- subset(plot_data, ARM==input$treatment & Stimulation==input$stimulation)

		return(dataOut)
	})

	title <- reactive({
		return(paste(input$population, " total cytokine response in \n", input$treatment, " group - Stimulation",input$stimulation))
	})
	
	output$datasetVisu_totalCyto <- DT::renderDataTable(dataTable(), options=list(pageLength=50, scrollX='400px', scrollY='400px'))
	output$boxplotVisu_totalCyto <- renderPlot({
			scatter_box(dataInput=dataTable(), modela="totalCYTO ~ TimePoint", 
				title=title(), ylabel=paste(input$population, "cytokine total"))
	})
	# output$interactive_scatter <- renderPlotly({
	# 	# bee <- beeswarm(totalCYTO~TimePoint, data=dataTable(), do.plot=F)
	# 	p <- #plot_ly(dataTable()) %>%
	# 		plot_ly(x=~dataTable()[["TimePoint"]], y=~dataTable()[["totalCYTO"]], type = 'box', boxpoints = "all", name = 'BoxPlot',
	# 		  	jitter=0.3, pointpos = -1.8) #%>% marker = list(coltaor = '#C9EFF9')

	# 		# add_trace(x=bee$x, y=~bee$y, mode= "markers", type="scatter",
	# 		#   	text= paste0("Participant", dataTable()[["Participant"]])) #%>% #popCells$CD4.totalCYTO$Participant))

	# 		# add_trace(x=dataTable()[["TimePoint"]], y=dataTable()[["totalCYTO"]], 
	#   # 			type="jitter", mode="markers", name='Scatter', 
	#   # 			yaxis = 'Total cytokines frequence expression', 
	#   # 			hoverinfo = "text", text = ~paste("Timepoint", TimePoint), jitter = 0.3) #%>%
	# 		# layout(title = 'Total Cytokine expression at W0 and W28',
	# 		# 	xaxis = list(title = ""),
	# 		# 	yaxis = list(side = 'left', title = '% Total of cytokine expression ', showgrid = FALSE, zeroline = FALSE),
	# 		# 	yaxis2 = list(side = 'right', overlaying = "y", title = 'Cytokine type', showgrid = FALSE, zeroline = FALSE))
	#     p$elementId <- NULL
	#     p 
	# })
}


#' run functions needed to compute stats and create graphs associated to the multiBoxUI function
#'
#' @param input input from the shiny dashboard
#' @param output object to e displayed towards the shiny UI output
multiCytoExpr <- function(input, output){
	custom_set <- NULL
	# observeEvent(input$show, {
 #      showModal(multiCytoBox_settings())
 #    })

 #    observeEvent(input$ok, {
 #      custom_set$title <- input$title
 #      custom_set$xaxis <- input$xaxis
 #      custom_set$yaxis <- input$yaxis
 #      removeModal()
 #    })

	dataTable <- reactive({
		#load(paste0(system.file("", package = "icsDataViz"), "popCells.RData"))
		#load("data/popCells.RData")
	  data("popCells")
		plot_data <- popCells[[paste0(input$population1, ".multiCYTO")]]
	    dataOut <-  subset(plot_data, ARM==input$treatment1 & Stimulation==input$stimulation1 & TimePoint==input$timepoint)
		return(dataOut)
	})

	settings <- reactive({
		stringOut <- NULL
		# if(is.null(custom_set)){
			stringOut$title <- paste(input$population1, "cells individual cytokine response in \n", input$treatment1, "group \n Stimulation", 
				input$stimulation1, " and timepoint ", input$timepoint)
			stringOut$yaxis <- paste(input$population1, "individual cytokine expression")
			stringOut$xaxis <- ""
		# }else{
		# 	stringOut$title <- custom_set$title
		# 	stringOut$yaxis <- custom_set$yaxis
		# 	stringOut$xaxis <- custom_set$xaxis
		# }
		return (stringOut)
	})

	output$datasetVisu_MultiCyto <- DT::renderDataTable(dataTable(), options=list(pageLength=50, scrollX='400px', scrollY='400px'))

	output$boxplotVisu_MultiCyto <- renderPlot({
		scatter_box(dataInput=dataTable(), modela="Frequence ~ Cytokine",title=settings()$title, 
			ylabel=settings()$yaxis) #xlabel=settings()$xaxis, 
	})
}

#' DEPRECATED - displays the the settings panel allowing to customize graphs
#'
#' @param failed 
multiCytoBox_settings <- function(failed = FALSE){
	 modalDialog(
        textInput("title", "Please enter custom title"),
        textInput("yaxis", "Please enter custom y axis name"),
        textInput("xaxis", "Please enter custom x axis name"),
        if(failed)
          div(tags$b("Invalid name of data object", style = "color: red;")),

        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok", "OK")
        )
     )
}

#' run functions needed to compute stats and create graphs associated to the multiBoxUI function
#'
#' @param input input from the shiny dashboard
#' @param output object to e displayed towards the shiny UI output
polyfuncExpr <- function(input, output){

	#load(paste0(system.file("", package = "icsDataViz"), "polyfunc.RData"))	
	#load("../data/polyfunc.RData")
  #cat("file path = >")
  #fp <- paste0(system.file("","VASICyto"),"data/polyfunc.RData")
  #cat(fp,"\n")
  #wd <- getwd()
  #load(paste0(wd,fp))
  data("polyfunc")
	elements <- expand.grid(c("W0", "W28"), c("Placebo", "Vaccin"))

	dataTable <- reactive({
		dataOut <- NULL
		dataOut$pie$data <- polyfunc.data[[input$population2]][[paste0("pie_", input$SelectStat)]]
		dataOut$bar <- polyfunc.data[[input$population2]][[input$SelectStat]]

	    return (dataOut)
	})

	values <- reactive({
		full_data <- dataTable()
		dataOut <- list()
		list_labels <- c()
		
		for (i in 1:dim(elements)[1]){
			assign(paste0("values", i), 
				full_data$pie$data[[paste(input$population2, elements[i,1], elements[i,2])]])
			dataOut <- list.append(dataOut, get(paste0("values", i)))
			list_labels <- c(list_labels, paste0("values", i))
		}
		names(dataOut) <- list_labels
		
		return (dataOut)
	})

	title <- reactive({
		return (paste("Boolean percent", input$SelectStat, "of \n cytokine expression in", input$population2, "population."))
		})
	
	output$Polyfunc_Dataset <- DT::renderDataTable(dataTable()$bar, options=list(pageLength=50, scrollX='400px', scrollY='200px'))
	output$Polyfunc_Barplot <- renderPlot({
			histo_polyfunc(dataInput=dataTable()$bar, title=title())
		})

	for (i in 1:dim(elements)[1]){		
		output[[paste0("Polyfunc_Pieplot",i)]] <- plotly::renderPlotly({
				p <- pie_polyfunc(dataInput=dataTable()$pie, dataValues=values()[[paste0("values",i)]], title=title())
	    		p$elementId <- NULL
	    		p
			})
	}
}

#' creates a box plot with scattered observations to visualize total of all cytokines frequence
#' @param dataInput:
#' @param modela:
#' @param title:
#' @param ylabel:
#' @return nothing
scatter_box <- function(dataInput, modela, title, ylabel){
	scatter.colors <- colors_scatter_participants(dataInput)

	boxplot(formula(modela), data=dataInput, notch=FALSE, col="#FFFBF7", main=title, ylab=ylabel, outpch= '.:')	  
	beeswarm::beeswarm(formula(modela), data=dataInput, method='swarm', pch=20, pwcol=scatter.colors$plot, add=T)	  
	legend('topright', legend=rownames(scatter.colors$legend), title='Participant', pch=20, col=as.vector(scatter.colors$legend[, 1]))
}

#' Creates a list of color combination for each participant
#' @param dataInput: 
#' @return colours: a list n colors for n observations, differenciated according to participants
colors_scatter_participants <- function(dataInput){
	colours <- NULL
	mapping <- as.data.frame(rainbow(dim(dataInput)[1])[!duplicated(dataInput$Participant)])
	rownames(mapping) <- as.vector(dataInput$Participant[!duplicated(dataInput$Participant)])
	list_col <- c()
	for (part in dataInput$Participant){
	  list_col <- c(list_col, paste(mapping[paste(part), ]))
	}
	save(mapping, file = "mapping.RData")

	colours$plot <- as.vector(list_col)
	colours$legend <- mapping

	return (colours)
}

#' DEPRECATED - Creates an interactive chart displaying both box plot and scatter
interactive_scatter <- function(){
		# ui <- fluidPage(
		#   plotlyOutput("plot"),
		#   verbatimTextOutput("hover"),
		#   verbatimTextOutput("click")
		# )

		# server <- function(input, output, session) {

		  output$plot <- renderPlotly({
		    plot_ly(x = rnorm(10), y = rnorm(10), z = rnorm(10), type = "scatter3d")
		  })

		  output$hover <- renderPrint({
		    d <- event_data("plotly_hover")
		    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
		  })

		  output$click <- renderPrint({
		    d <- event_data("plotly_click")
		    if (is.null(d)) "Click events appear here (double-click to clear)" else d
		  })

		# }

		shinyApp(ui, server)
}

#' creates an histogram visualizing cytokines polyfunctionnality
#' @param dataInput: an 
#' @param title: graph title
histo_polyfunc <- function(dataInput, title){
	barplot(dataInput, font.axis=1, beside=T, names=names(dataInput), col=rainbow(dim(dataInput)[1]), 
		legend=rownames(dataInput), las=2, main=title)
}

#' create a pie chart allowing to visualize cytokine distribution by group 
#'
#' @param dataInput 
#' @param dataValues 
#' @param title 
pie_polyfunc <- function(dataInput, dataValues, title){
  p <- plotly::plot_ly(dataInput$data, labels = ~dataInput$data$Polyfunction, values = ~dataValues, type = 'pie', 
		marker = list(colors = rainbow(length(dataInput$data$Polyfunction)), line = list(color = '#FFFFFF', width = 1)))# %>%
  layout(
          p,
         title = title,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}
