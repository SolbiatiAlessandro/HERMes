library(shiny)
library(dygraphs)
library(datasets)
library(timeSeries)
library(tseries)
library(TTR)
source("checks.R")
source("portafoglio.R")
source("getprice.R")
source("tether.R")
source("hma_sign.R")
source("blockize.R")
source("get_risk.R")
source("get_cap.R")
library(xts)


shinyServer(function(input, output) {

	hma_n <- 20
	tether_t <- 25
	adx_n <- 14

	dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
  		dyPlugin(
    		dygraph = dygraph,
    		name = "Crosshair",
    		path = system.file("examples/plugins/crosshair.js", 
                       package = "dygraphs"),
    		options = list(direction = match.arg(direction))
  		)
	}

	# no internet
	# load("C:\\Users\\dell\\Desktop\\Workspace\\HERMes\\HERMes workspace\\my_achi")



	output$table_p_titles <- renderTable(strsplit(input$p_titles, " "),colnames=FALSE,hover=TRUE)
	output$table_up_titles <- renderTable(strsplit(input$up_titles, " "),colnames=FALSE,hover=TRUE)
	output$table_down_titles <- renderTable(strsplit(input$down_titles, " "),colnames=FALSE,hover=TRUE)
	
 	
	daily_check_output <- eventReactive(input$Start_HERMes, {
		progress <- shiny::Progress$new()
    	progress$set(message = "", value = 0)
       	on.exit(progress$close())
    
    	updateProgress <- function(value = NULL, detail = NULL) {
      		if (is.null(value)) {
        		value <- progress$getValue()
        		value <- value + (progress$getMax() - value) / length(titoli_funzionanti)
      		}
      		progress$set(value = value, detail = detail)
    	}
    input_titles<-strsplit(input$p_titles, " ")
    print(input_titles)
		daily_check_complete(strsplit(input$p_titles, " ")[[1]], ups=strsplit(input$up_titles, " ")[[1]], downs=strsplit(input$down_titles, " ")[[1]], updateProgress=updateProgress)
		#no_internet_test(titoli_funzionanti,updateProgress=updateProgress)
	})

	output$HERMes_output_index <- renderText({paste0(length(strsplit(input$p_titles, " ")[[1]])," ",length(strsplit(input$up_titles, " ")[[1]])," ",length(strsplit(input$down_titles, " ")[[1]]))})
	output$HERMes_output_content <- renderText({paste0(daily_check_output())})	
	#output$non_pos_titoli <- renderText({paste0(titoli_funzionanti)})
	#output$ups_titoli <- renderText({paste0(my_ups)})
	#output$downs_titoli <- renderText({paste0(my_downs)})
	output$non_pos_titoli <- renderText({paste0(input$p_titles)})
	output$ups_titoli <- renderText({paste0(input$up_titles)})
	output$downs_titoli <- renderText({input$down_titles})
  
  	#Single Check
	len <- reactive({abs(input$time_length[2]-input$time_length[1])})
  my_OHLC <- reactive({   
    if(input$title_name!=""){
      getOHLC_last(input$title_name, 800)
      }			
  })
  my_C <- reactive({my_OHLC()[,4]})
  	
  	my_tether_line <- reactive({tether(cbind(my_OHLC()[,2],my_OHLC()[,3]),t=tether_t)})
  	my_hma_line <- reactive({as.timeSeries(HMA(my_OHLC()[,4],n=hma_n))})
  	my_hma_signal <- reactive({as.timeSeries(hma_sign(my_hma_line()))})
    #blocks_mat is the matrix with date of the hma_sign for shading, use cbind to add the correct dates
  	blocks_mat <- reactive({blockize(cbind(my_OHLC()[,4],my_hma_signal())[,2])})
  	my_adx <- reactive({ADX(cbind(my_OHLC()[,2],my_OHLC()[,3],my_OHLC()[,4]),n=adx_n)[,4]})
  	
	#main chart
	
    #sistemare colori!
  	output$dygraph1 <- renderDygraph({if(input$title_name!=""){
      price <- reactive(my_OHLC()[,4])
      tetherLine <- reactive(my_tether_line())
  		my_dygraph <- dygraph(cbind(price(),tetherLine()),height=350) %>% 
  	  	#dySeries("price") %>%
  		#dySeries("tether_line") %>%
  		dyLegend(width=400) %>%
  		dyCrosshair(direction = "vertical") %>%
  		dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.6,
              hideOnMouseOut = FALSE)   %>%
      dyOptions(colors=c("black","blue"))
  		len2 <- reactive({length(blocks_mat()[,1])})
		  for(i in 1:len2()-1){
			  #cat("doing shading")
			  shading <- list()
    	  shading$from <- as.Date(blocks_mat()[i,3])
    		shading$to <- as.Date(blocks_mat()[i,2])
        color_flag <- blocks_mat()[i,4] 
			  ifelse(color_flag==1, shading$color <- "#a6ffa3", shading$color <- "#ffa5a5")
    		shading$axis <- "x"
			  my_dygraph$x$shadings[[length(my_dygraph$x$shadings) + 1]] <- shading
			  #print(my_dygraph$x$shadings)
		  }

      shading$from <- as.Date(blocks_mat()[i,2])
      shading$to <- as.Date("2020-01-01")
      ifelse(color_flag==1, shading$color <- "#ffa5a5", shading$color <- "#a6ffa3")
      my_dygraph$x$shadings[[length(my_dygraph$x$shadings) + 1]] <- shading

		  my_dygraph
    }
  	})
  	output$scheck_graph_A <- renderUI({dygraphOutput("dygraph1")})
  	
  	#adx chart -> da sistemare dimensioni!
  	output$dygraph2 <- renderDygraph({if(input$title_name!=""){
      ADX <- reactive(my_adx())
  		dygraph(cbind(ADX()),height=100) %>%
  		dyAxis("x", drawGrid = FALSE) %>%
  		dyOptions( 
           axisLineColor = "navy", 
            gridLineColor = "lightblue") %>%
  		dyLimit(20, color = "red") %>%
  		dyCrosshair(direction = "vertical")
  		}})
  	output$scheck_graph_B <- renderUI({dygraphOutput("dygraph2")})

  	
	output$yesterday_price <- renderText({yesterday_price_r()})
	output$today_price <- renderText({today_price_r()})
	
  	#algorithm check
	len_my_C <- reactive({length(my_OHLC()[,4])})	
  yesterday_price_r <- reactive({my_OHLC()[len_my_C()-1,4]})
  today_price_r <- reactive({my_OHLC()[len_my_C(),4]})

	data_hma <- reactive({HMA(my_OHLC()[(len_my_C()-24):len_my_C(),4],n=20)})
	data_hma_sign <- reactive({hma_sign(data_hma())})	
	len_data_hma_sign <- reactive({length(data_hma_sign())})

  data_adx <- reactive({ADX(cbind(my_OHLC()[(len_my_C()-50):len_my_C(),2],my_OHLC()[(len_my_C()-50):len_my_C(),3],my_OHLC()[(len_my_C()-50):len_my_C(),4]),n=14)})
	len_data_adx <- reactive({length(data_adx()[,4])})

  data_tt <- reactive({tether(cbind(my_OHLC()[(len_my_C()-51):len_my_C(),2],my_OHLC()[(len_my_C()-51):len_my_C(),3]),t=25)})
	len_data_tt <- reactive({length(data_tt())}) 
	#capire perché reattivita non funziona con caselle di testo!
	
  output$yesterday_Tether_Line <- renderText({data_tt()[len_data_tt()-1]})
  output$today_Tether_Line <- renderText({data_tt()[len_data_tt()]})
  output$today_ADX <- renderText({data_adx()[len_data_adx(),4]})
  output$today_HMA <- renderText({data_hma_sign()[len_data_hma_sign()]})

  #EWMA RiskMetrics
  last_day <- reactive({as.numeric(time(my_OHLC())[length(my_OHLC()[,1])])})
  risk <- reactive({get_risk(my_OHLC(),"2016-11-11",debug=0)})

  output$extimated_VaR <- renderText(risk()[1])
  output$extimated_EF <- renderText(risk()[2])
  output$extimation_p_value <- renderText(risk()[3])
  output$extimation_confidence <- renderText(risk()[4])
  output$extimated_mean_VaR <- renderText(risk()[5])

  capital <- reactive({get_cap(risk())})
  output$cap_position <- renderText({capital()})
  output$cap_position_bar <- renderText({capital()})

    	
  	
})