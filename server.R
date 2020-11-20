library(shiny)
library(fmsb)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  
  
  ##SET UP DATA (will be able to read in from github in future## 
  
  #city <- c("Paris","Los Angeles","Vancouver","New York City","Boston")
  ev_prc <- c(100, 85, 90, 80, 100) #percentage of vehicle fleet as EVs
  dens_inc <- c(15, 50, 50, 30, 10) #density increase 
  grid_rnw <- c(60, 75, 85, 90, 60) #percent of grid that is renewable
  transit <- c(40, 35, 50, 70, 75) #percent increase in transit 
  vmt_percap <- c(20, 10, 25, 30, 50) #vmt per capita
  
  data <- data.frame(ev_prc, dens_inc, grid_rnw, transit, vmt_percap)
  
  maxmin <- data.frame( #set maximum and minimum values for radar plot
    ev_prc=c(100, 0),
    dens_inc=c(100, 0),
    grid_rnw=c(100, 0),
    transit=c(100, 0),
    vmt_percap=c(100, 0))
  
  
  ##WIDGET FOR USER-SELECTED CITY##
  
  output$selected_city <- renderText({ 
    paste("Benchmark City:", input$city)
  })
  
  #SET UP STAR PLOTS
    
   pal <- c("#871AF0","#5BF0EF","#1DF039","#EBF022","#F59D6B")
    
        observeEvent(c(input$city, input$ev_prc, input$transit, input$dens_inc, input$grid_rnw, input$vmt_percap), {
              
            user_data <- reactive({ c(as.numeric(input$ev_prc), as.numeric(input$transit), as.numeric(input$dens_inc), as.numeric(input$grid_rnw), as.numeric(input$vmt_percap))})
            user <- user_data()
            mock_data <- rbind(maxmin, data, user)
            rownames(mock_data) <- c("1","2","Paris","Los Angeles","Vancouver","New York City","Boston","User")
            
            city <- subset(mock_data, rownames(mock_data) %in% c(paste(input$city),"User","1","2"))
            city_labels <- subset(mock_data, rownames(mock_data) %in% paste(input$city))
            
            output$city_ev_prc <- renderText(paste(city_labels$ev_prc, "%"))
            output$city_dens_inc <- renderText(paste(city_labels$dens_inc, "%"))
            output$city_grid_rnw <- renderText(paste(city_labels$grid_rnw, "%"))
            output$city_transit <- renderText(paste(city_labels$transit, "%"))
            output$city_vmt_percap <- renderText(paste(city_labels$vmt_percap, "%"))
            
        output$radar <- renderPlot({
        
        radarchart(city, axistype = 4, seg = 5, vlabels = c("EV Percentage", "Transit Mode Share", "Density Increase", "Grid Renewable %", "VMT Reduction  Per Capita"), 
                            plty = 7,
                            pcol = pal, 
                            pfcol = pal, 
                            pdensity = 40, 
                            cglwd = 1, 
                            cglty = 3, 
                            calcex = 0.5, 
                            cglcol = "grey", 
                            axislabcol = "#871AF0",
                            vlcex = 0.8)
          legend(x=1.15, 
                 y=1.35, 
                 legend = rownames(city[-c(1,2),]), 
                 bty = "n", pch=20 , col = pal, cex = 1.05, pt.cex = 1.5)
          
        })
        
        ##SET UP SUCCESS MESSAGE##
        
        vars <- c(input$ev_prc, input$transit, input$dens_inc, input$grid_rnw, input$vmt_percap)
        status_quo <- sum(vars)
        ghgs <- if_else((300 - status_quo>0),
                         300 - status_quo,
                                   0)
        
        output$status_quo <- renderText(paste("Current Emissions:", as.character(status_quo), "GtCO2"))
                                        
        output$savings <- renderText(if_else(ghgs > 0, paste("Total Emissions in 2050:", as.character(ghgs), "GtCO2"), paste("Net zero in 2050!")))
        output$yay <- renderText(if_else(ghgs > 0, paste("Not there yet!"), paste("You did it!")))
        
        })
        
        ##Image of Kaya Identity##
        
        output$full_kaya <- renderImage({
          
          list(src = "/Volumes/Samsung_T5/Decarbonizing_Transport/Decarbonizing_Transport/full_kaya.png",
               width = "400px",
               height = "100px",
               alt = "Kaya Identity for Transport",
               deleteFile = TRUE)
          
        })
        
        output$city_select <- renderText(paste("Selected City:",input$city))
        
        output$kaya <- renderImage({
          
          list(src = "/Volumes/Samsung_T5/Decarbonizing_Transport/Decarbonizing_Transport/decarbonizing_transpo_kaya_identity.png",
               width = "100%",
               height = "auto",
               alt = "Kaya Identity for Transport",
               deleteFile = TRUE)
          
        })
        
        #Data Table
        cities <- c("Paris","Los Angeles","Vancouver","New York City","Boston")
        city_data <- as.tibble(cbind(cities, data))
        
        output$mytable = DT::renderDataTable({
          city_data %>% rename('Cities' = cities, 'EV Percentage' = ev_prc, 'Density % Increase' = dens_inc, 'Gride Renewable Prc.' = grid_rnw, 'Transit %' = transit,'VMT Reduction %'= vmt_percap)
        })
        

})
