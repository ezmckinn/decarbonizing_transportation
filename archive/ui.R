#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Decarbonizing Transport",
    
    ## Title
    
    ## Tab 1: Plots
    
    ## Column 1 (70% left)
    
        ##Row 1 (70%) 
            
            ##star plot
    
    tabPanel('Plots',
             
             sidebarLayout(fluid = TRUE,
                 
                 sidebarPanel(width = 5,
                    
                                  p(span(em("Note: Draft mockup. Units and values generated at random, and do not reflect cities listed here."), style = "color:red")),
                                  p(em(strong("Instructions")),
                                  p("1. Select a benchmark city."),
                                  p("2. Change your mix of decarbonization strategies."),
                                  p("3. Make it to your emissions goal!"),
                                  p("  ")),
                                
                     fluidRow(
                         column(6, span(strong("Select Benchmark City"), style = "color:#871AF0")), #pal <- c("#871AF0","#5BF0EF","#1DF039","#EBF022","#F59D6B")
                         column(6, selectInput('city', label = NULL, selected = "Boston", choices = c("Paris","Los Angeles","Vancouver","New York City","Boston")))
                         ),
                     
                     fluidRow(column(4, em(strong("Factor"))), column(4, p(em(strong("Benchmarks")))), column(4, p(em(strong("Your Values"))))),
                     fluidRow(column(4, strong("EV %")), column(4, textOutput('city_ev_prc')), column(4, numericInput('ev_prc', label = NULL, min = 0, max = 100, value = 50, step = 5)) ),
                     fluidRow(column(4, strong("Transit Mode Share")), column(4, textOutput('city_transit')), column(4, numericInput('transit', label = NULL, min = 0, max = 100, value = 50, step = 5)) ),
                     fluidRow(column(4, strong("Density Increase %")), column(4, textOutput('city_dens_inc')), column(4, numericInput('dens_inc', label = NULL, min = 0, max = 100, value = 50, step = 5)) ),
                     fluidRow(column(4, strong("Grid Renewable %")), column(4, textOutput('city_grid_rnw')), column(4, numericInput('grid_rnw', label = NULL, min = 0, max = 100, value = 50, step = 5)) ),
                     fluidRow(column(4, strong("VMT Reduce %")), column(4, textOutput('city_vmt_percap')), column(4, numericInput('vmt_percap', label = NULL, min = 0, max = 100, value = 50, step = 5)) ),
                     
                    
                 
                 ),
    
            mainPanel(width = 7,
                
                fluidRow(
                    
                    column(6, h4("Current Emissions: 250 GtCO2"), h4(textOutput('city_select'))),
                    column(6, h4(textOutput('savings'), h4(textOutput('yay'))))
                    ),
                
                fluidRow(
                 plotOutput("radar")
                         ),
                
                fluidRow(
                        
                        em(h4("A Kaya Identity for Transportation")),    
                    
                        plotOutput("kaya")
                        
                       
                )
            ), #close main panel
        )
    ),

    tabPanel('Data', DT::dataTableOutput("mytable")),
    
    tabPanel('About',
             
             p("Cities have set long-range goals to create a zero-emissions transportation system by 2050, consistent with IPCC goals. What mix of interventions will it take to get there? This tool helps users try out different mixes, and compare their strategies to cities that have set net zero goals."),
             p("This platform reflects the Decarbonizing Transportation Model built by Andrew Salzberg. Access the full Excel model", a("here.", href = "https://docs.google.com/spreadsheets/d/1odc3-H2BlM42PjJ-xAuPcuv7GDYESJXDWqADQwoy4TM/edit#gid=143699780"),
             p("Subscribe to the Decarbonizng Transportation newsletter", a("here.", href = "https://decarbonizingtransportation.substack.com/")))
             )
    
    )

)


    
        ##Row 2 (30%)
    
            ## reset button
            ## numeric inputs (sliders?)
            ## baseline
            ## city select baseline 
    
    ## Column 2 (30% Right)
        
        ## Row 1 (70%) 
    
             ##star plots for 5 model cities
    
        ## Row 2 (30%) 
    
            ## instructions 
    
    ## Tab 2: The Data
        
        ## output DF — showing targets
    
    ## Tab 3: Sources
        
        ## links to plans 
    


