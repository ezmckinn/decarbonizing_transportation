
library(shiny)
library(tidyverse)
library(DT)
library(googlesheets4)
library(plotly)
library(shinythemes)
library(fontawesome)
library(emojifont)
library(scales)

#setwd("/Users/emmettmckinney/Documents/CodeAcademy/Decarbonizing_Transport/Decarbonizing_Transport/shiny_app")

transit <- read.csv("./data_for_viz/modes.csv") #mode share
avo <- read.csv("./data_for_viz/avo.csv") #average vehicle occupancy
grid <- read.csv("./data_for_viz/grid_renewable_prc.csv") #percent renewable energy on grid
ev_prc <- read.csv("./data_for_viz/ev_prc.csv")
density <- read.csv("./data_for_viz/density.csv")
work_home <- read.csv("./data_for_viz/work_home.csv")
carpool <- read.csv("./data_for_viz/carpool_work.csv")
cbsa_pop <- read.csv("./data_for_viz/cbsa_pop.csv")
density_change <- read.csv("./data_for_viz/density_change.csv")

transit <- transit %>% mutate(green_modes = subway + commuter_rail + public_bus + walk + bicycle)

cbsa_cols <- cbsa_pop %>% select(GEOID, cbsa, state_abb, pop)
carpool_cols <- carpool %>% select(cbsa, state_abb, carpool_wrk_prc)
transit_cols <- transit %>% select(cbsa, state_abb, green_modes) ## combined mode share of transit, walk, bike
avo_cols <- avo %>% select(cbsa, state_abb, mean_avo) #mean vehicle occupancy
grid_cols <- grid %>% select(cbsa, state_abb, ren_prc) #% of consumption that is from renewable sources (hydro, wind, solar)
ev_prc_cols <- ev_prc %>% select(cbsa, state_abb, elec_n_hyb) #% of trips made in electric & hybrid cars
density_cols <- density %>% select(cbsa, state_abb, dens_enough) #% of census tracts above 10k people / sq mile
work_home_cols <- work_home %>% select(cbsa, state_abb, work_home_pct) #% of census tracts above 10k people / sq mile
density_change_cols <- density_change %>% select(cbsa, state_abb, HU_dens_inc_prc) # %increase in density of housing units per square mile from 2009-2018 

df <- cbsa_cols %>% 
    left_join(transit_cols, by = c("cbsa","state_abb")) %>%
    left_join(carpool_cols, by = c("cbsa","state_abb")) %>%
    left_join(avo_cols, by = c("cbsa", "state_abb")) %>%
    left_join(grid_cols, by = c("cbsa", "state_abb")) %>%
    left_join(ev_prc_cols, by = c("cbsa", "state_abb")) %>%
    left_join(work_home_cols, by = c("cbsa", "state_abb")) %>%
    left_join(density_cols, by = c("cbsa", "state_abb")) %>%
    #left_join(density_change_cols, by = c("cbsa","state_abb")) %>%
    filter(cbsa != "All" & cbsa != "Suppressed") %>% drop_na() %>% 
    mutate(workshop_id = "NA", scenario_name = "NA")

##set up icons

vars <- c("green_modes","mean_avo","ren_prc","elec_n_hyb","dens_enough","work_home_pct","carpool_wrk_prc","HU_dens_inc_prc")
icons <- c("/custom_icons/bus-circle.png", "/custom_icons/carpool-circle.png", "/custom_icons/bolt-circle.png", "/custom_icons/plug-circle.png", "/custom_icons/house-laptop-circle.png", "/custom_icons/house-laptop-circle.png", "/custom_icons/carpool-circle.png", "/custom_icons/leaf-circle.png") ## change these later but will work for now
pretty_names <- c("Sustainable Modes","Avg. Vehicle Occ.","% Grid Renewables","% Fleet EVs","% Dense Enough for Transit","% Telecommute","% Carpool Pct.","10 year % Increase in Housing Density")
pretty_df <- cbind(vars, pretty_names) %>% as.data.frame() %>% mutate(icons = paste0(getwd(),icons)) 
pretty_df$id <- 1:nrow(pretty_df)

gs4_deauth()

values <- reactiveValues(plot = NULL)

past_users <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1-2GqMyAq7vuzPNIJVVapjWg5B7AYnNMuX6VMNFc7E1s/edit#gid=2068996923",
  range = "Shiny_App_Input!B1:M501",
  col_names = TRUE,
  trim_ws = TRUE
) %>% mutate(cbsa = "Past User", state_abb = "PAST") %>% relocate (c(GEOID, cbsa, state_abb), .before = everything()) %>% select(-HU_dens_inc_prc) %>% drop_na()


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   
#past_users <- read.csv("./data_for_viz/dev_data.csv", header = TRUE) %>% 
#  mutate(cbsa = "Past User", state_abb = "PAST") %>% relocate (c(GEOID, cbsa, state_abb), .before = everything()) %>% drop_na()
     #   order so that Google Sheet values mirror spreadsheet, add in columns from population / CBSA sheet
    
    viz_cols <- reactive({ 
      
      var_names <- pretty_df$vars[pretty_names %in% input$metrics] #create list of variable names, where the pretty names match
      return(var_names)
      }) 
    
    model_cbsa <- reactive({ return(input$benchmarks)
      })
    
    user_vals <- reactive ({ c(99999,"User Input","UI",1000,input$green_modes,input$carpool_wrk_pct,
                               input$mean_avo,input$ren_prc,input$elec_n_hyb,input$work_home_pct,
                               input$dens_enough, #input$housing, #remove density metrics
                               input$workshop, input$nickname) }) #KEEP THESE IN SAME ORDER AS JOIN
    
    observe({ 
    print(ncol(df))
    print(length(user_vals()))
    print(ncol(past_users))
    
    print(names(df))
    print(user_vals())
    print(names(past_users))
    })
    
    viz_df <- eventReactive(c(input$green_modes,input$carpool_wrk_pct,input$mean_avo,input$ren_prc,input$elec_n_hyb,input$work_home_pct,input$dens_enough, input$metrics, input$nickname, input$workshop), { 
    
      #append user values to prepped data frame     
    df <- df %>% rbind(user_vals()) %>% rbind(past_users)

     #coerce into correct data format
    df[,1] <- sapply(df[,1], as.numeric) 
    df[,2:3] <- sapply(df[,2:3], as.character) 
    df[,4:11] <- sapply(df[,4:11], as.numeric) 
    df[,12:13] <- sapply(df[,12:13], as.character) 
  
      #pivot for table
    df <- df %>% pivot_longer(cols = viz_cols(), names_to = "metric", values_to = "percent") %>% arrange(pop)
    
      #add in pretty labels
    df <- df %>% left_join(pretty_df, by = c("metric" = "vars")) %>% mutate(pretty_names = fct_reorder(pretty_names, id))
    
    #create new column that's the same as the percent, but with unique values for vehicle occupancy. Decouple label from position.
    df$label <- df$percent #store most percentages as the labels
    df$percent[df$pretty_names == "Avg. Vehicle Occ."] <- (100/df$percent) #create percentage column for position.
    
    print(head(df %>% filter(pretty_names == "% Telecommute"), 10))
    
    return(df)
    
    }, ignoreNULL = FALSE)  #specify that it should do this, even if the inputs are NULL
    
    df_user <- reactive ({ viz_df() %>% filter(state_abb == "UI") })
    
    df_benchmark <- reactive ({ 
      
      if(is.null(input$benchmarks)) { models  <- viz_df() %>% filter(cbsa %in% "Boston-Cambridge-Newton") } #set default value
      if(!is.null(input$benchmarks)) { models  <- viz_df() %>% filter(cbsa %in% input$benchmarks) } 
      return(models)
      
      })
    
    df_past <- reactive ({ 
      
      if(input$workshop == "All") { viz <- viz_df() %>% filter(cbsa == "Past User") }
      if(input$workshop != "All") { viz <- viz_df() %>% filter(workshop_id == input$workshop)}
      
      return(viz)
      
      })

    output$plot <- renderPlotly({
      
    scale_factor <- (1000000)
    
    ## SET UP LAYER TOGGLES
  
    ## Steps to Fix Icons: (1) Add index number for each metric in input df (2) reorder metrics according to this index number (3) use this to set position of image
    
    color_scale <- c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17")
    
    p <- ggplot(viz_df() %>% filter(state_abb != "PAST" & state_abb != "UI") %>% arrange(cbsa), aes(alpha = 0.3, x = pretty_names, size = pop, y = percent, colour = pretty_names, label = icons,
                                                                                  weight = 2)) +
            ylim(0, 100) + ylab("%") +
            xlab(NULL) +
            theme(legend.title = element_blank(),  
                  axis.text.x = element_text(pretty_names, angle = 30, vjust = 0.5, hjust=1),
                  legend.position = "bottom") +
            scale_fill_discrete(labels = pretty_names) +
            scale_colour_discrete(labels = pretty_names) +
            scale_size_continuous(name = "Population", range = c(min(df$pop)/scale_factor, max(df$pop)/scale_factor), breaks = 5) 
   
    if (input$cities) {
      p <- p + geom_jitter(data = viz_df() %>% filter(cbsa != "User Input" & cbsa != "Past User") %>% arrange(cbsa), width = 0, alpha = 0.3, aes(color = pretty_names, alpha = 0.3,
                                                    text = paste("<b>",cbsa,"</b>", "<br>","Pop:",format(pop,big.mark=",",scientific=FALSE),"<br>", pretty_names, round(label, 2)))) 
        
      
    }
    
    if (input$model) {
      p <- p + geom_jitter(data = df_benchmark(), alpha = 0.5, width = 0, color = "navyblue", aes(x = pretty_names, y = percent, size = pop, fill = pretty_names,
                                              text = paste("<b>",cbsa,"</b>", "<br>","Pop:",format(pop,big.mark=",",scientific=FALSE),"<br>", pretty_names, round(label, 2)))) 

    }
    
    
    if (input$past) {
       
      p <- p + geom_jitter(data = df_past() %>% arrange(cbsa), width = 0, alpha = 1.0, color = "yellow", size = 2, shape = 18, 
                           aes(text = paste("<b>", "Scenario:", scenario_name, "</b>","<br>", pretty_names,":", round(label, 2), "%"))) 
      
    }
    
   # if (input$user) {
   #    p <- p + geom_jitter(data = df_user(), aes(x = pretty_names, y = percent, text = paste("<b> User Input </b>", "<br>", pretty_names, round(label, 2))), size = 2, width = 0)
  #    
  #  }
   
    fancy <- ggplotly(p, tooltip = c("text")) %>% add_lines(x=~2:2, y=~0:2, colors=NULL, width = 0, yaxis= "y2", 
                                                            data=viz_df(),
                                                            line = list(color = 'rgb(205, 12, 24)', width = 0), 
                                                            showlegend=FALSE, inherit=FALSE) %>% 
                                                           layout(showlegend = FALSE,
                                                                  yaxis2 = list(
                                                                    tickfont = list(size=12),
                                                                    titlefont= list(size=12),
                                                                    overlaying = "y",
                                                                    nticks = 2,
                                                                    side = "right",
                                                                    title = "Average Vehicle Occupancy (#)"
                                                                  ),
                                                                  
                                                                  margin = list(r = 11))
    
    for (i in 1:length(fancy$x$data)){ ##remove [,1] from plotly legend output
      if (!is.null(fancy$x$data[[i]]$name)){
        fancy$x$data[[i]]$name =  gsub("\\(","",str_split(fancy$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    if (input$user) {
      
      #idea -- subset icon_df where viz_switch is TRUE, assign x_pos for those values based on the length of that list, bind it back to the other ones, and set the other values equal to 0
      
      icon_df <- pretty_df %>% mutate(viz_switch = if_else(pretty_names %in% input$metrics, TRUE, FALSE))
      icon_true <- icon_df %>% filter(viz_switch == TRUE)
      icon_false <- icon_df %>% filter(viz_switch == FALSE)
      icon_true$x_pos <- 1:nrow(icon_true)
      #icon_true$x_pos <- as.integer(rownames(icon_true))/(nrow(icon_true)) * 0.91 ^ (1+ (nrow(icon_true) * .05)) # ^ 0.5 * (1+(0.08*nrow(icon_true)) ))
      icon_false$x_pos <- 0 
      icon_df <- rbind(icon_true, icon_false) %>% arrange(id)
      
      print(head(icon_df, 10))

     fancy <- fancy %>% 
      
        layout(       
        images = 
          
          list(
          
          list(
          source = base64enc::dataURI(file = icon_df$icons[pretty_names == "% Grid Renewables"]),
          x = icon_df$x_pos[pretty_names == "% Grid Renewables"], y = (input$ren_prc * .01),
          sizex = 0.2, sizey = 0.1,
          xref = "x", yref = "paper", 
          xanchor = "center", yanchor = "bottom",
          visible = icon_df$viz_switch[pretty_names == "% Grid Renewables"]
          ),
          
          list(
            source = base64enc::dataURI(file = icon_df$icons[pretty_names == "Avg. Vehicle Occ."]),
            x = icon_df$x_pos[pretty_names == "Avg. Vehicle Occ."], y = (input$mean_avo * .01),
            sizex = 0.2, sizey = 0.1,
            xref = "x", yref = "paper", 
            xanchor = "center", yanchor = "bottom",
            visible = icon_df$viz_switch[pretty_names == "Avg. Vehicle Occ."]
          ),
          
          list(
            source = base64enc::dataURI(file = icon_df$icons[pretty_names == "Sustainable Modes"]),
            x = icon_df$x_pos[pretty_names == "Sustainable Modes"], 
            y = (input$green_modes * .01),
            sizex = 0.2, sizey = 0.1,
            xref = "x", yref = "paper", 
            xanchor = "center", yanchor = "bottom",
            visible = icon_df$viz_switch[pretty_names == "Sustainable Modes"]
          ),
          
          list(
            source = base64enc::dataURI(file = icon_df$icons[pretty_names == "% Fleet EVs"]),
            x = icon_df$x_pos[pretty_names == "% Fleet EVs"], 
            y = (input$elec_n_hyb * .01),
            sizex = 0.2, sizey = 0.1,
            xref = "x", yref = "paper", 
            xanchor = "center", yanchor = "bottom",
            visible = icon_df$viz_switch[pretty_names == "% Fleet EVs"]
          ),
          
          list(
            source = base64enc::dataURI(file = icon_df$icons[pretty_names == "% Telecommute"]),
            x = icon_df$x_pos[pretty_names == "% Telecommute"], 
            y = (input$work_home_pct * .01),
            sizex = 0.2, sizey = 0.1,
            xref = "x", yref = "paper", 
            xanchor = "center", yanchor = "bottom",
            visible = icon_df$viz_switch[pretty_names == "% Telecommute"]
          ),
          
          list(
            source = base64enc::dataURI(file = icon_df$icons[pretty_names == "% Carpool Pct."]),
            x = icon_df$x_pos[pretty_names == "% Carpool Pct."], 
            y = (input$carpool_wrk_prc * .01),
            sizex = 0.2, sizey = 0.1,
            xref = "x", yref = "paper", 
            xanchor = "center", yanchor = "bottom",
            visible = icon_df$viz_switch[pretty_names == "% Carpool Pct."]
          )
          ),
        
        margin = list(t = 50) )
      
     
    }
  
  return(fancy) 

    })
    
    
    stat_table <- reactive ({ 
      
      print(head(viz_df()$work_home_pct, 10))
      
      a <- viz_df() %>% group_by(metric) %>% 
        summarise( 
            min = min(percent, na.rm = TRUE), 
            q25 = quantile(percent, 0.25, na.rm = TRUE), 
            median = median(percent, na.rm = TRUE), 
            q75 = quantile(percent, 0.75, na.rm = TRUE), 
            max = max(percent, na.rm = TRUE),
            mean = mean(percent, na.rm = TRUE), 
            sd = sd(percent, na.rm = TRUE))
      
      return(a) })
    
    output$stats <- DT::renderDataTable({ 
        
       #datatable(stat_table()) %>% formatRound(c(2:8), 2) 
        df <- as.tibble(stat_table())
        
        print(df)
        
        df <- df %>% left_join(pretty_df, by = c("metric" = "vars"), copy = TRUE) 
        
        df <- df %>% mutate(across(where(is.numeric), round, 2)) %>%
          rename("Variable" = pretty_names, "Minimum" = min, "25th %ile" = q25, "Median" = median, "75th %ile" = q75,
                 "Maximum" = max, "Average" = mean, "Stand. Dev." = sd) %>% select(-c("metric","icons","id")) %>% relocate(.col = Variable, .before = everything()) 
        
        return(df)
        })
    
    output$city_data <- DT::renderDataTable({
      
      city_data <- as.tibble(df) %>% select(-c("scenario_name","workshop_id"))
      
      city_data <- city_data %>% mutate(across(where(is.numeric), round, 2))
      
      colnames(city_data)[2:4] <- c("City","State","Population")
      colnames(city_data)[5:11] <- pretty_df$pretty_names[match(names(city_data[5:11]),pretty_df$vars)]
      
      return(city_data)
        
    }, options = list(dom = 't'))
    
}


# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Decarbonizing Transport", theme = shinytheme("flatly"),
                         
                         tabPanel('Main',
                                  
                                  fluidRow(
                                      column(width = 4,
                                             wellPanel(
                                                 p(h4("Instructions")),
                                                 p("This tool helps display your mix of decarbonization strategies, and compare them to the status quo in U.S. cities, as well as the strategies created by other workshop participants."), 
                                                 p("Mouse over the plot for details, and enter your results on the", em("Submit Your Results"), "page."),
                                                 selectizeInput('benchmarks', label = em("1. Select Benchmark Cities"), choices = unique(df$cbsa), selected = "Boston-Cambridge-Newton", multiple = TRUE),
                                                 selectizeInput('metrics', label = em("2. Select Metrics"), choices =  pretty_df$pretty_names[!grepl("Dens", pretty_names)],  ## taking out density metrics.
                                                                selected = c("Sustainable Modes","Avg. Vehicle Occ.","% Grid Renewables","% Fleet EVs","% Carpool Pct.","% Telecommute"),
                                                                options = list(maxItems = 8), 
                                                                multiple = TRUE),
                                                 
                                                 p(em(strong("3. Enter your values (default are U.S. averages)."))),
                                                 
                                                 fluidRow(
                                                 
                                                   column(6,  
                                                          textInput('nickname', label = "Scenario Name", value = "My Scenario", placeholder = "My Scenario"),
                                                          numericInput('green_modes', label = "Transit Mode Share", min = 0, max = 100, value = round(mean(df$green_modes),2), step = 5),
                                                          numericInput('mean_avo', label = "Average Vehicle Occupancy", min = 0, max = 100, value = round(mean(df$mean_avo),2), step = 5),
                                                          numericInput('elec_n_hyb', label = "EV Fleet Share", min = 0, max = 100, value = round(mean(df$elec_n_hyb), 2), step = 5),
                                                           ),
                                                   column(6, 
                                                          numericInput('carpool_wrk_prc', label = "Commuting by Carpool", min = 0, max = 100, value = round(mean(df$carpool_wrk_prc), 2), step = 5),
                                                          #numericInput('dens_enough', label = "% Area with Transit Supportive Density", min = 0, max = 100, value = round(mean(df$dens_enough), 2),  step = 5),
                                                          numericInput('ren_prc', label = "Grid Renewable %", min = 0, max = 100, value = round(mean(df$dens_enough), 2),  step = 5), 
                                                          #numericInput('housing', label = "10-year % Density Increase", min = 0, max = 100, value = round(mean(df$HU_dens_inc_prc), 2),  step = 1),
                                                          numericInput('work_home_pct', label = "Work from Home %", min = 0, max = 100, value = round(mean(df$work_home_pct), 2), step = 5),
                                                          p(em("Data Sources: National Household Travel Survey (2017), ACS 5-year Survey, Energy Information Administration"), style = "font-size:80%")
                                                          )
                                                 ),
                                              
                                                
                                          )
                                      ),
                                      
                                      column(width = 8,
                                             
                                             fluidRow(
                                             column(width = 10, plotlyOutput('plot')),
                                             column(width = 2, absolutePanel(top = "20px", right = "20px", bottom = "auto", left = "auto", draggable = TRUE, height = "auto",
                                                                             width = "120px",
                                                                             
                                                                             strong('Select Layers'),
                                                                             
                                                                             checkboxInput('cities', label = p(img(src='green-bubbles.png', align = "left", height = 18, width= 18, style = "margin:5px"), "All Cities"), value = TRUE),
                                                                             checkboxInput('model', label =  p(img(src='empty-circle.png', align = "left", height = 18, width= 18, style = "margin:5px"), "Model Cities"),  value = TRUE), #fa("circle", fill = "blue"), 
                                                                             checkboxInput('user', label = p(img(src='bus-circle.png', align = "left", height = 18, width= 18, style = "margin:5px"), "Input Values"), value = TRUE), #fa("square", fill = "gray34")
                                                                             checkboxInput('past', label = p(img(src='yellow-diamond.png', align = "left", height = 18, width= 18, style = "margin:5px"), "Past User Values"), value = TRUE),
                                                                             selectizeInput('workshop', label = p(strong("Select Workshop")), choices = c(past_users$workshop_id, "All"), selected = "All")
                                                      ))             
                                             ),
                                             
                                             fluidRow(
                                                 wellPanel(DT::dataTableOutput('stats'))
                                             ),
                                             
                                          
                                      )
                                      
                                  )
                                  
                         ),
                         tabPanel('Submit Your Results',
                                  
                                  
                                  HTML('<b>','Submit Your Results!','</b>'),
                                  
                                  tags$iframe(src = "https://docs.google.com/forms/d/e/1FAIpQLSdTNnNuPps91P4W78kKgrV78QQEmAok1DudLp4Y6tKrD1St4A/viewform?embedded=true",
                                              width = 640,
                                              height = 1051,
                                              frameborder = 0,
                                              marginheight = 0)
                            
                                  
                                  #set up google form, per these instructions: 
                                  #https://blog.usejournal.com/how-to-save-user-responses-in-your-google-spreadsheet-using-r-shiny-fa35775aab90
                                  
                                  ),
                         tabPanel('The Data',
                                  DT::dataTableOutput('city_data'),
                                  p("Transportaion data are taken from the", a("National Household Travel Survey", href = "https://nhts.ornl.gov/"), "Energy data are from the", a("US Energy Information Administration (2019).", href = "https://www.eia.gov/state/seds/seds-data-complete.php?sid=US")),
                                  p('City data reflect Core Based Statistical Areas (CBSA), per the 2019 American Community Survey. Renewable energy data are taken from the state-level and applied to each CBSA within the state (i.e., Buffalo and New York City are assumed to have the same energy mix).')
                                  ),
                         tabPanel('About',
                                  
                                  p("Cities have set long-range goals to create a zero-emissions transportation system by 2050, consistent with IPCC goals. What mix of interventions will it take to get there? This tool complements the Decarbonizing Transportation workshop, and helps participants compare their strategies to major US cities."),
                                  p("This platform reflects the Decarbonizing Transportation Model built by", a("Andrew Salzberg.", href = "https://www.linkedin.com/in/andrew-salzberg-a9625718/"), "Access the full Excel model", a("here.", href = "https://docs.google.com/spreadsheets/d/1odc3-H2BlM42PjJ-xAuPcuv7GDYESJXDWqADQwoy4TM/edit#gid=143699780"),
                                  p("Subscribe to the Decarbonizng Transportation newsletter", a("here.", href = "https://decarbonizingtransportation.substack.com/")),
                                  p("Platform developed by", a("Emmett McKinney. Last updated January 1, 2021.", href = "https://www.emmettz.com")))
                         )
                                  
                         
)                                            
)


# Run the application 
shinyApp(ui = ui, server = server)
