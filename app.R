library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(htmltools)
library(DT)
library(reshape2)

cities_data <- read.csv("data/future_cities_data.csv") %>% 
  select(Longitude, Latitude, current_city,future_city_1_source,change_Annual_Mean_Temperature,change_Max_Temperature_of_Warmest_Month,change_Min_Temperature_of_Coldest_Month) %>% 
  rename("Future_climate" = future_city_1_source )  %>% 
  rename("Increase in Annual Temperature" = change_Annual_Mean_Temperature) %>% 
  rename("Increase in Temperature of Warmest Month" = change_Max_Temperature_of_Warmest_Month) %>% 
  rename("Increase in Temperature of Coldest Month" = change_Min_Temperature_of_Coldest_Month) %>% 
  rename("latitude" = Latitude) %>% 
  rename("longitude" = Longitude)
cities_data[-c(1,2,3,4)] <- round(cities_data[-c(1,2,3,4)], 1)

cities_display <- cities_data %>% 
  select(current_city,Future_climate,"Increase in Annual Temperature","Increase in Temperature of Warmest Month","Increase in Temperature of Coldest Month") %>% 
  rename("City" = current_city) %>% 
  rename("Future climate" = Future_climate)

cities_exerpt <- cities_display %>% 
  melt(id.vars = "City")

cities_plot <- read.csv("data/cities_plot.csv", stringsAsFactors = FALSE) %>% 
  mutate(variable = replace(variable, variable == "Annual Mean Temperature", "Annual")) %>% 
  mutate(variable = replace(variable, variable == "Maximum Temperature of Warmest Month", "Warmest Month")) %>% 
  mutate(variable = replace(variable, variable == "Minimum Temperature of Coldest Month", "Coldest Month"))
cities_plot$time <- as.factor(cities_plot$time)

ui <- navbarPage("Future cities", id="nav", theme = "styles.css",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                              tags$head(tags$style(type = "text/css", "#table_city th {display:none;}")),
                              
                              leafletOutput("map", width="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            
                                            selectInput("variable", label = "Select changing climate variable",
                                                        colnames(cities_data[c(6,5,7)]),
                                                        selected = colnames(cities_data[6])),
                                            
                                            h2(fluidRow(verbatimTextOutput("Click_text"))),
                                            
                                            dataTableOutput("table_city"),
                                            
                                            plotOutput("plot", height = 150),
                                            
                                            a("Data from Bastin et al., Plos One 2019", href="https://www.crowtherlab.com/")
                                            
                              )
                              
                          )
                          
                 ),
                 
                 tabPanel("Data explorer",
                          dataTableOutput("table_total"))
)

server <- function(input, output) {
  
  # This reactive is responsible for maintaining the color palette for the circles and legend,
  # according to the variable the user has chosen to map to color
  palette <- reactive({
    colorNumeric(
      palette = "YlOrRd",
      domain = cities_data[[input$variable]])
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet(cities_data) %>%
      addProviderTiles(providers$CartoDB.Positron)  
  })
  
  # Change palette of color circles, remove previously existing points
  observe({
    palette_circles <- colorNumeric(
      palette = "YlOrRd",
      domain = cities_data[[input$variable]])
    
    leafletProxy("map", data = cities_data) %>% 
      clearMarkers() %>% 
      addCircleMarkers(color = ~palette_circles(cities_data[[input$variable]]),
                       stroke = FALSE, 
                       fillOpacity = 0.5,
                       label = ~htmlEscape(cities_data$current_city),
                       layerId = cities_data$current_city,
                       group = cities_data[[input$variable]])
  })
  
  # Remove any existing legend, create a new one.
  observe({
    leafletProxy("map", data = cities_data) %>% 
      clearControls() %>% 
      addLegend("bottomleft", 
                pal = palette(), 
                values = cities_data[[input$variable]],
                title = "°C")
  })
  
  # Add dataframe as table
  output$table_total <- DT::renderDataTable({
    cities_display
  })
  
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    selected_city <- click$id
    text1 <- paste("Selected city:", click$id)
    text2 <- paste("Annual Temperature",click$group)
    output$Click_text<-renderText({
      text1
    })
  })
  
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    
    cities_show <- cities_exerpt %>% 
      filter(City == click$id) %>% 
      select(-City)
    
    output$table_city <- DT::renderDataTable({
      datatable(cities_show, rownames = FALSE,
                options = list(autoWidth = TRUE,
                               searching = FALSE,
                               info = FALSE,
                               paging = FALSE
                ))
      
    })
    
    cities_plotdata <- cities_plot %>% 
      filter(City == click$id) 
    
    output$plot <- renderPlot({ 
      ggplot(data = cities_plotdata, aes(x= time, y = value)) +
        geom_bar(stat = "identity", aes(fill = variable, alpha = 0.8)) +
        xlab("") + ylab("Degrees Celcius") +
        facet_wrap(vars(variable), nrow = 1, scales = "free") +
        theme_minimal() +
        scale_fill_manual(values = brewer.pal(name = "YlOrRd", n = 8), guide = "none") +
        theme(panel.grid.minor = element_blank(),
              legend.position = "none")
      
      })
     
  })
  
}

shinyApp(ui, server)


