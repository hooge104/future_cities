navbarPage("Future cities", id="nav", theme = "styles.css",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                              tags$head(tags$style(type = "text/css", "#table_city th {display:none;}")),
                              
                              leafletOutput("map", width="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = "350", height = "auto",
                                            
                                            selectInput("variable", label = "Climate variable",
                                                        colnames(cities_data[c(6,5,7,8)]),
                                                        selected = colnames(cities_data[6])),
                                            
                                            # plotOutput("plot", height = 200),
                                            
                                            h2(fluidRow(verbatimTextOutput("Click_text"))),
                                            
                                            dataTableOutput("table_city"),
                                            
                                            
                                            a("Data from Bastin et al., Plos One 2019", href="https://www.crowtherlab.com/")
                                            
                              )
                              
                          )
                          
                 ),
                 
                 tabPanel("Data explorer",
                          dataTableOutput("table_total"))
)