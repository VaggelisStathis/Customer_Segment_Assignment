
source('global.R')


shinyUI(
  navbarPage("EFOOD",#superhero, slate
             tabPanel('Data Tables',     DT::dataTableOutput('table')),
             tabPanel("Segment",
                      sidebarLayout(
                        sidebarPanel(
                          
                          hr(),
                          fluidRow(
                            h2("Segment Selection"),
                            column(12, hr(),
                                   textOutput("Explanation")
                            ),
                          ),
                          fluidRow(
                            
                            column(12, textOutput("Preselection")
                                   
                                   
                            ),
                            
                            
                          ),
                          fluidRow(
                            column(4,
                            ),
                            
                            
                          ),
                          
                          hr(),
                          
                          
                        ),
                        
                        mainPanel(
                          
                          fluidRow(h3("Parcel Frequency by Date"),
                                   column(3, 
                                          checkboxGroupInput("FBRScore1", label = h3("Select FBRScore"), 
                                                             choices = list(1,2,3,4,5),
                                                             selected = list(5))
                                   ),
                          ),
                          hr(),
                          fluidRow(
                                   column(12,
                                          echarts4rOutput("piechart"),
                                          
                                   ),
                          ),
                          fluidRow(
                                   column(12,
                                       
                                          
                                   ),
                          ),
                          fluidRow(
                                   column(12,
                                     
                                          
                                   ),
                          ),
                          
                        )
                        
                      )),
             
             tabPanel("Campaign",
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            h2("Campaign Selection"),
                            column(12, hr(),
                                   textOutput("NumUsers")
                            ),
                          ),
                          hr(),
                          fluidRow(
                            
                            column(6,
                                   ),
                            
                            
                          ),
                          fluidRow(
                            column(4,
                            ),
                            
                            
                          ),
                          
                          hr(),
                          
                          
                        ),
                        
                        mainPanel(
                          
                          fluidRow(
                                   column(3, 
                                          
                                          checkboxGroupInput("MFWBRScores", label = h3("Select Segments"), 
                                                             choices = segments,
                                                             selected = list("5 4"))
                                         # selectInput('in6', 'Options', Query1$city, selected = Query1$city, multiple=TRUE, selectize=TRUE)
                                   ),
                                   column(6,
                                                 
                                          echarts4rOutput('gauge')
                                   ),
      
                                   column(3, 
                                          
                                          checkboxGroupInput("FBRScore", label = h3("Select FBRScore"), 
                                                             choices = list(1,2,3,4,5),
                                                             selected = list(1,2,3,4))
                                          # selectInput('in6', 'Options', Query1$city, selected = Query1$city, multiple=TRUE, selectize=TRUE)
                                   ),
                          ),
                          hr(),
                          
                          fluidRow(
                                   column(12,
                                        #  DT::dataTableOutput("table")
                                          
                                   ),
                          ),
                          fluidRow(
                                   column(12,
                                          #    plotOutput("bus_days_plot")
                                          
                                   ),
                          ),
                          
                        )
                        
                      )),
             
             
             tags$head(
               tags$style(
                 HTML(".info-box{margin-bottom:50px;}")
               )
               
               #tags$style("#Feedback_Table{color: gray;
               #                              font-size: 12px;
               #                           font-style: italic;
               #                           }"
               # ),
               
             )
             
  )
  
)
