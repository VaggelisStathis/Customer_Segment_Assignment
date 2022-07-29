
source('global.R')

shinyServer(
  function(input, output,session) {
    
    ##################tab1##########################
    
    output$table <- DT::renderDataTable(
      DT::datatable(
        Queries, options = list(
          paging = FALSE, searching = FALSE
        )
      )
    )
    
    
    
    ##################tab2##########################
    
    output$Explanation <- renderText({ 
    "In order to  choose the proper segments for the 'Breakfast' campaign, 
      we created the following segments based on the orders frequency (without breakfast) 
      and the monetary  quadrants. Thus, a user that has monetary score equal to one belongs in the first quadrant.
      In order to match the assessments instructions we created an extra group for the 'FBRScore' and the 'FWBRScore' 
      variables which in both cases sets the upper limit for the last quadrant a bit further. It is very easy for the user though, to
      select both these groups (4 and 5). The segments generated with this formation are 20, which is the four groups for 'MScore' multiplied 
      with the five groups of the 'FWBRScore'."
    })
    
    output$Preselection <- renderText({ 
      "We chose to demonstrate the data for the Customers that belong to the fifth 'FBRScore' group. Thus, these customers made more than three breakfast purchases on January.
      The diagram  showcases that if we target the customers who belong in this group (and they don't belong in the FBRScore group already), we will maximize the possibility to turn this   
      customer into a loyal breakfast customer. Customers that belong to this segment (5 4), pay more money than the 75% of the customers and use the efood app more often than the 75% of the customers."
    })
    
    SegmentData <- reactive({df1 %>%
        filter(FBRScore %in% input$FBRScore1) %>% 
        group_by(MFWBRScores)%>%
        dplyr::summarise(Count = n(), .groups = "drop")
    })
    
    output$piechart <- echarts4r::renderEcharts4r({
      
      SegmentData() %>%  e_charts(MFWBRScores) %>%
        e_pie(Count, names = "Users") %>%
        e_tooltip(formatter = htmlwidgets::JS("
                                        function(params)
                                        {
                                            return `<strong>${params.name}</strong>
                                                    <br/>Total: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
                                        }  ")) %>%
        e_title("Frequency of Users Per Segment",
                subtext = "FWBRScore, Mscore")%>%
        e_legend(show = FALSE)
    })
    
    
    
    
    ##################tab3##########################
    
    
    
   AppData  <- reactive({df1 %>%
        filter(MFWBRScores %in% input$MFWBRScores) %>%  
        filter(FBRScore %in% input$FBRScore) %>% 
        group_by(user_id) %>%  
        dplyr::summarize(temp = n_distinct(user_id))  %>% 
        dplyr::summarize(NumOfUsers = sum(temp))
    })
   

    
   output$gauge <- echarts4r::renderEcharts4r({
       e_charts() %>%
       e_gauge(round(AppData()$NumOfUsers/121065*100,2), "PERCENT") })
   
   
   output$NumUsers <- renderText({ 
     paste("For the given selection the campaign will take affect on", AppData()$NumOfUsers, " users, which corresponds to ",
           round(AppData()$NumOfUsers/121065*100,2), "% of the total number of users. If this a large number of customers then 
           we could apply more restrictions. For instance, we could choose users that live in the cities that have significant smaller percentage of 'loyal breakfast customers' than the loyal customers like 'Larissa'." )
     })
   
   
 
    
    output$out6 <- renderPrint(round(AppData()$NumOfUsers/121065*100,2))    

    
    
  }
)