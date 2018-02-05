server <- function(input, output) { 
  
  
  
  output$noMemory<- renderValueBox({
    
    valueBox(
      value = weekly$Does.Not.remember[nrow(weekly)],
      subtitle = HTML(paste("Contacts can't", "rembember ordering", sep="<br/>")),
      icon = icon("bell-slash-o"),
      color = "purple"
    )
  })
  
  output$completed <- renderValueBox({
    
    valueBox(
      value = weekly$Completed[nrow(weekly)],
      subtitle = HTML(paste("Completed", "Surveys", sep="<br/>")),
      icon = icon("list-ol"),
      color = "green"
    )
  })
  
  
  output$p_completed <- renderValueBox({
    
    valueBox(
      value = weekly$Partially.Completed[nrow(weekly)],
      subtitle = HTML(paste("Partially", "Completed Surveys", sep="<br/>")),
      icon = icon("list-ul"),
      color = "olive"
    )
  })
  
  output$follow_up <- renderValueBox({
    
    valueBox(
      value = weekly$Follow.up[nrow(weekly)],
      subtitle = HTML(paste("Contacts to", "Follow-Up with", sep="<br/>")),
      icon = icon("refresh"),
      color = "teal"
    )
  })  
  
  output$no_response <- renderValueBox({
    
    valueBox(
      value = weekly$No.response[nrow(weekly)],
      subtitle = HTML(paste("Contacts", "not responding", sep="<br/>")),
      icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  output$wrong <- renderValueBox({
    
    valueBox(
      value = weekly$Wrong.Person[nrow(weekly)],
      subtitle = HTML(paste("Wrong / Unknown", "Contact info", sep="<br/>")),
      icon = icon("question"),
      color = "maroon"
    )
  })
  
  
  output$overall_progress<- renderPlotly({
    plot_ly(progress, x = ~Initiated, y = ~y, type = 'bar', orientation = 'h', marker = list( color = 'rgb(255, 132, 26)',width = 1) , name = 'Survey Initiated', hoverinfo="x")%>%
      add_trace(x = ~Not_Initiated ,  marker = list(color = 'rgba(240, 142, 29, 0.45)') , name = 'Survey Not Initiated' , hoverinfo="x") %>% 
      layout(title='Overall Progress',
             xaxis = list(title = paste0("Last Update on ", format(Sys.Date(), "%m/%d/%Y"))),
             yaxis = list(title = "",
                          showticklabels = FALSE),
             barmode = 'stack',
             showlegend = F) %>% 
      add_annotations(xref = 'x', yref = 'paper',
                      x = 50, y = 0.5,
                      text = paste0("Survey Initiated with ", initiated , " (",round(progress$Initiated[1],2), '%',")" , " contacts"),
                      font = list(family = 'Arial', size = 14,color = 'rgb(0, 0, 0)'),
                      showarrow = FALSE)
  }
  )
  
  output$initiated_progress<- renderPlotly({
    colors <- c('rgb(0, 166, 90)', 'rgb(95, 93, 168)' ,'rgb(56, 204, 204)', 'rgb(187, 64, 48)', 'rgb(62, 153, 112)', 'rgb(217, 26, 96)')
    plot_ly(initiated_df, 
            labels = ~type, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = 'Status of the Initiated Calls',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  ############# Weekly ################


  
  output$surveyWeekly<- renderPlotly({
    plot_ly(weekly_tracker) %>% 
      add_trace(y = ~Week, x = ~Completed, type = 'bar', orientation='h',
                hoverinfo = "x",
                text = weekly_tracker$Completed, textposition = 'auto',
                marker = list(color = 'rgb(0, 166, 90)',line = list(color = 'rgb(0, 0, 0)', width = 1.5))
                ) %>% 
      add_trace(y = ~Week, x = ~Success.rate, 
                type = 'scatter', mode = 'lines+markers' , 
                hoverinfo = "x",
                marker = list(size = 15 , color ='rgb(255, 132, 26)' ) ,line = list(dash = 'dot' ,width = 6, color= 'rgb(255, 132, 26)') ,
                xaxis="x2") %>% 
      layout(title = '',
             showlegend = FALSE,
             yaxis = list(title = "Week"),
             xaxis = list(side = 'top', title = '# Completed Surveys', showgrid = F, zeroline = F,color='rgb(0, 166, 90)'),
             xaxis2 = list(side = 'bottom', overlaying = "x", title = 'Success Rate (%)', showgrid = T, zeroline = FALSE,color='rgb(255, 132, 26)'),
             margin = list( t = 40)
             )
    
  })
  
  output$InitiatedWeekly <- renderPlotly({
    plot_ly(weekly_tracker, x = ~Completed, y = ~Week, type = 'bar', orientation = 'h',
            marker = list(color = 'rgb(0, 166, 90)',line = list(color = 'rgb(248, 248, 249)', width = 1)),
            hoverinfo="x" , name = "Completed Surveys") %>%
      add_trace(x = ~Partially.Completed, marker = list(color = 'rgb(62, 153, 112)') , name = "Partially Completed") %>%
      add_trace(x = ~Follow.up, marker = list(color = 'rgb(56, 204, 204)') , name = "Follow-up") %>%
      add_trace(x = ~Does.Not.remember, marker = list(color = 'rgb(95, 93, 168)') ,  name = "Does Not Remember") %>%
      add_trace(x = ~No.response, marker = list(color = 'rgb(187, 64, 48)') , name = "No Response") %>%
      add_trace(x = ~Wrong.Person, marker = list(color = 'rgb(217, 26, 96)') , name= "Wrong Person") %>% 
      layout(title = '',
             xaxis = list(title = "#Contacts",
                          showgrid = T,
                          showline = T,
                          showticklabels = T,
                          zeroline = T),
             yaxis = list(title = "Week",
                          showgrid = FALSE,
                          showline = FALSE,
                          showticklabels = T,
                          zeroline = FALSE),
             barmode = 'stack',
             showlegend = T,
             legend = list(orientation = 'h' , y=-0.15)
             )
  })
  

  
  ### Survey Results
  
  output$NPS_SCORE<- renderValueBox({
    
    valueBox(
      value = round((NPS_groups$Count[ NPS_groups$overall_NPS_NPS_GROUP  == "Promoter"] - NPS_groups$Count[ NPS_groups$overall_NPS_NPS_GROUP  == "Detractor"])*100/ nrow(survey) ,2),
      subtitle = HTML(paste("NPS Score", sep="<br/>")),
      icon = icon("code-fork" ,lib = "font-awesome"),
      color = "orange"
    )
  })
  
  output$NES_SCORE<- renderValueBox({
    
    valueBox(
      value = round((NES_groups$Count[ NES_groups$nes_Group == "Easy"] - NES_groups$Count[ NES_groups$nes_Group == "Difficult"])*100/ nrow(survey) ,2),
      subtitle = HTML(paste("NES Score", sep="<br/>")),
      icon = icon("certificate" , lib = "font-awesome"),
      color = "red"
    )
  })
  
  output$NPS <- renderPlotly({
    plot_ly(diamonds %>%  filter(cut == "NPS"), x = ~n, y = ~cut, color = ~Questions, 
            type = "box" , orientation = "h",
            hoverinfo = 'x') %>%
      layout(boxmode = "group",
             title = '',
             showlegend = T,
             xaxis = list(title = "Response"),
             yaxis = list(title = ""))
  })
  
  output$NPS_group <- renderPlotly({

    
    
    plot_ly(NPS_groups, 
            labels = ~overall_NPS_NPS_GROUP, values = ~Count,
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent',
            marker = list(colors = c("red","orange" , "green"),
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      add_pie(hole = 0.35) %>%
      layout(title = 'NPS Groups',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$NES <- renderPlotly({
    plot_ly(diamonds %>%  filter(cut == "NES"), x = ~n, y = ~cut, color = ~Questions,
            type = "box" , orientation = "h",
            hoverinfo = 'x') %>%
      layout(boxmode = "group",
             title = '',
             showlegend = T,
             xaxis = list(title = "Response"),
             yaxis = list(title = ""))
  })
  
  output$NES_group <- renderPlotly({
    plot_ly(NES_groups, 
            labels = ~nes_Group, values = ~Count, 
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+percent',
            marker = list(colors = c("red","green","orange"),
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      add_pie(hole = 0.35,
              rotation = 90) %>%
      layout(title = 'NES Groups',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
}

