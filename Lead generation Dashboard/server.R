server <- function(input, output) { 
  
  
  output$Leads <- renderValueBox({
    valueBox(
      value = initiated$Count[initiated$Call.Status == "Lead"],
      subtitle = HTML(paste("Leads", "Generated", sep="<br/>")),
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$New <- renderValueBox({
    valueBox(
      value = new,
      subtitle = HTML(paste("New Contacts", "Added", sep="<br/>")),
      icon = icon("address-card-o"),
      color = "blue"
    )
  })
  
  output$follow_up <- renderValueBox({
    valueBox(
      value = initiated$Count[initiated$Call.Status == "Follow-up"],
      subtitle = HTML(paste("Contacts", "in Follow-Up", sep="<br/>")),
      icon = icon("refresh"),
      color = "teal"
    )
  })  
  
  output$no_response <- renderValueBox({
    valueBox(
      value = initiated$Count[initiated$Call.Status == "No response"],
      subtitle = HTML(paste("Contacts", "not responding", sep="<br/>")),
      icon = icon("question-circle-o"),
      color = "red"
    )
  })
  
  output$notInterested <- renderValueBox({
    valueBox(
      value = initiated$Count[initiated$Call.Status == "Not Interested"],
      subtitle = HTML(paste("Not", "Interested", sep="<br/>")),
      icon = icon("thumbs-down"),
      color = "purple"
    )
  })
  
  output$wrong <- renderValueBox({
    
    valueBox(
      value = initiated$Count[initiated$Call.Status == "Wrong POC"],
      subtitle = HTML(paste("Wrong / Unknown", "Contact info", sep="<br/>")),
      icon = icon("user-times"),
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
                      text = paste0("Calls Initiated with ", sum(initiated$Count) , " (",round(progress$Initiated[1],2), '%',")" , " contacts"),
                      font = list(family = 'Arial', size = 14,color = 'rgb(0, 0, 0)'),
                      showarrow = FALSE)
  }
  )
  
  output$initiated_progress<- renderPlotly({
    colors <- c('rgb(56, 204, 204)','rgb(0, 166, 90)', 'rgb(187, 64, 48)','rgb(95, 93, 168)', 'rgb(217, 26, 96)')
    plot_ly(initiated, 
            labels = ~Call.Status, values = ~Count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+value',
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
  
  output$leads<- renderPlotly({
    plot_ly(leads, y = ~Lead...Status, x = ~Count, type = 'bar',
            orientation = 'h',
            text = ~Count, textposition = 'auto' , 
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5)))%>%
      layout(title = "Lead Status",
             xaxis = list(title = "#Contacts"),
             yaxis = list(title = ""))
  })
  
  output$follow<- renderPlotly({
    colors <- c('rgb(0, 166, 90)', 'rgb(95, 93, 168)' ,'rgb(56, 204, 204)', 'rgb(187, 64, 48)', 'rgb(62, 153, 112)', 'rgb(217, 26, 96)')
    plot_ly(follow_up, 
            labels = ~Follow...Up, values = ~Count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+value',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = 'Follow-up Type',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$phone_type <- renderPlotly({
    colors <- c('rgb(0, 166, 90)', 'rgb(95, 93, 168)' ,'rgb(56, 204, 204)', 'rgb(187, 64, 48)', 'rgb(62, 153, 112)', 'rgb(217, 26, 96)')
    plot_ly(phone_type, 
            labels = ~Phone.Type, values = ~Count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+value',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = 'Phone Type of Initiated Calls',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$poc_contact <- renderPlotly({
    colors <- c('rgb(0, 166, 90)', 'rgb(95, 93, 168)' ,'rgb(56, 204, 204)', 'rgb(187, 64, 48)', 'rgb(62, 153, 112)', 'rgb(217, 26, 96)')
    plot_ly(important, 
            labels = ~Point.of.Contact.Status, values = ~Count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+value',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = 'Point of Contact Status of the Initiated Calls',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ######Companies Tab#######
  output$overall_progress_comp<- renderPlotly({
    plot_ly(progress_companies, x = ~Initiated, y = ~y, type = 'bar', orientation = 'h', marker = list( color = 'rgb(255, 132, 26)',width = 1) , name = 'Survey Initiated', hoverinfo="x")%>%
      add_trace(x = ~Not_Initiated ,  marker = list(color = 'rgba(240, 142, 29, 0.45)') , name = 'Survey Not Initiated' , hoverinfo="x") %>% 
      layout(title='Overall Progress',
             xaxis = list(title = paste0("Last Update on ", format(Sys.Date(), "%m/%d/%Y"))),
             yaxis = list(title = "",
                          showticklabels = FALSE),
             barmode = 'stack',
             showlegend = F) %>% 
      add_annotations(xref = 'x', yref = 'paper',
                      x = 50, y = 0.5,
                      text = paste0("Calls Initiated with ", Companies_called , " (",round(perc_comp,2), '%',")" , " companies"),
                      font = list(family = 'Arial', size = 14,color = 'rgb(0, 0, 0)'),
                      showarrow = FALSE)
  }
  )
  
  output$POC <- renderPlotly({
    colors <- c('rgb(95, 93, 168)' , 'rgb(0, 166, 90)')
    plot_ly(important_companies, 
            labels = ~imp, values = ~Count,
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'label+value',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      add_pie(hole = 0.35 ,
              rotation = 50) %>%
      layout(title = 'Sucess Rate of Identifying the Right POC in each Company',
             font = list(
               family = "sans serif",
               size = 14.5),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
}