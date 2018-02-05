
ui <- dashboardPage(
  dashboardHeader(title = "Project Performance"),
  dashboardSidebar(sidebarMenu(
    menuItem("Contacts", tabName = "contacts", icon = icon("users")),
    menuItem("Companies" ,tabName = "companies" , icon = icon("th-list"))
  )),
  dashboardBody(
    tabItems(
      tabItem("contacts",
              fluidRow(
                box(plotlyOutput("overall_progress" ,  height = 125),status = "primary", width = "100%")
              ),
              fluidRow(
                box(width="100%",
                valueBoxOutput("Leads" , width="2"),
                tags$style("#Leads {width:16.6%;}"),
                valueBoxOutput("New" , width="2"),
                tags$style("#New {width:16.6%;}"),
                valueBoxOutput("follow_up" , width="2"),
                tags$style("#follow_up {width:16.6%;}"),
                valueBoxOutput("notInterested" , width="2"),
                tags$style("#notInterested {width:16.6%;}"),
                valueBoxOutput("no_response" , width="2"),
                tags$style("#no_response {width:16.6%;}"),
                valueBoxOutput("wrong" , width="2"),
                tags$style("#wrong {width:16.6%;}")
                ),
                
                tags$style("box1 {width:100%;}")
              ),
              fluidRow(
                
                
                tabBox(width = "100%" ,
                  tabPanel("Calls",box( status = "warning",width = "100%",
                                        column(12, align="center"),
                                        plotlyOutput("initiated_progress" ))),
                  tabPanel("Leads",box( status = "success", width = "100%",
                                        column(12, align="center"),
                                        plotlyOutput("leads" , height = 250))),
                  tabPanel("Follow-ups",box( status = "info", width = "100%",
                                            column(12, align="center"),
                                            plotlyOutput("follow"))),
                  tabPanel("Phone Type",box(  width = "100%",
                                            column(12, align="center"),
                                            plotlyOutput("phone_type"))),
                  tabPanel("Point Of Contact",box(  width = "100%",
                                              column(12, align="center"),
                                              plotlyOutput("poc_contact")))
                  
                ))
      ),
      tabItem("companies",
              fluidRow(
                box(plotlyOutput("overall_progress_comp" ,  height = 125),status = "primary", width = "100%")
              ),
              fluidRow(
                box( status = "warning",width = "100%",
                     column(12, align="center"),
                     plotlyOutput("POC" , height = 500)))
              )
      )
    )
  )



