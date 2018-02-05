
ui <- dashboardPage(
  dashboardHeader(title = "Survey Performance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Weekly Progress" ,tabName = "weekly" ,  icon = icon("calendar")),
      menuItem("Survey Results",tabName = "survey" ,  icon = icon("list-ol"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(plotlyOutput("overall_progress" ,  height = 125),status = "primary", width = "100%")
              ),
              fluidRow(
                box(width="100%",
                valueBoxOutput("completed"),
                tags$style("#completed {width:16.6%;}"),
                valueBoxOutput("p_completed"),
                tags$style("#p_completed {width:16.6%;}"),
                valueBoxOutput("follow_up" , width="2"),
                tags$style("#follow_up {width:16.6%;}"),
                valueBoxOutput("noMemory" , width="2"),
                tags$style("#noMemory {width:16.6%;}"),
                valueBoxOutput("no_response" , width="2"),
                tags$style("#no_response {width:16.6%;}"),
                valueBoxOutput("wrong" , width="2"),
                tags$style("#wrong {width:16.6%;}")
                ),
                tags$style("box1 {width:100%;}")
              )
              ,
              fluidRow(
                #tabsetPanel(
                  #tabPanel("Pie", plotlyOutput("initiated_progress" , height = 525))
                box( status = "info",width = "100%",
                    column(12, align="center",
                    plotlyOutput("initiated_progress" , height = 550)
              
              )))
      ),
      tabItem("weekly",
              tabBox(
                tabPanel("Sucess rate" ,plotlyOutput("surveyWeekly", height = 500)),
                tabPanel("Distribution of Initiated calls" , plotlyOutput("InitiatedWeekly", height = 500)),
                width = "100%"
              )),
      tabItem("survey",
              fluidRow(box(width = "100%",
                          valueBoxOutput("NPS_SCORE" , width="2"),
                          tags$style("#NPS_SCORE {width:50%;}"),
                          valueBoxOutput("NES_SCORE" , width="2"),
                          tags$style("#NES_SCORE {width:50%;}"))),
                      tabBox(
                        tabPanel("NPS" , box(width = "100%",plotlyOutput("NPS")),
                                 box(width = "100%",plotlyOutput("NPS_group"))),
                        tabPanel("NES" , plotlyOutput("NES"),
                                 box(width = "100%",plotlyOutput("NES_group"))),
                        width = "100%"
                      )        
              
      )
    )
  )

)
