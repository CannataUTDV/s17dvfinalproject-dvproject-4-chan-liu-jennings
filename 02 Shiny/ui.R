#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barchart1", tabName = "barchart1", icon = icon("dashboard")),
      menuItem("Barchart2", tabName = "barchart2", icon = icon("dashboard")),
      menuItem("Barchart3", tabName = "barchart3", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      tabItem(tabName = "barchart1",
              tabsetPanel(
                tabPanel("Data1",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Barchart1", "White = Average Birth Weight of a Race per State, Red = Average Birth Weight per State, and  Blue = (Average Birth Weight of a Race per State - Average Birth Weight per State)", plotOutput("plot1", height=1500))
              )
      ),
      tabItem(tabName = "barchart2",
              tabsetPanel(
                tabPanel("Data2",  
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data2")
                ),
                tabPanel("Barchart2", "White = State id (from Natality table), Red = Median Age of that State (from Census2015 table)", plotOutput("plot2", height=700))
              )
      ),
      tabItem(tabName = "barchart3",
              tabsetPanel(
                tabPanel("Data3",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data3")
                ),
                tabPanel("Barchart3", "Number of births by race for the high median_age states", plotOutput("plot3", height=700))
              )
      )
    )
  )
)

