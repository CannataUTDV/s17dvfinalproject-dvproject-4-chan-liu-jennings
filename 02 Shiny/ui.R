#ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplots", tabName = "boxplots", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histograms", icon = icon("dashboard")),
      menuItem("ScatterPlots", tabName = "scatterplots", icon = icon("dashboard")),
      menuItem("Crosstabs", tabName = "crosstabs", icon = icon("dashboard")),
      menuItem("Barcharts", tabName = "barcharts", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      tabItem(tabName = "boxplots",
              tabsetPanel(
                tabPanel("BoxplotData1",  
                         radioButtons("bprb1", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("states3"),
                         actionButton(inputId = "bpclick1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("bpdata1")
                ),
                tabPanel("Boxplot1: Births per State", "Births per state boxplot for selected states", plotlyOutput("bpplot1", height=1500))
              )
      ),
      tabItem(tabName = "histograms",
              tabsetPanel(
                tabPanel("HistogramData1",  
                         radioButtons("hgrb1", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "hgclick1",  label = "To get data, click here"),
                         uiOutput("races2"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("hgdata1")
                ),
                tabPanel("Histogram1: Avg Age of Mother", "Count of average age of mother for selected races, Red line is avg age of mother for each race", plotlyOutput("hgplot1", height=500))
              )
      ),
      tabItem(tabName = "scatterplots",
              tabsetPanel(
                tabPanel("ScatterplotData1",  
                         radioButtons("sprb1", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("states2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "spclick1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("spdata1")
                ),
                tabPanel("Scatterplot1: Median_Age and Births", "Median Age of a State vs Births", plotlyOutput("spplot1", height=700))
              )
      ),
      tabItem(tabName = "crosstabs",
              tabsetPanel(
                tabPanel("CrosstabData1",  
                         radioButtons("ctrb1", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         sliderInput("KPI1", "KPI_Low1:", 
                                     min = 0, max = .78,  value = .78),
                         sliderInput("KPI2", "KPI_Medium1:", 
                                     min = .78, max = .82,  value = .82),
                         actionButton(inputId = "ctclick1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("ctdata1")
                ),
                tabPanel("CrossTab1: Average Birth Weight per State by Race", "This graph shows average birth weight per state by race. It is colored by the calculated field (KPI): AVG(average birth weight)/4000", plotlyOutput("ctplot1", height=1000)),
                tabPanel("CrosstabData2",  
                         radioButtons("ctrb2", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         sliderInput("KPI3", "KPI_Low2:", 
                                     min = 0, max = .78,  value = .78),
                         sliderInput("KPI4", "KPI_Medium2:", 
                                     min = .78, max = .82,  value = .82),
                         actionButton(inputId = "ctclick2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("ctdata2")
                ),
                tabPanel("CrossTab2: 4 Highest Birth Weight States", "This graph shows average birth weight per state by race for the 4 highest birth weight states. It is colored by the calculated field: AVG(average birth weight)/4000", plotlyOutput("ctplot2", height=1000)),
                tabPanel("CrosstabData3",  
                         radioButtons("ctrb3", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         sliderInput("KPI5", "KPI_Low3:", 
                                     min = 0, max = 34.7,  value = 34.7),
                         sliderInput("KPI6", "KPI_Medium3:", 
                                     min = 34.7, max = 39.3,  value = 39.3),
                         actionButton(inputId = "ctclick3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("ctdata3")
                ),
                tabPanel("CrossTab3: Sum of Births with Median Age Parameter", "This graph shows the sum of births for each race per state, colored by the median age of each state parameterized into low, med, and high", plotlyOutput("ctplot3", height=1000))
              )
      ),
      tabItem(tabName = "barcharts",
              tabsetPanel(
                tabPanel("Barchart1Data",  
                         radioButtons("bcrb1", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "bcclick1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("bcdata1")
                ),
                tabPanel("Barchart1: Birth Weights", "White = Average Birth Weight of a Race per State, Red = Average Birth Weight per State, and  Blue = (Average Birth Weight of a Race per State - Average Birth Weight per State)", plotOutput("bcplot1", height=1800)),
                tabPanel("Barchart2Data",  
                         radioButtons("bcrb2", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "bcclick2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("bcdata2")
                ),
                tabPanel("Barchart2: High Median Age States", "Blue = State ID", plotlyOutput("bcplot2", height=700)),
                tabPanel("Barchart3Data",  
                         radioButtons("bcrb3", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "bcclick3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("bcdata3")
                ),
                tabPanel("Barchart3: # of Births in High and Low Median Age States", "Number of births by race for the high and low median_age states", plotOutput("bcplot3", height=700))
                
              )
      )
    )
  )
)

