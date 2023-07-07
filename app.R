library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  skin = "green", dashboardHeader(title = "The fuckin dashboard"),
  
  dashboardSidebar
  ( 
    # sidebarUserPanel(
    #   name = "Welcome Onboard!" ),
    sidebarMenu
    (
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "incomplete", badgeColor = "orange"),
      menuItem("Raw data", tabName = "Rawdata", icon = icon("th"),badgeLabel = "new", badgeColor = "green"),
      menuItem("Charts",
               tabName = "charts", 
               icon=icon("stats", lib = "glyphicon"),
               menuSubItem("First chart", tabName = "chart1",icon=icon("line-chart")),
               menuSubItem("Second chart", tabName = "chart2",icon=icon("line-chart")),
               menuSubItem("Third chart", tabName = "chart3",icon=icon("line-chart"))
               
      )
      
      
    )
  ),
  
  dashboardBody
  (       tags$head(tags$style(HTML('            /* body */
                                .content-wrapper, .right-side {
                                background-color: #D1F2EB;
                                }
                                
                                '))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")),
      
      tabItem(tabName = "Rawdata",
              fluidPage( DT::dataTableOutput("data"),style = 'font-size:10px;'
              )),
      tabItem(tabName = "chart1",
              fluidRow(
                box(title = h2("Top 20 countries covid deaths", align="center"),status="success",
                    collapsible = T,solidHeader = T,plotOutput("plot"),collapsed = TRUE),
                box(title = h2("Observations from charts", align="center"),status="warning",
                    collapsible = T,solidHeader = T,collapsed = TRUE,
                    "The COVID-19 pandemic spreads in countries with the highest population and the least social distance, even when they have good medical care.
                        The excess mortality rate was highest in the United States, Brazil, India, and Russia.",style = 'font-size:25px;')
                
                
              ),
              fluidRow(
                box(title = h2("Smallest 20 countries covid deaths", align="center"),status="success",
                    collapsible = T,solidHeader = T,plotOutput("plot2"),collapsed = TRUE),
                box(title = h2("Observations from charts", align="center"),status="warning",
                    collapsible = T,solidHeader = T,collapsed = TRUE,
                    " (San-Martin, Monaco and GreenLand ), Most of these places has just a few numbers of people with high social distance and they are 
                        isolated from the world ",style = 'font-size:25px;')
                
              )),
      tabItem(tabName = "chart2",
              h2("chert2 tab content")),
      tabItem(tabName = "chart3",
              h2("chert3 tab content"))
      
    )
    
    
    
  )
)
#---------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot<-renderPlot({
    
    library(ggplot2)
    data <-read.csv(file="D:\\ASU FCIS 2023\\Year 4\\Semester 1\\2. Information Visualization\\Project\\Information_Visualization_Project\\COVID-19 Coronavirus.csv") 
    #print("sort the data in decreasing order based on subjects ")
    x<- data[order(data$Total_Deaths, decreasing = TRUE), ]   
    x<-head(x,10)
    x<- x[order(x$Total_Deaths, decreasing = FALSE), ]   
    country<-x$Country
    deaths<-x$Total_Deaths
    ggplot(x,aes(x=reorder(country,+deaths),y=deaths)) + 
      geom_bar(fill = "#E95D5D",stat='identity')+
      ggtitle("top 20 countries covid deaths")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text = element_text(size = 20))+
      theme(axis.title = element_text(size = 20))+
      theme(plot.title  = element_text(size = 20))+
      xlab("Countries")+ylab("Deaths")+
      theme(axis.text.x = element_text(angle = 50,hjust = 1))+
      theme(plot.background = element_rect(fill = "#D1F2EB"))+
      theme(panel.background = element_rect(fill = "#D1F2EB",
                                            colour = "#D1F2EB",
                                            size = 0.5, linetype = "solid"))
  })
  output$plot2<-renderPlot({
    
    library(ggplot2)
    data <-read.csv(file="D:\\ASU FCIS 2023\\Year 4\\Semester 1\\2. Information Visualization\\Project\\Information_Visualization_Project\\COVID-19 Coronavirus.csv") 
    #print("sort the data in decreasing order based on subjects ")
    x<- data[order(data$Total_Deaths, decreasing = TRUE), ]   
    x<-head(x, - 20)     
    x<-tail(x,10)
    x<- x[order(x$Total_Deaths, decreasing = FALSE), ]   
    country<-x$Country
    deaths<-x$Total_Deaths
    ggplot(x,aes(x=reorder(country,+deaths),y=deaths)) + 
      geom_bar(fill = "#E95D5D",stat='identity')+
      ggtitle("Smallest 20 countries covid deaths")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text = element_text(size = 20))+
      theme(axis.title = element_text(size = 20))+
      theme(plot.title  = element_text(size = 20))+
      xlab("Countries")+ylab("Deaths")+
      theme(axis.text.x = element_text(angle = 50,hjust = 1))+
      theme(plot.background = element_rect(fill = "#D1F2EB"))+
      theme(panel.background = element_rect(fill = "#D1F2EB",
                                            colour = "#D1F2EB",
                                            size = 0.5, linetype = "solid"))
  })
  output$data<-DT::renderDataTable({
    data <-read.csv(file="D:\\ASU FCIS 2023\\Year 4\\Semester 1\\2. Information Visualization\\Project\\Information_Visualization_Project\\COVID-19 Coronavirus.csv") 
    data
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
