#Starter file for any Shiny dashboard app
#This should replace the default app.r that displays Old Faithful data
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
#The user interface
header <- dashboardHeader(title = "D4 According to Biggs")
sidebar <- dashboardSidebar(
  width = 100,
  actionButton("btninit", "Initialize"),
  actionButton("btni","Apply i"),
  actionButton("btnr1","Apply r1"),
  actionButton("btnr2","Apply r2"),
  actionButton("btnr3","Apply r3"),
  actionButton("btnm1","Apply m1"),
  actionButton("btnm2","Apply m2"),
  actionButton("btnd1","Apply d1"),
  actionButton("btnd2","Apply d2")
)
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height =200)
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("square", height = 300)
    ),
    column(
      width = 6,
      dataTableOutput("multable")
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("d4calc.R")

#Variables that are shared among server functions
D4DF <- D4.makeDataFrame()
config <- "ABCD"

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
    #Initialization
  output$configs <- renderPlot(D4.showConfigs(D4DF))
  output$square <- renderPlot(D4.showSquare(config))
  tbl <-outer(D4DF$name,D4DF$name,vD4.multiply,DF=D4DF)
  colnames(tbl) <- D4DF$name
  rownames(tbl) <- D4DF$name 
  #Use options to suppress the fancy controls
  output$multable <- renderDataTable(tbl, options = list(dom = "t"))
    #Functions that respond to events in the input
  observeEvent(input$btninit,{
    config <<- "ABCD"
    output$square <- renderPlot(D4.showSquare(config))
  })

  observeEvent(input$btni,{
    config <<- D4.apply("i",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr1,{
    config <<- D4.apply("r1",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr2,{
    config <<- D4.apply("r2",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr3,{
    config <<- D4.apply("r3",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnm1,{
    config <<- D4.apply("m1",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnm2,{
    config <<- D4.apply("m2",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnd1,{
    config <<- D4.apply("d1",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnd2,{
    config <<- D4.apply("d2",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)