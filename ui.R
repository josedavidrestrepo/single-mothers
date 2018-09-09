library(shiny)
library(visNetwork)
library(sqldf)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
        width = 3,
        checkboxInput("only_singles", "Sólo hogares con madres solteras"),
        dataTableOutput("table_llaves")
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(
                      "Estructura Familiar", 
                       visNetworkOutput("network")
                  ),
                  tabPanel("data", dataTableOutput("table")),
                  tabPanel("video", htmlOutput("frame"))
      )
    )
  )
)