library(shiny)
library(visNetwork)
library(sqldf)

ui <- fluidPage(
  
    tabsetPanel(type = "tabs",

      tabPanel(
        
          title = "Estructura Familiar", 
          sidebarLayout(
            
            sidebarPanel(
              width = 3,
              checkboxInput("only_singles", "Sólo hogares con madres solteras"),
              dataTableOutput("table_llaves")
            ),
            
            mainPanel(
              visNetworkOutput("network")
            ),
            
            position = c("right")
          )
      ),
      
      tabPanel("data", dataTableOutput("table")),
      
      tabPanel("video", htmlOutput("frame"))
      
    )
)