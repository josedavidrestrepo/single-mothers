library(shiny)
library(visNetwork)
library(sqldf)
library(shinyWidgets)

ui <- fluidPage(
  
    tabsetPanel(type = "tabs",

      tabPanel(
        
          title = "Estructura Familiar", 
          sidebarLayout(
            
            sidebarPanel(
              width = 3,
              checkboxInput("only_singles", "Sólo hogares con madres solteras", TRUE),
              dataTableOutput("table_llaves")
            ),
            
            mainPanel(
              visNetworkOutput("network"),
              dataTableOutput("table")
            )
          )
      ),
      
      tabPanel("Predicciones",
               
        flowLayout(
          numericInput("age", "Edad de la madre", min = 10, max = 100, value = 18),
          numericInput("count_persons", "Cantidad de personas en el hogar", min = 2, max = 15, value = 3),
          selectInput("ss_joined", "Afiliada a seguridad social", c(1,2,9)),
          materialSwitch("read_write", "Sabe leer y escribir", status = "primary", right = TRUE),
          numericInput("monthly_income", "Ingreso mensual", min = 100000, max = 20000000, value = 1500000),
          materialSwitch("eating", "No comió ninguna de las 3 comidas la semana pasada", status = "primary", right = FALSE),
          selectInput("house_type", "Tipo de vivienda", c(1,2,3,4,5)),
          selectInput("health", "Estado de salud", c(1,2,3,4)),
          sliderInput("security_satisfaction", "Satisfacción seguridad", min = 0, max = 10, value = 7),
          sliderInput("income_satisfaction", "Satisfacción ingreso", min = 0, max = 10, value = 7)
        )
        
      ),
      
      tabPanel("video", htmlOutput("frame"))
      
    )
)