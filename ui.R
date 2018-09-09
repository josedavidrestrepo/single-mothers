library(shiny)
library(visNetwork)
library(sqldf)
library(shinyWidgets)
library(DT)

ui <- fluidPage(
  titlePanel("Satisfaccion de las madres solteras"),
  
    tabsetPanel(type = "tabs",

      tabPanel(
        
          title = "Estructura Familiar", 
          sidebarLayout(
            
            sidebarPanel(
              width = 3,
              checkboxInput("only_singles", "Sólo hogares con madres solteras", TRUE),
              DTOutput("table_llaves")
            ),
            
            mainPanel(
              visNetworkOutput("network"),
              DTOutput("table")
            )
          )
      ),
      
      tabPanel("Predicciones",
               
               splitLayout(
                 verticalLayout(
                   numericInput("age", "Edad de la madre", min = 10, max = 100, value = 18),
                   selectInput("ss_joined", "Afiliada a seguridad social", c("SI"=1,"NO"=2,"No se"=9)),
                   selectInput("house_type", "Tipo de vivienda", c("Casa"=1,"Apartamento"=2,"Cuarto(s)"=3,"Vivienda (casa)indigena"=4,"Otro tipo de vivienda (carpa, tienda, vagón, embarcación, cueva, refugio natural, puente, etc)"=5)),
                   materialSwitch("read_write", "Sabe leer y escribir", status = "primary", right = TRUE),
                   sliderInput("security_satisfaction", "Satisfacción seguridad", min = 0, max = 10, value = 7)
                  ),
                 verticalLayout(
                   numericInput("count_persons", "Cantidad de personas en el hogar", min = 2, max = 15, value = 3),
                   selectInput("monthly_income", "Ingreso mensual", c("No alcanza para cubrir los gatos mínimos"=1,"Sólo alcanza para cubrir los gastos mínimos"=2,"Cubre más que los gastos mínimos"=3)),
                   selectInput("health", "Estado de salud", c("Muy bueno"=1,"Bueno"=2,"Regular"=3,"Malo"=4)),
                   materialSwitch("eating", "No comió ninguna de las 3 comidas la semana pasada", status = "primary", right = FALSE),
                   sliderInput("income_satisfaction", "Satisfacción ingreso", min = 0, max = 10, value = 7)
                  )
               ),
               textOutput("predict")
        
      ),
      
      tabPanel("video", htmlOutput("frame"))
      
    )
)