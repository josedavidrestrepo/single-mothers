library(shiny)
library(visNetwork)
library(sqldf)
library(shinyWidgets)
library(DT)

ui <- fluidPage(
  
  tags$head(
    tags$style(
      "#predict {
          color: green;
          font-size: 60px;
          font-style: italic;
        }"
    )
  ),
  
  
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
                    numericInput("sons", "Cantidad hijos", min = 1, max = 15, value = 1),
                    numericInput("count_persons", "Cantidad de personas en el hogar", min = 2, max = 15, value = 3),
                    selectInput(
                      "house_type", 
                      "Tipo de vivienda", 
                      c("Casa"=1,"Apartamento"=2,"Cuarto(s)"=3,
                        "Vivienda (casa)indigena"=4,"Otro tipo de vivienda (carpa, tienda, vagón, embarcación, cueva, refugio natural, puente, etc)"=5)
                    ),
                    selectInput(
                      "home_income", "Ingreso del hogar", 
                      c("No alcanza para cubrir los gatos mínimos"=1,
                        "Sólo alcanza para cubrir los gastos mínimos"=2,
                        "Cubre más que los gastos mínimos"=3)
                      ),
                    
                    sliderInput("income_satisfaction", "Satisfacción ingreso", min = 0, max = 10, value = 7),
                    
                    tags$label("No comió ninguna de las 3 comidas la semana pasada"),
                    materialSwitch("eating", "SI", status = "primary", right = TRUE)
                  ),
                 tags$h1("Satisfacción predicha:"),
                 textOutput("predict")
               )
      ),
      
      tabPanel("Video", htmlOutput("frame"))
      
    )
)