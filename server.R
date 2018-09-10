require(gamlss)
require(MASS)
require(betareg)
require(RColorBrewer)
library(DT)
library(shiny)
library(visNetwork)

# sqldf("attach db as new")
#
# read.csv.sql("hogar.csv", sql = "create table hogar as select * from file",
#               dbname = "db")
# read.csv.sql("madres.csv", sql = "create table main.madres as select * from file",
#             dbname = "db")
# closeAllConnections()

#Lectura de los datos
# data <- read.csv("madres.csv", header=T)
# # sapply(data, class) ##veridficando la clase de las variables
# datos<-data[,-c(1,2,12,13)] #eliminando orden, llave hogar, niveleducativo y salario por na
# datos<-na.omit(datos)
# #
# # ###------------------- MODELOS -----------------------------###
# set.seed(1) #semilla para que a todos nos salgan los mismos resultados
# trainingRows <- sample(1:nrow(datos), 0.7 * nrow(datos))
# trainingData <- datos[trainingRows, ] ##datos para entrenar el modelo
# testData <- datos[-trainingRows, ] ##datos para test del modelo
# #
# # #cambiando la clase de la variable respuesta
# datos$SATISFACCION_VIDA<-as.numeric(datos$SATISFACCION_VIDA)
# 
# #modelo lm
# modelo<-lm(SATISFACCION_VIDA ~ ANOS_CUMPLIDOS + CANT_PERSONAS +
#               INGRESO_HOGAR + COMIDA + HIJOS + TIPO_VIVIENDA + 
#               SATISFACCCION_INGRESO,
#             data=datos)
# 
# 
# saveRDS(modelo, "modelo.rds")

modelo <- readRDS("modelo.rds")


function(input, output) { 
  
  #Datatable con la lista de hogares por llave
  output$table_llaves <-  renderDataTable({
    
    only_singles_value = input$only_singles    
    
    query_llaves <- "SELECT DISTINCT REPLACE(LLAVEHOG,'\"','') AS [HOGAR] FROM hogar"
    
    if (only_singles_value) {
      query_llaves <- "SELECT DISTINCT REPLACE(h.LLAVEHOG,'\"','') AS [HOGAR]
                        FROM hogar h
                        INNER JOIN (
                        	SELECT h2.LLAVEHOG, h2.ORDEN, COUNT(DISTINCT h.ORDEN) HIJOS
                        	FROM hogar h
                        	INNER JOIN hogar h2 ON h2.ORDEN = h.p6083s1 AND h.LLAVEHOG = h2.LLAVEHOG
                        	WHERE h.p6083 = 1						-- si la madre vive en el hogar
                        			AND h2.p5502 = 5				-- estado civil soltera
                        	GROUP BY h2.LLAVEHOG, h2.ORDEN
                        ) m ON m.LLAVEHOG = h.LLAVEHOG AND m.ORDEN = h.ORDEN"
    }
      
    llaves_data_set <- sqldf(query_llaves, dbname = "db")
    
    datatable(
      llaves_data_set, rownames = FALSE,
      options = list(lengthChange = FALSE, dom = 'tlipr'),
      selection = list(mode = 'single', target = 'row', selected = c(1)),
      filter = 'bottom'
    ) %>% formatStyle(0, cursor = 'pointer')
    
  })

  output$network <- renderVisNetwork({
    
    llave_hog_sel = input$table_llaves_cell_clicked$value
    
    if (is.null(llave_hog_sel)) {
      if (input$only_singles)
      {
        llave_hog_sel <- '60000131'
      }
      else
      {
        llave_hog_sel <- '60000001'
      }
    }
    
    query <- paste("SELECT * FROM hogar WHERE LLAVEHOG = '\"", llave_hog_sel, sep="")
    query <- paste(query, "\"'" ,sep="")
    familia <- sqldf(query, dbname = "db", user = "")

    fromNodes <- c()
    toNodes <- c()
    Groups = c()
    label = c()
    dashes = c()
    arrows = c()

    for (row in 1:nrow(familia)) {
      queryhijos <- paste("SELECT COUNT(DISTINCT ORDEN) HIJOS FROM hogar WHERE LLAVEHOG = '\"", llave_hog_sel, sep="")
      queryhijos <- paste(queryhijos,"\"\'"," AND P6083 = 1 AND P6083S1 = ", row, sep="")
      numHijos <- sqldf(queryhijos, dbname = "db", user = "")

      label = c(label, row)
      sexo <- familia[row, 'P6020']
      padre <- familia[row, 'P6081']
      madre <- familia[row, 'P6083']
      conyuge <- familia[row, 'P6071']
      estadocivil <- familia[row, 'P5502']

      #Esposos
      if (familia[row,"P6071"] == 1) {
        fromNodes <- c(fromNodes,row)
        toNodes <- c(toNodes,familia[row,"P6071S1"])
        dashes = c(dashes, TRUE)
        arrows = c(arrows, "none")
      }
      
      #Padre
      if(padre == 1){
        fromNodes <- c(fromNodes,familia[row, 'P6081S1'])
        toNodes <- c(toNodes, row)
        dashes = c(dashes, FALSE)
        arrows = c(arrows, "to")
      }

      #Madre
      if(madre == 1){
        fromNodes <- c(fromNodes, familia[row, 'P6083S1'])
        toNodes <- c(toNodes, row)
        dashes = c(dashes, FALSE)
        arrows = c(arrows, "to")
      }

      #Madre soltera
      if(sexo == 2){
        if (numHijos > 0 && estadocivil == 5) {
          Groups = c(Groups, 'SingleMother')
        } else{
          Groups = c(Groups, 'Woman')
        }
      } else{
        Groups = c(Groups, 'Man')
      }
    }

    nodes <- data.frame(
      id = 1:nrow(familia),
      group = Groups,
      label = label
    )
    
    edges <- data.frame(
      from = fromNodes, 
      to = toNodes, 
      dashes = dashes, 
      arrows = arrows
    )

    visNetwork(nodes, edges, width = "100%") %>%
      visEvents(
        select = "function(nodes) {
                    Shiny.onInputChange('current_node_id', nodes.nodes);
                  ;}"
      ) %>%
      visGroups(
        groupname = "SingleMother",
        shape = "icon",
        icon = list(code = "f182", color = 'lightgreen')
      ) %>%
      visGroups(
        groupname = "Man", 
        shape = "icon",
        icon = list(code = "f183", color = "skyblue")
      ) %>%
      visGroups(
        groupname = "Woman", 
        shape = "icon",
        icon = list(code = "f182", color = "pink")
      ) %>%
      addFontAwesome() %>%
      visLegend(
        addNodes = list(
          list(label = "Hombre", shape = "icon",
               icon = list(code = "f183", color = 'skyblue')),
          list(label = "Mujer", shape = "icon",
               icon = list(code = "f182", color = "pink")),
          list(label = "Madre Soltera", shape = "icon",
               icon = list(code = "f182", color = 'lightgreen'))
        ),
        addEdges = data.frame(
          label = c('Padre/Madre de', 'Esposo(a) de'),
          dashes = c(FALSE, TRUE), 
          color = 'black', 
          arrows = c("to","none")
        ),
        width = 0.3, useGroups = FALSE,
        position = 'right', zoom = FALSE
      ) %>%
      visInteraction(
        dragNodes = TRUE,
        dragView = FALSE,
        zoomView = FALSE
      ) %>%
      visEdges(
        color = list(color = "black")
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50)
      ) %>%
      visNodes(
        size = 35, 
        font = '28px arial black'
      )
  })
  
  myNode <- reactiveValues(selected = '')
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })

  output$table <- renderDataTable({
    desc_values <- c(
      "Ingreso Hogar"=c("1"="No alcanza para cubrir los gatos mínimos",
                        "2"="Sólo alcanza para cubrir los gastos mínimos",
                        "3"="Cubre más que los gastos mínimos"
                        ),
      "Comida"=c("1"="SI","2"="NO"),
      "Tipo de Vivienda"=c("1"="Casa","2"="Apartamento","3"="Cuarto(s)",
                           "4"="Vivienda (casa) indígena","5"="Otro tipo de vivienda (carpa, tienda, vagón, embarcación, cueva, refugio natural, puente, etc)")
    )
    
    llave_hog_sel = input$table_llaves_cell_clicked$value
    if (is.null(llave_hog_sel)) {
      if (input$only_singles)
      {
        llave_hog_sel <- '60000131'
      }
      else
      {
        llave_hog_sel <- '60000001'
      }
    }
    
    query <- paste("SELECT ORDEN [ID], ANOS_CUMPLIDOS [Edad], CANT_PERSONAS [Cantidad de Personas], 
                    INGRESO_HOGAR [Ingreso Hogar], HIJOS [Cantidad de Hijos],
                    TIPO_VIVIENDA [Tipo de Vivienda], SATISFACCCION_INGRESO [Satisfaccion Ingreso],
                    SATISFACCION_VIDA [Satisfaccion Vida]
                   FROM madres WHERE LLAVEHOG = '", llave_hog_sel, sep="")
    query <- paste(query,"\'",sep="")
    familia <- sqldf(query, dbname = "db", user = "")
    
    for (row in 1:nrow(familia)) {
      ingreso <- toString(familia[row, 'Ingreso Hogar'])
      tipo_vivienda <- toString(familia[row, 'Tipo de Vivienda'])
      
      familia[row, 'Ingreso Hogar'] = desc_values[paste("Ingreso Hogar.", ingreso, sep="")]
      familia[row, 'Tipo de Vivienda'] = desc_values[paste("Tipo de Vivienda.", tipo_vivienda, sep="")]
    }
    
    datatable(
      familia, rownames = FALSE,
      options = list(lengthChange = FALSE, dom = 't'),
      selection = list(mode = 'single')
    ) %>% formatStyle(0, cursor = 'pointer')

  })
  
  output$predict = renderText({
    
    
    
    age <- input$age
    sons <- input$sons
    count_persons <- input$count_persons
    house_type <- as.numeric(input$house_type)
    home_income <- as.numeric(input$home_income)
    income_satisfaction <- input$income_satisfaction
    
    eating <- 2
    if (input$eating){
      eating <- 1
    }
    
    if (is.na(age)) age <- 0
    if (is.na(sons)) sons <- 0
    
    newdatos<-data.frame(
      wt= age, count_persons, home_income, eating, sons,
        house_type, income_satisfaction
    )
    names(newdatos) <-  c("ANOS_CUMPLIDOS", "CANT_PERSONAS",
                          "INGRESO_HOGAR",
                          "COMIDA","HIJOS","TIPO_VIVIENDA",
                          "SATISFACCCION_INGRESO")
    
    s<-predict(modelo,newdata=newdatos)
    
    resultado <- round(s,0)
    
    if (resultado > 10) resultado <- 10
    if (resultado < 0) resultado <- 0
    
    resultado
  })
  
  output$thermometer <- renderPlot({
    symbols(0, thermometers = cbind(0.3, 1, 4.1/9), fg = 2, xlab = NA, ylab = NA, inches = 2.5, yaxt='n', xaxt='n', bty='n')
  })
  
  output$frame <- renderUI({
    tags$iframe(src="https://biteable.com/watch/embed/tae-2018-02-1990018", height=500, width=800)
  })
}