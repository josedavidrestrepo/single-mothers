library(shiny)
library(visNetwork)
library(DT)

sqldf("attach db as new")

#read.csv.sql("hogar.csv", sql = "create table main.hogar as select * from file",
#             dbname = "db")

#closeAllConnections()

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
      filter = 'top'
    ) %>% formatStyle(0, cursor = 'pointer')
    
  })

  output$network <- renderVisNetwork({
    
    llave_hog_sel = input$table_llaves_cell_clicked$value
    
    if (is.null(llave_hog_sel)) llave_hog_sel <- '60000001'
    
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
      queryhijos <- paste("SELECT COUNT(DISTINCT ORDEN) HIJOS FROM hogar WHERE LLAVEHOG = '\"",llave_hog_sel,sep="")
      queryhijos <- paste(queryhijos,"\"\'"," AND P6083 = 1 AND P6083S1 = ",row,sep="")
      numHijos <- sqldf(queryhijos, dbname = "db", user = "")

      label = c(label, row)
      sexo <- familia[row, 'P6020']
      relacion <- familia[row, 'P6051']
      padre <- familia[row, 'P6081']
      madre <- familia[row, 'P6083']
      conyuge <- familia[row, 'P6071']
      estadocivil <- familia[row, 'P5502']

      if (familia[row,"P6071"] == 1) {
        fromNodes <- c(fromNodes,row)
        toNodes <- c(toNodes,familia[row,"P6071S1"])
        dashes = c(dashes, TRUE)
        arrows = c(arrows, "none")
      }

      if(padre == 1){
        fromNodes <- c(fromNodes,familia[row, 'P6081S1'])
        toNodes <- c(toNodes, row)
        dashes = c(dashes, FALSE)
        arrows = c(arrows, "to")
      }

      if(madre == 1){
        fromNodes <- c(fromNodes, familia[row, 'P6083S1'])
        toNodes <- c(toNodes, row)
        dashes = c(dashes, FALSE)
        arrows = c(arrows, "to")
      }

      if(sexo == 2){
        if (numHijos > 0 && (estadocivil == 3 || estadocivil == 4 || estadocivil == 5)
              && (conyuge == 2 || conyuge == Inf)) {
          Groups = c(Groups, 'SingleMother')
        } else{
          Groups = c(Groups, 'Woman')
        }
      } else{
        Groups = c(Groups, 'Man')
      }
    }

    nodes <- data.frame(id = 1:nrow(familia),
                        group = Groups,
                        label = label)
    edges <- data.frame(from = fromNodes, to = toNodes, dashes = dashes, arrows = arrows)

    visNetwork(nodes, edges,width = "100%") %>%
      visGroups(groupname = "SingleMother", shape = "icon",
                icon = list(code = "f182", color = 'lightgreen')) %>%
      visGroups(groupname = "Man", shape = "icon",
              icon = list(code = "f183", color = "skyblue")) %>%
      visGroups(groupname = "Woman", shape = "icon",
                icon = list(code = "f182", color = "pink")) %>%
      addFontAwesome() %>%
      visLegend(addNodes = list(
                list(label = "Hombre", shape = "icon",
                     icon = list(code = "f183", color = 'skyblue')),
                list(label = "Mujer", shape = "icon",
                     icon = list(code = "f182", color = "pink")),
                list(label = "Madre Soltera", shape = "icon",
                     icon = list(code = "f182", color = 'lightgreen'))),
                addEdges = data.frame(label = c('Padre/Madre de', 'Esposo(a) de'),
                                      dashes = c(FALSE, TRUE), color = 'black', arrows = c("to","none")),
                width = 0.3, useGroups = FALSE, position = 'right') %>%
      visInteraction(dragNodes = FALSE,
               dragView = FALSE,
               zoomView = FALSE) %>%
      visEdges(color = list(color = "black")) %>%
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -50)) %>%
      visNodes(size = 35, font = '28px arial black')
  })

  output$table <- renderDataTable({
    dir <- input$LLAVEHOG
    query <- paste("SELECT m.ORDEN as id, P6081 as vivePadre, P6083 as viveMadre, P6087 as eduPadre,
                          P6088 as eduMadre, P6090 as afiliadoSalud, P6127 as estadoSalud,
                          P6160 as sabeLeerEscribir, P9030 as condiVida
                   FROM hogar m
                   INNER join salud s ON m.LLAVEHOG = s.LLAVEHOG AND m.ORDEN = s.ORDEN
                   INNER join educacion e ON m.LLAVEHOG = e.LLAVEHOG AND m.ORDEN = e.ORDEN
                   INNER join condivida c ON m.LLAVEHOG = c.LLAVEHOG
                   WHERE m.LLAVEHOG = '",dir,sep="")
    query <- paste(query,"\'",sep="")
    familia <- sqldf(query, dbname = "db", user = "")
    familia
  })
  
  output$frame <- renderUI({
    tags$iframe(src="https://biteable.com/watch/embed/tae-2018-02-1990018", height=500, width=800)
  })
}