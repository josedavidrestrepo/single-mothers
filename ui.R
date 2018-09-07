library(shiny)
library(visNetwork)
library(sqldf)

# create an empty database.
# can skip this step if database already exists.
 sqldf("attach mydata as new")
# 
#read.csv.sql("CYCH2017.csv", sql = "create table main.hogar as select * from file",
#             dbname = "mydata")
#read.csv.sql("Salud.csv", sql = "create table main.salud as select * from file",
#             dbname = "mydata")
#read.csv.sql("Educacion.csv", sql = "create table main.educacion as select * from file",
#             dbname = "mydata")
#read.csv.sql("Datosvivienda.csv", sql = "create table main.vivienda as select * from file",
#            dbname = "mydata")
#read.csv.sql("Condicionesvida.csv", sql = "create table main.condivida as select * from file",
#            dbname = "mydata")
#closeAllConnections()

# look at first three lines
dirs <- sqldf("select distinct LLAVEHOG from hogar", dbname = "mydata", user = "")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      verticalLayout(
        selectInput("LLAVEHOG", "LLAVE HOGAR:",
                    dirs)
        
      )
    ),
    mainPanel(
      verticalLayout(
        visNetworkOutput("network")
      )
    )
  ),
  dataTableOutput("tabla")
)