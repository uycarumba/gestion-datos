# install.packages("shinydashboard")


library(shinydashboard)
library(ggplot2)
library(excelR)


#====================================================
#               MAPA 
#====================================================
library(geos)
library(PROJ)

library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)


#La dirección de tu directorio de trabajo
dirmapas <- "C:/LUCY_ARCHIVOS/2020_MAESTRIA/06_Gestion de Datos/Trabajo Grupal/Experimento_L/mapa_GEOGPSPERU/departamentos" 
setwd(dirmapas)

#Este comando permite leer el shapefile y 'transformarlo' en un data frame
peru_d <- st_read("DEPARTAMENTOS.shp") 

# Una muestra de como luce nuestra base de datos
peru_d 


#Se crea el centroide
peru_d <- peru_d %>% mutate(centroid = map(geometry,  st_centroid), 
                            coords = map(centroid, st_coordinates),
                            coords_x = map_dbl(coords, 1), 
                            coords_y = map_dbl(coords, 2))

#====================================================
#               EXPEDIENTES POR DEPARTAMENTO  
#====================================================

library(mongolite)
url_path="mongodb+srv://usuario:1234@cluster0.zjeev.mongodb.net/dbMINJUS"
mongo <- mongo(collection = "expedientesDepartamento", db = "dbMINJUS", 
               url = url_path, 
               verbose = TRUE)

#mongo
#mongo$find(query = '{}', fields = '{DEPARTAMENT}')

# Identificar si los nombres de departamentos son iguales
filtro <- '{}'
claves <- '{"DEPARTAMENT": 1}'
resultado <- mongo$find(filtro, claves)

#head(resultado, 5)
unique(resultado$DEPARTAMENT)
unique(peru_d$DEPARTAMEN)

print("json -> df")
#cambiar "PROVINCIA CONSTITUCIONAL DEL CALLAO" x "CALLAO"
mongo$update('{"DEPARTAMENT":"PROVINCIA CONSTITUCIONAL DEL CALLAO"}', 
             '{"$set":{"DEPARTAMENT": "CALLAO"}}',
             multi=TRUE)


dfExpDptoYear_mapa<-data.frame(mongo$find())
dfExpDptoYear_mapa

dfExpDptoYear_mapa <- dfExpDptoYear_mapa %>% drop_na()
# _id   DEPARTAMENT ANHO CANTIDAD
dfExpDptoYear_mapa1 = rename(dfExpDptoYear_mapa, c(DEPARTAMEN="DEPARTAMENT"))
dfExpDptoYear_mapa1<- dfExpDptoYear_mapa1[dfExpDptoYear_mapa1$ANHO<=2021,]
ListaAnios<- unique(dfExpDptoYear_mapa1$ANHO)



  
dfExpDptoYear_mapa1 <- na.omit(dfExpDptoYear_mapa1)
#====================================================
#               FUSIONAR SHINY
#====================================================



header <- dashboardHeader(title = " Final - Grupo 4")

sidebar <-  dashboardSidebar(
        sidebarMenu(
          menuItem("Mapa Expedientes Exitosos", tabName = "dashboard4", icon = icon("dashboard"))
        )
    )

body <- dashboardBody(
                      tabItems(
                           tabItem(tabName = "dashboard4",
                                   h2("Expedientes Existosos por Departamento"),
                                   fluidRow(box(title = "INPUT", "Ingresar Datos",width = 3,
                                                selectInput("IdAnio","Año", sort(ListaAnios))
                                                ),
                                            box(plotOutput("plotmapa", height = 800)
                                                )
                                            )
                                   )
                            )
                  )


ui <- dashboardPage(
                    
                    header,
                    sidebar,
                    body
                )


 


server <- function(input, output) {

    
    output$plotmapa <- renderPlot({
      
                                  for (i in 1:length(ListaAnios))
                                    print (ListaAnios[i])
                                    dfX<-dfExpDptoYear_mapa1[dfExpDptoYear_mapa1$ANHO==ListaAnios[i],]
                                    dfExpDptoYear_mapa1$ratio <-round(100*dfExpDptoYear_mapa1$CANTIDAD/sum(dfX$CANTIDAD),2)
                                    print(dfExpDptoYear_mapa1)
                                    dfExpDptoYear_mapa2<-dfExpDptoYear_mapa1[c(1:2,4)]
                                  print(dfExpDptoYear_mapa2)
                                  
                                  ANIO<-as.integer(input$IdAnio)
                                 # pobreza_mapa2 <-pobreza_mapa[pobreza_mapa$ANIO==ANIO,]
                                  dfExpDptoYear_mapa3 <-dfExpDptoYear_mapa2[dfExpDptoYear_mapa1$ANHO==ANIO,]
                                  print(dfExpDptoYear_mapa3)
                                  
                                  #Juntamos ambas bases de datos
                                  peru_datos <- peru_d %>% left_join(dfExpDptoYear_mapa3)
                                  print(peru_datos)
                                  peru_datos %>%
                                    ggplot() +
                                    
                                     #geom_sf(aes(fill = ratio))+
                                      
                                    geom_sf(fill = "grey60",color = "white", aes(fill = ratio))+
                                      
                                    labs(title = "Porcentaje de expedientes exitosos por departamento ",
                                           caption = "Fuente: Elaboración propia(2021)",
                                           x="Longitud",
                                           y="Latitud")+
                                    geom_point(aes(coords_x,coords_y, size=ratio),color= "orange",  alpha = 0.7 ) +
                                      
                                    geom_text_repel(mapping = aes(coords_x, coords_y, label = DEPARTAMEN), size = 2.25,  color ="black") +
                                    scale_fill_continuous(guide_legend(title = "Ratio de Expedientes exitosos"))
                                  
                                  
                                 })
    
    }
shinyApp(ui, server)


