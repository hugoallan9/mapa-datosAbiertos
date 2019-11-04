#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(data.table)
library(dplyr)
library(shiny)
library(rgdal)  
library(leaflet)
library(ckanr)
library(htmltools)
Sys.setlocale("LC_ALL","es_GT.utf8")

select <-dplyr::select



proveedor <- "Stamen.TonerLite"

guate <- readOGR("municipios_gtm/prueba2.shp",  encoding = "UTF-8")

ckanr_setup(url = "https://datos.minfin.gob.gt/")

id_guateCompras <- "105c966a-b71f-4db6-8e8e-caacca249823"

sql <- 'SELECT DISTINCT  "Clase" from "3b7e5806-3a6e-4243-b4ec-c54df1475323" '

categorias <- ds_search_sql(sql, as = 'table')

categorias <- categorias$records



#gdp = fread("munis2017.csv", sep = ',', na.strings = "",header = T, stringsAsFactors = T)
#gdp = gdp[rowSums(is.na(gdp)) != ncol(gdp),] %>% as.data.frame()




#categorias <- levels(gdp$Clase)

#colnames(gdp)[2] <- "Codigo"

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  fluidRow(
    column(7, align= "center",offset = 1,
           br(),
           div(h1("Información Presupuestaria Municipal 2019")),
           br(),
           div(h4(textOutput("title"), align = "center"), style = "color:black"),
           div(h5(textOutput("moneda"), align = "center"), style = "color:black"),
           div(h6(htmlOutput("fuente"), align = "center"), style = "color:black"),
           br())),
  fluidRow(
    column(7, offset = 1,
           leafletOutput("map", height="530"),
           br(),
           div( h6( textOutput("consulta"), align = "left" ) ),
           div( h6( htmlOutput("error"), align = "left" ) ),
           actionButton("reset_button", "Reiniciar mapa")
           ),
    column(3,
           uiOutput("categoryOut", align = "left")))
))

# Define server logic required to draw a histogram
server <- (function(input, output, session) {
  
  output$categoryOut <- renderUI({
    selectInput("category", "Clase de Ingreso:",
                categorias)
  })  
  
  selected <- reactive({
    consulta_sql =  paste('SELECT "Código Entidad" as "Codigo" ,  sum("Percibido") as "Percibido", "Clase"  FROM "3b7e5806-3a6e-4243-b4ec-c54df1475323" 
    WHERE "Clase" =  ', paste0('\'' , input$category, '\'') ,' GROUP BY "Código Entidad" , "Clase" '  )
    print(consulta_sql)
    municipios <- NULL
    try(municipios <- categorias <- ds_search_sql(consulta_sql, as = 'table') )
    try(municipios$records$Codigo <-  as.numeric( substring(as.character(municipios$records$Codigo),5,8) ) )
    try(municipios$records$Percibido <- as.numeric(municipios$records$Percibido) )
    return(municipios$records)
    #print(data)
    #return(data)
    
    
  })
  
  output$title <- renderText({
    req(input$category)
    paste0( paste(toupper(substring(input$category, 1, 1)), tolower(substring(input$category, 2)), sep = "")   , ", en quetzales")
  })
  
  output$fuente <- renderText({
    req(input$category)
    paste('Fuente: <a href="https://datos.minfin.gob.gt/dataset/informacion-presupuestaria-municipal-2017"> Portal de datos abiertos, MINFIN </a> ')
  })
  
  
 
  
  lat <- 16
  lng <- -89.5
  zoom <- 7
  
  output$map <- renderLeaflet({
    
    generico<- leaflet("map") %>% 
      setView(lat = lat, lng = lng, zoom = zoom)
  })
  
  
  observe({
    temp <- selected()
    guate1 <- sp::merge(guate, selected(), by.x='Codigo')
    qpal <- colorQuantile("YlGn", guate1$Percibido, n = 5, na.color = "#bdbdbd")
    popup <- paste0("Mapa")

    tryCatch(
    mapita<-leafletProxy("map", data = guate1) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = guate1, fillColor = ~qpal(Percibido), fillOpacity = 0.7,
                  color = "white", weight = 2, popup = popup, label = mapply(function(x, y) {
          HTML(sprintf("<em>Municipio: %s <br> Ingreso: </em> %s", htmlEscape(x), htmlEscape(y)))},
          guate1$Municipio,paste0("Q",formatC(guate1$Percibido, big.mark = ",", format = "d", digits = 2)),SIMPLIFY = F)
      
      #label = paste(as.character( guate1$Municipio) ,"<br>", 
      #paste0("Q",formatC(guate1$Percibido, big.mark = ",", format = "d", digits = 2)) )
      )%>%
      addLegend(pal = qpal, values = ~Percibido, opacity = 0.7,
                position = 'bottomright', na.label = "No aplica",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(formatC( cuts[-n], big.mark = ",", digits = 2, format = "d" ), " &ndash; ", formatC( cuts[-1], big.mark = ",", digits = 2 , format = "d" ))
                },
                title = paste0(input$category, "<br>")),
    output$consulta <- renderText({
      req(input$category)
      paste('Consulta:', format(Sys.Date(), "%A, %d de %B de %Y"))
      
      
    }),
    
    output$error <- renderText({
      req(input$category)
      paste('')
    })
    
    , error =  function(c) {leafletProxy("map", data = guate1) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons()
      output$error <- renderText({
        req(input$category)
        paste('<strong>Nota:</strong>', " No hay suficiente información para hacer este mapa")
      })
      } )
  })
  
  

  
  


  
})


# Run the application 
shinyApp(ui = ui, server = server)

