####################ESTE CODIGO LEVANTA UNA BASE Y ARMA UN EXPLORADOR QUE INCLUYE MAPA Y TABLAS####




library(readr)
library(leaflet)
library(xlsx)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)
library(htmltools)
library(stringr)
library(ggiraph)
library(ggplot2)
library(leaflet.minicharts)
library(tibble)

url<- 'https://www.dropbox.com/s/7jcxenzyd00h9fl/google_shiny.tsv?raw=1' 
#iberoamerica<-read.table(urla, header = TRUE, sep = "\t")
iberoamerica <- read_delim(url, "\t", escape_double = FALSE, locale = locale(), trim_ws = TRUE)

#Encoding(iberoamerica$Inst)= "UTF-8"


###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

iberoamerica <- iberoamerica %>%
  left_join(countries, by=c('iso'='country'))


rsconnect::setAccountInfo(name='observatorio-cts',
                          token='EFB80756D7A13A771E3F6419EA165929',
                          secret='DfBCakMJl3pZLkwvNS5aGELaMZ95GK5A72rGazAh')



iberoamerica$Date <- parse_date_time(iberoamerica$Date, orders = c("ymd", "dmy", "mdy"))
iberoamerica$Date <-as.Date(format(iberoamerica$Date , "%Y-%m-%d"))




##########Levanta datos#####


cantidades <- iberoamerica %>%
  group_by(country, iso, latitude, longitude)  %>%
  summarize(cantidad=n_distinct(Link)) %>%
  arrange(desc(cantidad))

ordenpais <- cantidades$country

levels(cantidades$country) <- ordenpais



tabla <- iberoamerica %>%
  group_by(country, Inst)  %>%
  summarize(cantidad=n_distinct(Link)) %>%
  arrange(desc(cantidad)) %>%
  unique() %>%
  filter(!is.na(Inst))


tabla <- tabla %>%
  ungroup() %>%
  select(country,Inst)    %>%
  unique()

orden <- tabla$Inst

levels(tabla$Inst) <- orden






server <- function(input, output, session){
  v <- reactiveValues()
  v$s <- NULL
  
  output$map <- renderLeaflet({ 
    leaflet() %>% addTiles()  %>%
      addCircleMarkers(data = cantidades,lng = ~longitude, 
                       lat = ~latitude,  weight = ~sqrt(cantidad)*3,  
                       color='red', layerId = ~unique(country), 
                       label=~paste0(country,': \n',cantidad), opacity = 0.6, popup = ~htmlEscape(country))
  })
  
  #####################PAIS#################
  
  observeEvent(input$map_marker_click$id, {
    updateSelectizeInput(session,
                         inputId = "country", 
                         selected = input$map_marker_click$id,
                         choices = c(tabla %>%
                                       #filter(country == input$map_marker_click$id) %>%
                                       select(country) %>%
                                       unique() %>%
                                       arrange(country)))
  })
  
  
  ##############INSTITUCION##################
  observeEvent(input$country, {
    updateSelectizeInput(session,
                         inputId = "Inst",
                         #selected  = Clicked,
                         choices = c(tabla %>%
                                       filter(country == input$country) %>%
                                       select(Inst) %>%
                                       unique()))  })
  
  
  
  data <- reactive({
    iberoamerica %>% filter (country %in% input$country) %>% 
      select(country, Inst, Link)  %>%
      group_by(Inst)  %>%
      summarize(cantidad=n_distinct(Link))   %>%
      filter(!is.na(Inst))  %>%
      filter(!Inst =='')  %>%
      select(Inst, cantidad)   %>%
      arrange(desc(cantidad))  %>% column_to_rownames(var = "Inst")
  })
  
  
  
  
  data2 <- reactive({
    iberoamerica %>%
      #rownames_to_column(var = "Inst") %>% 
      #column_to_rownames(var = "Inst") %>% 
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      select(Date, Title,  Link)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver Articulo</a>")) %>%
      arrange(desc(Date))  
    
    
  })
  
  
  
  output$seleccionada <- renderText({
    paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", ")
  }) 
  
  
  observe({
    if(!is.null(input$inst_pais_rows_selected)){
      v$s <- input$inst_pais_rows_selected
    }
  })
  
  
  
  
  
  output$inst_pais  <- DT::renderDataTable({datatable(data(),selection = "single")})
  output$inst_paper <- renderDT(data2(), server = FALSE, escape = FALSE, rownames= FALSE)
  
  
  
}



###################




ui <- function(id) {
  fluidPage(
    #fluidRow(column(12,textOutput("seleccionada"))),
    fluidRow(column(12, leafletOutput("map", height = "600", width = "800"))),
    fluidRow(column(6, selectInput("country", label = "Seleccione Pais", choices = unique(ordenpais)))),
    fluidRow(column(6, tags$h4("Principales Instituciones")), column(6, tags$h4("Publicaciones y Noticias"))),
    fluidRow(column(6,DT::dataTableOutput("inst_pais")),column(6,DT::dataTableOutput("inst_paper"))))
}






shinyApp(ui, server)







