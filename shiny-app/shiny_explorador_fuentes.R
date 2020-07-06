####################ESTE CODIGO LEVANTA UNA BASE Y ARMA UN EXPLORADOR QUE INCLUYE MAPA Y TABLAS####



library(readr)
library(leaflet)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)
library(htmltools)
library(stringr)
library(ggplot2)
library(tibble)
library(lubridate)
library(highcharter)
library(ggiraph)
library(shinydashboard)
library(semantic.dashboard)



##########################Levanta Datos#############################
url<- 'https://www.dropbox.com/s/7jcxenzyd00h9fl/google_shiny.tsv?raw=1' 
iberoamerica <- read_delim(url, "\t", escape_double = FALSE, locale = locale(), trim_ws = TRUE)
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)


iberoamerica <- iberoamerica %>%
  left_join(countries, by=c('iso'='country'))

iberoamerica$Date <- parse_date_time(iberoamerica$Date, orders = c("ymd", "dmy", "mdy"))
iberoamerica$Date <-as.Date(format(iberoamerica$Date , "%Y-%m-%d"))

iberoamerica$country <- str_replace(iberoamerica$country, "Brazil", "Brasil")
iberoamerica$country <- str_replace(iberoamerica$country, "Dominican Republic", "Rep. Dominicana")


######################CANTIDADES INSTITUCIONES#########################

listado_inst <- iberoamerica %>%
  group_by(country, Inst)  %>%
  summarize(cantidad=n_distinct(Link)) %>%
  arrange(desc(cantidad)) %>%
  unique() %>%
  filter(!is.na(Inst))

listado_inst1 <- iberoamerica %>%
  filter(Fuente == 'PubMed')  %>%
  group_by(country, Inst)  %>%
  summarize(cantidadPM=n_distinct(Link)) %>%
  arrange(desc(cantidadPM)) %>%
  unique() %>%
  filter(!is.na(Inst))

listado_inst2 <- iberoamerica %>%
  filter(Fuente == 'Noticias')  %>%
  group_by(country, Inst)  %>%
  summarize(cantidadN=n_distinct(Link)) %>%
  arrange(desc(cantidadN)) %>%
  unique() %>%
  filter(!is.na(Inst))

listado_inst3 <- iberoamerica %>%
  filter(Fuente == 'LaReferencia')  %>%
  group_by(country, Inst)  %>%
  summarize(cantidadLR=n_distinct(Link)) %>%
  arrange(desc(cantidadLR)) %>%
  unique() %>%
  filter(!is.na(Inst))

listado_inst <- listado_inst %>% left_join(listado_inst1)  %>% left_join(listado_inst2)  %>% left_join(listado_inst3)  


######################LISTADO INSTITUCIONES#########################

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



############################CANTIDADES###########################

cantidades <- iberoamerica %>%
  group_by(country, iso)  %>%
  #group_by(country, iso, latitude, longitude)  %>%
  summarize(cantidad=n_distinct(Link)) %>%
  arrange(desc(cantidad))

cantidades <- cantidades %>% left_join(countries, by=c('iso'='country'))



ordenpais <- cantidades$country

levels(cantidades$country) <- ordenpais


cantidades1 <- iberoamerica %>%
  filter(Fuente == 'PubMed')  %>%
  group_by(country, iso)  %>%
  #group_by(country, iso, latitude, longitude)  %>%
  summarize(cantidadPM=n_distinct(Link)) %>%
  arrange(desc(cantidadPM))



ordenpais1 <- cantidades1$country

levels(cantidades1$country) <- ordenpais1

cantidades2 <- iberoamerica %>%
  filter(Fuente == 'Noticias')  %>%
  group_by(country, iso)  %>%
  #group_by(country, iso, latitude, longitude)  %>%
  summarize(cantidadN=n_distinct(Link)) %>%
  arrange(desc(cantidadN))

ordenpais2 <- cantidades2$country

levels(cantidades2$country) <- ordenpais2



cantidades3 <- iberoamerica %>%
  filter(Fuente == 'LaReferencia')  %>%
  group_by(country, iso)  %>%
  #group_by(country, iso, latitude, longitude)  %>%
  summarize(cantidadLR=n_distinct(Link)) %>%
  arrange(desc(cantidadLR))

ordenpais3 <- cantidades3$country

levels(cantidades3$country) <- ordenpais3


cantidades <- cantidades %>% left_join(cantidades1)  %>% left_join(cantidades2)  %>% left_join(cantidades3)  





server <- function(input, output, session){
  v <- reactiveValues()
  v$s <- NULL
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()  %>%
      addCircleMarkers(data = cantidades,lng = ~longitude, 
                       lat = ~latitude,  weight = ~sqrt(cantidad)*3,  
                       color='red', layerId = ~unique(country), 
                       label=~paste0(country,': ',cantidad), opacity = 0.6, popup = ~htmlEscape(country))
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
  
  
  
#  data <- reactive({
  #    iberoamerica %>% 
  #   # filter (country %in% input$country) %>% 
  #    select(country, Inst, Link, Fuente)  %>%
  #    group_by(Inst, Fuente)  %>%
  #    summarize(cantidad=n_distinct(Link))   %>%
  #    filter(!is.na(Inst))  %>%
  #    filter(!Inst =='')  %>%
  #    tidyr::spread(Fuente, cantidad)   %>%
  #    select(Inst, PubMed, Noticias, LaReferencia)  %>%
  #    #select_if(names(.) %in% c('Inst', 'PubMed', 'Noticias', 'LaReferencia'))   %>%
  #    arrange(desc(PubMed))  %>% column_to_rownames(var = "Inst")
  #})
  
  
  
  data <- reactive({
    listado_inst %>% 
      filter (country %in% input$country) %>% 
      rename(Pais=country)  %>% 
      select(Inst, PubMed=cantidadPM, Noticias=cantidadN, LaReferencia=cantidadLR)  %>%
      filter(!is.na(Inst))  %>%
      filter(!Inst =='')  %>%
      arrange(desc(PubMed))  %>% column_to_rownames(var = "Inst")
  })
  
  
  
  
  
  data2 <- reactive({
    iberoamerica %>%
      #rownames_to_column(var = "Inst") %>% 
      #column_to_rownames(var = "Inst") %>% 
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      filter (Fuente == 'PubMed')   %>%
      select(Date, Title, Link)  %>%
      #filter(Fuente %in% input$fuente)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver</a>")) %>%
      arrange(desc(Date))  
    
    
  })
  
  
  
  data3 <- reactive({
    iberoamerica %>%
      #rownames_to_column(var = "Inst") %>% 
      #column_to_rownames(var = "Inst") %>% 
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      filter (Fuente == 'Noticias')   %>%
      select(Date, Title, Link)  %>%
      #filter(Fuente %in% input$fuente)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver</a>")) %>%
      arrange(desc(Date))  
    
    
  })
  
  
  
  
  data4 <- reactive({
    iberoamerica %>%
      #rownames_to_column(var = "Inst") %>% 
      #column_to_rownames(var = "Inst") %>% 
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      filter (Fuente == 'LaReferencia')   %>%
      select(Date, Title, Link)  %>%
      #filter(Fuente %in% input$fuente)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver</a>")) %>%
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
  output$inst_paper2 <- renderDT(data2(), server = FALSE, escape = FALSE, rownames= FALSE)
  output$inst_paper3 <- renderDT(data3(), server = FALSE, escape = FALSE, rownames= FALSE)
  output$inst_paper4 <- renderDT(data4(), server = FALSE, escape = FALSE, rownames= FALSE)
  
  
  

  output$paises <- renderGirafe({ggplot1a <- cantidades %>%
    ggplot(aes(reorder(country, +cantidad), cantidad, tooltip = paste0(country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot1a, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  output$paises2 <- renderGirafe({ggplot1b <- cantidades %>%
    select(country, iso, latitude, longitude, cantidad=cantidadPM)  %>%
    filter(!is.na(cantidad))  %>%
    ggplot(aes(reorder(country, +cantidad), cantidad, tooltip = paste0(country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot1b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  output$paises3 <- renderGirafe({ggplot1c <- cantidades %>%
    select(country, iso, latitude, longitude, cantidad=cantidadN)  %>%
    filter(!is.na(cantidad))  %>%
    ggplot(aes(reorder(country, +cantidad), cantidad, tooltip = paste0(country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot1c, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  output$paises4 <- renderGirafe({ggplot1d <- cantidades %>%
    select(country, iso, latitude, longitude, cantidad=cantidadLR)  %>%
    filter(!is.na(cantidad))  %>%
    ggplot(aes(reorder(country, +cantidad), cantidad, tooltip = paste0(country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot1d, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  
  

  output$instituciones <- renderGirafe({ggplot2a <- listado_inst %>%
    arrange(desc(cantidad)) %>%
    head(20) %>%
    ggplot(aes(reorder(Inst, +cantidad), cantidad, fill=country, tooltip = paste0(Inst,", ", country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))

  
  girafe(ggobj = ggplot2a, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  
  output$instituciones2 <- renderGirafe({ggplot2b <- listado_inst %>%
    select(country, Inst, cantidad=cantidadPM)  %>%
    filter(!is.na(cantidad))  %>%
    arrange(desc(cantidad)) %>%
    head(20) %>%
    ggplot(aes(reorder(Inst, +cantidad), cantidad, fill=country, tooltip = paste0(Inst,", ", country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot2b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  
  output$instituciones3 <- renderGirafe({ggplot2c <- listado_inst %>%
    select(country, Inst, cantidad=cantidadN)  %>%
    filter(!is.na(cantidad))  %>%
    arrange(desc(cantidad)) %>%
    head(20) %>%
    ggplot(aes(reorder(Inst, +cantidad), cantidad, fill=country, tooltip = paste0(Inst,", ", country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot2c, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  
  output$instituciones4 <- renderGirafe({ggplot2d <- listado_inst %>%
    select(country, Inst, cantidad=cantidadLR)  %>%
    arrange(desc(cantidad)) %>%
    filter(!is.na(cantidad))  %>%
    head(20) %>%
    ggplot(aes(reorder(Inst, +cantidad), cantidad, fill=country, tooltip = paste0(Inst,", ", country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
    #scale_color_manual(name = paises ,values = jColors) +
    geom_text(aes(label = cantidad, color='black', size=24))
  
  
  girafe(ggobj = ggplot2d, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  
  
  

  output$value1 <- renderValueBox({
    valueBox(
      formatC(iberoamerica  %>% summarize(n_distinct(PMID)), format="d", big.mark=',')
      ,paste('')
      #,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(iberoamerica %>% filter(Fuente == 'PubMed') %>% summarize(n_distinct(PMID)), format="d", big.mark=',')
      ,paste('')
      #,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(iberoamerica %>% filter(Fuente == 'Noticias') %>% summarize(n_distinct(PMID)), format="d", big.mark=',')
      ,paste('')
      #,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value4 <- renderValueBox({
    valueBox(
      formatC(iberoamerica %>% filter(Fuente == 'LaReferencia') %>% summarize(n_distinct(PMID)), format="d", big.mark=',')
      ,paste('')
      #,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
}




ui <- dashboardPage(
  dashboardHeader(title = "Explorador de la investigacion sobre COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "resumen", "Resumen", icon = icon("dashboard")),
      menuItem(tabName = "explorador", "Explorador", icon = icon("table"))
    )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "resumen",
              #fluidRow(h1('Explorador de la investigación sobre COVID-19')),
              fluidRow(h4('Esta pagina contiene el seguimiento de instituciones latinoamericanas activas en la investigacion del COVID-19. Los datos son obtenidos de tres fuentes complementarias: PubMed, noticias periodisticas y la red de repositorios LaReferencia.')),
              fluidRow(h4('En esta pagina encontrara un resumen de cada fuente y en la columna izquierda podra acceder a un explorador mas detallado.')),
              fluidRow(h2('Pub Med')),
              fluidRow(h4("Los datos aqui presentados surgen del procesamiento de la base de datos de revistas cientificas ", a("PubMed", href = "https://pubmed.ncbi.nlm.nih.gov/?term=COVID-19", target="_blank"), "mantenida por el NIH de los Estados Unidos.")),
              fluidRow(
                box(width = 12,title = "Total",color = "green", title_side = "top right",column(width = 12,valueBoxOutput("value2")))),
                         fluidRow(
                           box(width = 8,title = "Distribucion Pais",color = "green", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("paises2"))),
                           box(width = 8, title = "Principales Instituciones",color = "green", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("instituciones2")))
                         ),
fluidRow(h2('Noticias')),
fluidRow(
  box(width = 12,title = "Total",color = "red", title_side = "top right",column(width = 12,valueBoxOutput("value3")))),
fluidRow(h4(" Los datos aqui presentados provienen de un relevamiento de medios de comunicacion realizado por la Oficina de Ciencias para America Latina de", a("UNESCO", href = "https://es.unesco.org/fieldoffice/montevideo/cienciaresponde", target="_blank"))),
              fluidRow(
                box(width = 8,title = "Distribucion Pais",color = "red", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("paises3"))),
                box(width = 8, title = "Principales Instituciones",color = "red", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("instituciones3")))
              ),
fluidRow(h2('La Referencia')),
fluidRow(h4("Los datos aqui presentados surgen del procesamiento de los datos disponibles en ", a("LaReferencia", href = "http://www.lareferencia.info/vufind/Search/Results?sort=year&lookfor=%22COVID19%22+OR++%22SARS-CoV%22+OR++%22HCoV-19%22+OR++%22mes:C000657245%22+OR++%22MERS-CoV%22+OR++%22mesh:COVID-19%22+OR++%22COVID2019%22+OR++%22COVID-19%22+OR++%22SARS-CoV-2%22+OR++%222019+novel+coronavirus%22+OR++%222019-nCoV%22+OR++%22mesh:D045169%22+OR++%22Orthocoronavirinae%22+OR++%22Coronaviridae%22+OR++%22mesh:D045169%22+OR++%22coronavirus%22+&type=AllFields", target="_blank"), "la red latinoamericana de repositorios institucionales de
publicaciones cientificas.")),
fluidRow(
  box(width = 12,title = "Total",color = "blue", title_side = "top right",column(width = 12,valueBoxOutput("value4")))),
              fluidRow(
                box(width = 8,title = "Distribucion Pais",color = "blue", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("paises4"))),
                box(width = 8, title = "Principales Instituciones",color = "blue", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("instituciones4")))
              )), 
      tabItem(tabName = "explorador",fluidRow(column(12,leafletOutput("map"))),
                         fluidRow(column(6, selectInput("country", label = "Seleccione Pais", choices = unique(ordenpais)))),
                         fluidRow(column(6, tags$h4("Instituciones"))),
                         fluidRow(column(12,DT::dataTableOutput("inst_pais"))),
                        fluidRow(column(4, tags$h4("PubMed")),column(4, tags$h4("Noticias")),column(4, tags$h4("LaReferencia"))),
                        fluidRow(column(4,DT::dataTableOutput("inst_paper2")),column(4,DT::dataTableOutput("inst_paper3")),column(4,DT::dataTableOutput("inst_paper4")))
              )
    )))




shinyApp(ui, server)


                
                










