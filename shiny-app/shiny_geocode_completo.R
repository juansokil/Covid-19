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




url<- 'https://www.dropbox.com/s/7jcxenzyd00h9fl/google_shiny.tsv?raw=1' 
#iberoamerica<-read.table(urla, header = TRUE, sep = "\t")
iberoamerica <- read_delim(url, "\t", escape_double = FALSE, locale = locale(), trim_ws = TRUE)

###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

iberoamerica <- iberoamerica %>%
  left_join(countries, by=c('iso'='country'))



rsconnect::setAccountInfo(name='ricyt',
                          token='C67AA1C90915AC03FC71A8864974F7AF',
                          secret='t32ahs3HTt4+77OUwN6BDpHbuWL+xbX+HuKic6O6')




iberoamerica$Date <- parse_date_time(iberoamerica$Date, orders = c("ymd", "dmy", "mdy"))
iberoamerica$Date <-as.Date(format(iberoamerica$Date , "%Y-%m-%d"))




iberoamerica$country <- str_replace(iberoamerica$country, "Brazil", "Brasil")
iberoamerica$country <- str_replace(iberoamerica$country, "Dominican Republic", "Rep. Dominicana")


##########Levanta datos#####


cantidades <- iberoamerica %>%
  group_by(country, iso, latitude, longitude)  %>%
  summarize(cantidad=n_distinct(Link)) %>%
  arrange(desc(cantidad))

ordenpais <- cantidades$country

levels(cantidades$country) <- ordenpais


listado_inst <- iberoamerica %>%
  group_by(country, Inst)  %>%
  summarize(cantidad=n_distinct(Link)) %>%
  arrange(desc(cantidad)) %>%
  unique() %>%
  filter(!is.na(Inst))


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
      #filter (Fuente == 'PubMed')   %>%
      select(Date, Title, Fuente, Link)  %>%
      filter(Fuente %in% input$fuente)  %>%
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
  output$inst_paper <- renderDT(data2(), server = FALSE, escape = FALSE, rownames= FALSE)
  
  
  

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
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "resumen", "Resumen", icon = icon("dashboard")),
      menuItem(tabName = "explorador", "Explorador", icon = icon("table"))
    )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "resumen",
                         fluidRow(
                           box(width = 4,title = "Total",color = "black", title_side = "top right",column(width = 4,valueBoxOutput("value1"))),
                           box(width = 4,title = "PubMed",color = "black", title_side = "top right",column(width = 4,valueBoxOutput("value2"))),
                           box(width = 4,title = "Noticias",color = "black", title_side = "top right",column(width = 4,valueBoxOutput("value3"))),
                           box(width = 4,title = "La Referencia",color = "black", title_side = "top right",column(width = 4,valueBoxOutput("value4")))
                         ),
                         fluidRow(
                           box(width = 8,title = "Distribucion Pais",color = "green", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("paises"))),
                           box(width = 8, title = "Principales Instituciones",color = "red", ribbon = TRUE, title_side = "top right",column(width = 8,girafeOutput("instituciones")))
                         )), 
      tabItem(tabName = "explorador",fluidRow(column(12,leafletOutput("map"))),
                         fluidRow(column(6, selectInput("country", label = "Seleccione Pais", choices = unique(ordenpais))),column(6, selectizeInput("fuente", label = "Seleccione las fuentes", choices = c('PubMed','Noticias','LaReferencia'), selected=c('PubMed','Noticias','LaReferencia'), multiple = TRUE))),
                         fluidRow(column(6, tags$h4("Instituciones")), column(6, tags$h4("Publicaciones"))),
                         fluidRow(column(6,DT::dataTableOutput("inst_pais")), column(6,DT::dataTableOutput("inst_paper")))
              )
    )))



shinyApp(ui, server)


                
                










