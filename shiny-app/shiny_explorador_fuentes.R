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
  filter(Fuente == 'LA Referencia')  %>%
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
  filter(Fuente == 'LA Referencia')  %>%
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
  
  

  data <- reactive({
    listado_inst %>% 
      filter (country %in% input$country) %>% 
      rename(Pais=country)  %>% 
      select(Inst, PubMed=cantidadPM, 'Notas Periodisticas'=cantidadN, 'LA Referencia'=cantidadLR)  %>%
      filter(!is.na(Inst))  %>%
      filter(!Inst =='')  %>%
      arrange(desc(PubMed))  %>% column_to_rownames(var = "Inst")
  })
  
  
  data2 <- reactive({
    iberoamerica %>%
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      filter (Fuente == 'PubMed')   %>%
      select(Date, Title, Link)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver</a>")) %>%
      select(Fecha=Date, Titulo=Title, Vinculo=Link)  %>%
      arrange(desc(Fecha))  
    
    
  })
  
  
  
  data3 <- reactive({
    iberoamerica %>%
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      filter (Fuente == 'Noticias')   %>%
      select(Date, Title, Link)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver</a>")) %>%
      select(Fecha=Date, Titulo=Title, Vinculo=Link)  %>%
      arrange(desc(Fecha))  
    
    
  })
  
  
  data4 <- reactive({
    iberoamerica %>%
      filter (country %in% input$country) %>% 
      filter(Inst %in% paste0(rownames(data())[input$inst_pais_rows_selected], collapse = ", "))  %>% 
      filter (Fuente == 'LA Referencia')   %>%
      select(Date, Title, Link)  %>%
      unique() %>%
      mutate(Link=paste0("<a href=",Link," target='_blank' >Ver</a>")) %>%
      select(Fecha=Date, Titulo=Title, Vinculo=Link)  %>%
      arrange(desc(Fecha))  
    
    
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
  
  
  
  output$paises <- renderHighchart({ cantidades %>% 
      arrange(desc(cantidad)) %>%
      rename(Pais=country, Cantidad=cantidad)  %>%
    hchart(
      'bar', hcaes(x = Pais, y = Cantidad, color = Pais)
    )
  })
  
  
  output$paises2 <- renderHighchart({ cantidades %>%
      select(country, iso, latitude, longitude, cantidad=cantidadPM)  %>%
      filter(!is.na(cantidad))%>% 
      arrange(desc(cantidad)) %>%
      rename(Pais=country, Cantidad=cantidad)  %>%
      hchart(
        'bar', hcaes(x = Pais, y = Cantidad, color = Pais)
      )
  })
  
  output$paises3 <- renderHighchart({ cantidades %>%
      select(country, iso, latitude, longitude, cantidad=cantidadN)  %>%
      filter(!is.na(cantidad))%>% 
      arrange(desc(cantidad)) %>%
      rename(Pais=country, Cantidad=cantidad)  %>%
      hchart(
        'bar', hcaes(x = Pais, y = Cantidad, color = Pais)
      )
  })
  
  output$paises4 <- renderHighchart({ cantidades %>%
      select(country, iso, latitude, longitude, cantidad=cantidadLR)  %>%
      filter(!is.na(cantidad))%>% 
      arrange(desc(cantidad)) %>%
      rename(Pais=country, Cantidad=cantidad)  %>%
      hchart(
        'bar', hcaes(x = Pais, y = Cantidad, color = Pais)
      )
  })

  

  
  output$instituciones <- renderHighchart({ listado_inst %>%
      arrange(desc(cantidad)) %>%
      head(20) %>%
      rename(Pais=country, Cantidad=cantidad, Institucion=Inst)  %>%
      hchart(
        'bar', hcaes(x = Institucion, y = Cantidad, color = Pais)
      )
  })
  
  
  output$instituciones2 <- renderHighchart({ listado_inst %>%
      select(country, Inst, cantidad=cantidadPM)  %>%
      filter(!is.na(cantidad))  %>%
      arrange(desc(cantidad)) %>%
      head(20) %>%
      rename(Pais=country, Cantidad=cantidad, Institucion=Inst)  %>%
      hchart(
        'bar', hcaes(x = Institucion, y = Cantidad, color = Pais)
      )
  })
  
  output$instituciones3 <- renderHighchart({ listado_inst %>%
      select(country, Inst, cantidad=cantidadN)  %>%
      filter(!is.na(cantidad))  %>%
      arrange(desc(cantidad)) %>%
      head(20) %>%
      rename(Pais=country, Cantidad=cantidad, Institucion=Inst)  %>%
      hchart(
        'bar', hcaes(x = Institucion, y = Cantidad, color = Pais)
      )
  })
  
  output$instituciones4 <- renderHighchart({ listado_inst %>%
      select(country, Inst, cantidad=cantidadLR)  %>%
      filter(!is.na(cantidad))  %>%
      arrange(desc(cantidad)) %>%
      head(20) %>%
      rename(Pais=country, Cantidad=cantidad, Institucion=Inst)  %>%
      hchart(
        'bar', hcaes(x = Institucion, y = Cantidad, color = Pais)
      )
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
      formatC(iberoamerica %>% filter(Fuente == 'LA Referencia') %>% summarize(n_distinct(PMID)), format="d", big.mark=',')
      ,paste('')
      #,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
}



ui <- dashboardPage(
  dashboardHeader(title = "Explorador de la investigacion sobre COVID-19"),
  dashboardSidebar(size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "resumen", "Resumen", icon = icon("dashboard")),
      menuItem(tabName = "explorador", "Explorador", icon = icon("table"))
    )
    ),
  dashboardBody(
    tags$head(
      tags$style(
        "body{
    max-height: 3000px;
    height: auto;
    max-width: 3000px;
    margin: auto;
        }"
      )
    ),
    tabItems(
      tabItem(tabName = "resumen",

              
              fluidRow(h4('Este explorador ofrece un seguimiento actualizado de las instituciones cientificas y tecnologicas que realizan investigacion sobre COVID-19 en America Latina. Cuenta con un resumen estadistico de la produccion cientifica de cada una y acceso a sus articulos cientificos y noticias de prensa.')),
              fluidRow(h4('Los datos fueron procesados por el Observatorio Iberoamericano de la Ciencia, la Tecnologia y la Sociedad (OCTS) a partir de tres fuentes complementarias: la base de datos de revistas cientificas PubMed, la Red Federada de Repositorios Institucionales de Publicaciones Cientificas (LA Referencia) y un conjunto de notas periodisticas relevadas por la Oficina de Ciencias para America Latina de la UNESCO.')),              
              fluidRow(h4('Desde el menu de la barra izquierda es posible acceder a la informacion por dos caminos: un resumen grafico que surge del procesamiento realizado con cada fuente y un explorador mas detallado donde se puede desgranar los datos segun pais e institucion y acceder a los articulos cientificos y noticias en sus fuentes originales.')),              
              fluidRow(h2('Articulos Cientificos en PubMed')),
              fluidRow(h4("PubMed es una base de datos desarrollada por el NIH de Estados Unidos que permite acceder a referencias bibliograficas y resumenes de articulos de investigacion biomedica publicados en alrededor de 4800 revistas de mas de 70 paises del mundo.")), 
                            fluidRow(h4("La informacion aqui presentada surge del procesamiento realizado por el", a("OCTS", href = "https://observatoriocts.oei.org.ar/", target="_blank"), "para la identificacion de instituciones firmantes de articulos indexados en ", a("PubMed", href = "https://pubmed.ncbi.nlm.nih.gov/?term=covid+19", target="_blank"))), 
              fluidRow(
                box(width = 12,title = "Total de articulos de investigacion biomedica",color = "green", title_side = "top right",column(width = 12,valueBoxOutput("value2")))),
                         fluidRow(
                           box(width = 8,title = "Distribucion Pais",color = "green", ribbon = TRUE, title_side = "top right",column(width = 8,highchartOutput("paises2"))),
                           box(width = 8, title = "Principales Instituciones",color = "green", ribbon = TRUE, title_side = "top right",column(width = 8, highchartOutput("instituciones2")))
                         ),
fluidRow(h2('Notas Periodisticas Seleccionadas')),
fluidRow(
  box(width = 12,title = "Total de notas periodisticas",color = "red", title_side = "top right",column(width = 12,valueBoxOutput("value3")))),
fluidRow(h4(" Los datos aqui presentados provienen de un relevamiento de notas periodisticas en medios de comunicacion de toda la region, realizado por la ", a("Oficina Regional de Ciencias para America Latina y el Caribe de UNESCO", href = "https://es.unesco.org/fieldoffice/montevideo/cienciaresponde", target="_blank"))),
              fluidRow(
                box(width = 8,title = "Distribucion Pais",color = "red", ribbon = TRUE, title_side = "top right",column(width = 8,highchartOutput("paises3"))),
                box(width = 8, title = "Principales Instituciones",color = "red", ribbon = TRUE, title_side = "top right",column(width = 8, highchartOutput("instituciones3")))
              ),
fluidRow(h2('Articulos en repositorios abiertos - LA Referencia')),
fluidRow(h4("La Red Federada de Repositorios Institucionales de Publicaciones Cientificas (LA Referencia), es una red latinoamericana de repositorios de acceso abierto. Integra articulos cientificos, tesis doctorales y de maestria provenientes de mas de un centenar de universidades e instituciones de investigacion America Latina.")), 
fluidRow(h4("La informacion aqui presentada surge del procesamiento de los datos realizado por el", a("OCTS", href = "https://observatoriocts.oei.org.ar/", target="_blank")," de la plataforma de ", a("LA Referencia", href = "http://www.lareferencia.info/vufind/Search/Results?sort=year&lookfor=%22COVID19%22+OR++%22SARS-CoV%22+OR++%22HCoV-19%22+OR++%22mes:C000657245%22+OR++%22MERS-CoV%22+OR++%22mesh:COVID-19%22+OR++%22COVID2019%22+OR++%22COVID-19%22+OR++%22SARS-CoV-2%22+OR++%222019+novel+coronavirus%22+OR++%222019-nCoV%22+OR++%22mesh:D045169%22+OR++%22Orthocoronavirinae%22+OR++%22Coronaviridae%22+OR++%22mesh:D045169%22+OR++%22coronavirus%22+&type=AllFields", target="_blank"))),
fluidRow(
  box(width = 12,title = "Total de articulos cientificos",color = "blue", title_side = "top right",column(width = 12,valueBoxOutput("value4")))),
              fluidRow(
                box(width = 8,title = "Distribucion Pais",color = "blue", ribbon = TRUE, title_side = "top right",column(width = 8,highchartOutput("paises4"))),
                box(width = 8, title = "Principales Instituciones",color = "blue", ribbon = TRUE, title_side = "top right",column(width = 8, highchartOutput("instituciones4")))
              )), 
      tabItem(tabName = "explorador",fluidRow(column(15,leafletOutput("map"))),
                         fluidRow(column(6, selectInput("country", label = "Seleccione Pais", choices = unique(ordenpais)))),
                         fluidRow(column(6, tags$h4("Instituciones"))),
                         fluidRow(column(15,DT::dataTableOutput("inst_pais"))),
                        fluidRow(column(5, tags$h4("PubMed")),column(5, tags$h4("Notas Periodisticas")),column(5, tags$h4("LA Referencia"))),
                        fluidRow(column(5,DT::dataTableOutput("inst_paper2")),column(5,DT::dataTableOutput("inst_paper3")),column(5,DT::dataTableOutput("inst_paper4")))
              )
    )))




shinyApp(ui, server)


                
                






