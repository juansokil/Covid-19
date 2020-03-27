
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)  
library(countrycode)
library(stringr)
library(igraph)
library(visNetwork)
library(tidyr)
library(readr)
#install.packages("leaflet")
library(leaflet)
library(plotly)



rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


#setwd('./github/covid-19/scripts')
###Levanto los datos viejos###
pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                          colClasses=c(Title="character", Abstract="character", country="character", afil="character", Date="Date"))


######################Listado PAISES###############

listado_paises <- pubmed_data %>%
  filter(!is.na(iso))  %>%
  group_by(country, iso) %>%
  summarize(cantidad=n_distinct(PMID)) %>%
  arrange(desc(cantidad))  %>%
  select(country)

#######################ARMA NODOS #######################
nodos <- pubmed_data %>%
  select(iso, PMID) %>%
  unique()  %>%
  filter(!is.na(iso)) %>%
  group_by(iso) %>%
  summarize(totales=n_distinct(PMID))

#######################ARMA VERTICES #######################

byHand <- pubmed_data %>%
  select(PMID, iso) %>%
  unique() %>%
  filter(!is.na(iso))  %>%
  group_by(PMID) %>% mutate(paises = paste(iso, collapse = ","))

#### identifico el primero de los autores###
byHand$primer_pais = as.character(lapply(strsplit(as.character(byHand$paises), split=","), "[", 1))
arreglado <- byHand %>% mutate(paises = strsplit(as.character(paises), ",")) %>% unnest(paises)
aristas_previo <- arreglado[c(3:4)]
colnames(aristas_previo) <- c("source","target")
aristas <- aristas_previo %>% group_by(source, target) %>% summarize(count=n())

nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = 1:nrow(listado_paises))     # size 
edges <- data.frame(from = aristas$source, to = aristas$target)

###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

countries_coord <- nodes %>%
  left_join(countries, by=c('id'='country'))

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
g<- simplify(g, remove.multiple = TRUE)




###################SERVER#####################
server <- function(input, output, session) {
  
  
  
  pais <- reactive({
    a <- pubmed_data  %>% filter(country %in% input$country)
    a <- data.frame(a)
    return(a)
  })    
  
  
  ##########################################GRAFICO NRO1###########################
  
  
  output$plot1b <- renderPlotly({
    plot1b <- pubmed_data %>%
      group_by(Date)  %>%
      summarize(dia=n_distinct(PMID))  %>% 
      mutate(total = cumsum(dia))  %>% 
      ggplot(aes(x=Date, y=total)) + 
      ylab("total de papers") +
      xlab("Date") +
      geom_line(size=2, alpha=0.6) +
      geom_point(size=3, alpha=0.8) +
      geom_smooth(method = "loess", size=2, alpha=0.3)   +  
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5)) +
      ggtitle("Publicaciones cientificas acumuladas en PubMed")
    
    ggplotly(plot1b)
  })
  
  #output$table1 <- renderDT(pubmed_data %>%
  #                            group_by(Date)  %>%
  #                            summarize(Dia=n_distinct(PMID)) %>% 
  #                            mutate(Acumulado = cumsum(Dia)),
  #                          extensions = 'Buttons',
  #                          options = list(pageLength = 1,
  #                                         dom = 'Bfrtip',
  #                                         buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
  #                                         exportOptions = list(modifiers = list(page = "all")
  #                                         )
  #                          ), server = FALSE)
  
  
  
  
  
  ##########################################GRAFICO NRO2###########################
  
  
  output$plot2b <- renderPlotly({
    cantidad_paises <- pubmed_data %>%
      filter(!is.na(iso))  %>%
      group_by(country, iso) %>%
      summarize(cantidad=n_distinct(PMID)) %>%
      arrange(desc(cantidad)) 
    
    cantidad_paises$country <- factor(cantidad_paises$country, levels = cantidad_paises$country[order(cantidad_paises$cantidad)])
    cantidad_paises <- head(cantidad_paises, 15)
    
    plot2b <- ggplot(cantidad_paises, aes(country, cantidad)) + geom_bar(stat='identity') + coord_flip() +  
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
      ggtitle("Publicaciones cientificas acumuladas  \n en PubMed por pais")
    
    ggplotly(plot2b)
  })
  
  
  
  # output$table2 <- renderDT(pubmed_data %>%
  #    filter(!is.na(iso))  %>%
  #    group_by(country, iso) %>%
  #    summarize(cantidad=n_distinct(PMID)) %>%
  #    arrange(desc(cantidad)) ,extensions = 'Buttons',
  #                          options = list(pageLength = 1,
  #                                         dom = 'Bfrtip',
  #                                         buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
  #                                         exportOptions = list(modifiers = list(page = "all")
  #                                         )
  #                          ), server = FALSE)
  
  
  ##########################################GRAFICO NRO3###########################
  
  
  output$plot3b <- renderPlotly({
    plot3b <- pais() %>%
      arrange(country, Date) %>%
      group_by(country, Date) %>%
      summarize(dia=n_distinct(PMID)) %>% 
      mutate(total = cumsum(dia))  %>% 
      ggplot(aes(x=Date, y=total, color=country)) + 
      ylab("Comparar paises") +
      xlab("Date") +
      geom_line(size=2, alpha=0.6) +
      geom_point(size=3, alpha=0.8) +
      #geom_smooth(method = "loess", size=2, alpha=0.3)  +  
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
      ggtitle("Comparacion de Publicaciones cientificas  \n en PubMed entre paises")
    
    ggplotly(plot3b)
  })
  
  output$table3 <- renderDT(pais() %>%
                              arrange(country, Date) %>%
                              group_by(country, Date) %>%
                              summarize(dia=n_distinct(PMID)) %>% 
                              mutate(total = cumsum(dia))  ,extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  
  
  
  
  ##########################################GRAFICO NRO4###########################
  
  
  output$network <- renderVisNetwork({
    visIgraph(g, type="full", layout = 'layout.norm', layoutMatrix = as.matrix(cbind(countries_coord$longitude, countries_coord$latitude*-1)))  %>%  
      visLegend(main = "Colaboracion entre paises", position = "left",zoom = FALSE) %>% 
      visOptions(highlightNearest = T,nodesIdSelection = T)
    
  })
  #https://www.rpubs.com/Steven_Surya/visNetwork
  
  
  
  output$download1 <- downloadHandler(
    filename = "publicaciones.csv",
    content = function(file) {
      readr::write_csv(pubmed_data %>%
                         group_by(Date)  %>%
                         summarize(Dia=n_distinct(PMID)) %>% 
                         mutate(Acumulado = cumsum(Dia)), file)
    }
  )
  
  
  
  output$download2 <- downloadHandler(
    filename = "paises.csv",
    content = function(file) {
      readr::write_csv(pubmed_data %>%
                         filter(!is.na(iso))  %>%
                         group_by(country, iso) %>%
                         summarize(cantidad=n_distinct(PMID)) %>%
                         arrange(desc(cantidad)), file)
    }
  )
  
  
  
  output$download3 <- downloadHandler(
    filename = "publicaciones_pais.csv",
    content = function(file) {
      readr::write_csv(pais() %>%
                         arrange(country, Date) %>%
                         group_by(country, Date) %>%
                         summarize(dia=n_distinct(PMID)) %>% 
                         mutate(total = cumsum(dia)) , file)
    }
  )
  
  
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Publicaciones Cientificas relacionadas con Covid-19"),
  #sidebarLayout(pickerInput(inputId = "country", label = "Seleccione los topicos",  choices = unique(pubmed_data$country), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE),
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Publicaciones cientificas",
                         fluidRow(column(12, plotlyOutput("plot1b"))),
                         fluidRow(column(12, downloadButton("download1", label = "Descargar datos")))),
                #fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                tabPanel("Publicaciones cientificas por pais",
                         fluidRow(column(12, plotlyOutput("plot2b"))),
                         fluidRow(column(12, downloadButton("download2", label = "Descargar datos")))),
                #fluidRow(column(12, dataTableOutput(outputId = "table2")))),
                tabPanel("Comparacion entre paises", 
                         fluidRow(column(6, pickerInput(inputId = "country", label = "Seleccione los paises a comparar", choices = listado_paises, selected = c("China","United States"), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion", 'count-selected-text' = "{0} seleccionados."), multiple = TRUE))), 
                         fluidRow(column(12, plotlyOutput("plot3b"))),
                         fluidRow(column(12, downloadButton("download3", label = "Descargar datos")))),
                #fluidRow(column(12, dataTableOutput(outputId = "table3")))),
                tabPanel("Colaboracion entre paises", visNetworkOutput(outputId = "network"))
                
    )))







shinyApp(ui, server)


