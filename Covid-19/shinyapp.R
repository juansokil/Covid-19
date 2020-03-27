
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


nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = 1:75)     # size 
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
    
    
######################PRIMER SOLAPA###########################
    output$plot1 <- renderPlot({pubmed_data %>%
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
    })
    
    output$plot3 <- renderPlot({pubmed_data %>%
            group_by(Date)  %>%
            summarize(dia=n_distinct(PMID))  %>% 
            ggplot(aes(x=Date, y=dia)) + 
            ylab("Papers x Dia") +
            xlab("Date") +
            geom_line(size=2, alpha=0.6) +
            geom_point(size=3, alpha=0.8) +
            geom_smooth(method = "loess", size=2, alpha=0.3)   +  
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Publicaciones cientificas por dia en PubMed")
    })
    
    output$table1 <- renderDT(publicaciones <- pubmed_data %>%
            group_by(Date)  %>%
            summarize(Dia=n_distinct(PMID)) %>% 
            mutate(Acumulado = cumsum(Dia)),
    extensions = 'Buttons',
    options = list(pageLength = 10,
      dom = 'Bfrtip',
       buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
            exportOptions = list(modifiers = list(page = "all")
            )
          ), server = FALSE)
    
    
           
######################SEGUNDA SOLAPA###########################    
  
    
    output$plot5 <- renderPlot({
      pubmed_data %>%
        filter(!is.na(iso))  %>%
        group_by(country, iso) %>%
        summarize(cantidad=n_distinct(PMID)) %>%
        arrange(desc(cantidad)) %>%
        head(15) %>%
        ggplot(aes(reorder(country, +cantidad), cantidad)) + geom_bar(stat='identity') + coord_flip() +  
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
        ggtitle("Publicaciones cientificas acumuladas en PubMed por pais") +
        geom_text(aes(label = cantidad, color='red', size=10))
    })
    

    output$table2 <- renderDT(pubmed_data %>%
                                filter(!is.na(iso))  %>%
                                group_by(country, iso) %>%
                                summarize(cantidad=n_distinct(PMID)) %>%
                                arrange(desc(cantidad)),extensions = 'Buttons',
                              options = list(pageLength = 10,
                                             dom = 'Bfrtip',
                                             buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                             exportOptions = list(modifiers = list(page = "all")
                                             )
                              ), server = FALSE)

    
    
    
    
    
    ######################TERCERA SOLAPA###########################      
    
    
    output$plot2 <- renderPlot({
      pais() %>%
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
        ggtitle("Comparacion de Publicaciones cientificas acumuladas en PubMed entre paises")
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
    
    
######################CUARTA SOLAPA###########################      

    
    output$network <- renderVisNetwork({
      visIgraph(g, type="full", layout = 'layout.norm', layoutMatrix = as.matrix(cbind(countries_coord$longitude, countries_coord$latitude*-1)))  %>%  
        visLegend() %>% 
        visOptions(highlightNearest = T,nodesIdSelection = T)
    
      
    })
        #https://www.rpubs.com/Steven_Surya/visNetwork


#https://riptutorial.com/r/example/16144/dynamic-leaflet-maps-in-shiny-applications

######################CUARTA SOLAPA###########################      


output$map <- renderLeaflet({ 
  leaflet() %>% addProviderTiles(providers$Esri.NatGeoWorldMap) 
})

  
    
#############BOTONES####################
    
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
                  mainPanel(
                      tabsetPanel(type = "tabs",
                               tabPanel("Publicaciones Total",
                                        fluidRow(column(6, plotOutput("plot1")),
                                                 column(6, plotOutput("plot3"))),
                                        #fluidRow(column(12, downloadButton("download1", label = "Descargar datos")),
                                        fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                               tabPanel("Publicaciones por Paises",
                                        fluidRow(column(12, plotOutput("plot5")),
                                        fluidRow(column(12, dataTableOutput(outputId = "table2"))))),
                               tabPanel("Comparativo paises",
                                        fluidRow(column(6, pickerInput(inputId = "country", label = "Seleccione los paises a comparar", choices = listado_paises, selected = c("China","United States"), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE))), 
                                        fluidRow(column(12, plotOutput("plot2")),
                                        fluidRow(column(12, dataTableOutput(outputId = "table3"))))),
                               tabPanel("Grafo de Colaboracion", visNetworkOutput(outputId = "network")),
                              tabPanel("Mapa", leafletOutput(outputId = "map")))))
                        
    


#https://m-clark.github.io/data-processing-and-visualization/ml.html



shinyApp(ui, server)

