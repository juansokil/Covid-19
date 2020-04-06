
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
#library(shinydashboard)
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
library(maps)
library(RColorBrewer)


rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


#setwd('./github/covid-19/scripts')
###Levanto los datos viejos###


#pubmed_data <- read.table("../bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1, colClasses=c(Title="character", Abstract="character", country="character", afil="character", Date="Date"))

#pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", Abstract="character", country="character", afil="character", Date="Date"))
pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(country="character",  Date="Date"))

######################Listado PAISES###############
listado_paises <- pubmed_data %>%
  filter(!is.na(iso))  %>%
  filter(iso!='')  %>%
  group_by(country, iso) %>%
  summarize(cantidad=n_distinct(PMID)) %>%
  arrange(desc(cantidad))  %>%
  select(country)

variable_selector <- pubmed_data %>% select(chloroquine, hydroxychloroquine) %>% names()

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
vector_colores <- as.character(sample(color, nrow(listado_paises)))

jColors <- vector_colores
names(jColors) <- listado_paises$country



#######################ARMA NODOS #######################
nodos <- pubmed_data %>%
  select(iso, PMID) %>%
  unique()  %>%
  filter(!is.na(iso)) %>%
  filter(iso!='')  %>%
  group_by(iso) %>%
  summarize(totales=n_distinct(PMID))

#######################ARMA VERTICES #######################

byHand <- pubmed_data %>%
  select(PMID, iso) %>%
  unique() %>%
  filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
  group_by(PMID) %>% mutate(paises = paste(iso, collapse = ","))

#### identifico el primero de los autores###
byHand$primer_pais = as.character(lapply(strsplit(as.character(byHand$paises), split=","), "[", 1))
arreglado <- byHand %>% mutate(paises = strsplit(as.character(paises), ",")) %>% unnest(paises)
aristas_previo <- arreglado[c(3:4)]
colnames(aristas_previo) <- c("source","target")
aristas <- aristas_previo %>% group_by(source, target) %>% summarize(count=n())





nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = nodos$totales, count=nodos$totales)     # size 
#nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = 1:nrow(listado_paises), count=nodos$totales)     # size 
#edges <- data.frame(from = aristas$source, to = aristas$target)
edges <- data.frame(source = aristas$source, target = aristas$target, weight = aristas$count)
edges <- edges %>%
  filter(as.character(source) != as.character(target))



###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

countries_coord <- nodes %>%
  left_join(countries, by=c('id'='country'))

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
g<- simplify(g, remove.multiple = TRUE)

edges_for_plot <- edges %>%
  inner_join(countries_coord %>% select(id, longitude, latitude), by = c('source' = 'id')) %>%
  rename(x = longitude, y = latitude) %>%
  inner_join(countries_coord %>% select(id, longitude, latitude), by = c('target' = 'id')) %>%
  rename(xend = longitude, yend = latitude)


maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))


country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)



#https://datascience.blog.wzb.eu/2018/05/31/three-ways-of-visualizing-a-graph-on-a-map/

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
        filter(iso!='')  %>%
        group_by(country, iso) %>%
        summarize(cantidad=n_distinct(PMID)) %>%
        arrange(desc(cantidad)) %>%
        head(15) %>%
        ggplot(aes(reorder(country, +cantidad), cantidad)) + geom_bar(stat='identity') + coord_flip() +  
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
        #scale_color_manual(name = paises ,values = jColors) +
        ggtitle("Publicaciones cientificas acumuladas en PubMed por pais") +
        geom_text(aes(label = cantidad, color='red', size=10))
    })
    

    output$table2 <- renderDT(pubmed_data %>%
                                filter(!is.na(iso))  %>%
                                filter(iso!='')  %>%
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
        geom_line(size=3, alpha=1) +
        geom_point(size=4, alpha=1) +
        #geom_smooth(method = "loess", size=2, alpha=0.3)  +  
        scale_color_manual(values = jColors) +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
        ggtitle("Comparacion de Publicaciones cientificas acumuladas en PubMed entre paises")
    })
    
    
    
  
    
    output$table3 <- renderDT(pais() %>%
                                arrange(country, Date) %>%
                                group_by(country, Date) %>%
                                summarize(dia=n_distinct(PMID)) %>% 
                                mutate(total = cumsum(dia)) %>%
                                arrange(Date),extensions = 'Buttons',
                              options = list(pageLength = 10,
                                             dom = 'Bfrtip',
                                             buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                             exportOptions = list(modifiers = list(page = "all")
                                             )
                              ), server = FALSE)
    
    
######################CUARTA SOLAPA###########################      

    
    output$network <- renderVisNetwork({
      E(g)$width <- E(g)$weight/8
      
      visIgraph(g, type="full", layout = 'layout.norm', layoutMatrix = as.matrix(cbind(countries_coord$longitude, countries_coord$latitude*-1)))  %>%  
        visLegend() %>% 
        #visEdges( width = weight) %>% 
        visNodes(scaling = list(min = 10, max = 100)) %>% 
        visOptions(highlightNearest = T,nodesIdSelection = T)
    
      
    })
        
    
  
    output$map <- renderPlot({ ggplot(countries_coord) + country_shapes +
      geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
                 data = edges_for_plot,
                 alpha = 0.5) +
      #geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
      #           data = edges_for_plot,
      #           alpha = 0.5) +
      geom_point(aes(x = longitude, y = latitude, size=count),           # draw nodes
                 shape = 21, fill = 'white',  
                 color = 'black', stroke = 0.5) +
      scale_size_continuous(guide = FALSE, range = c(2, 20)) +    # scale for node size
      geom_text(aes(x = longitude, y = latitude, label = id),             # draw text labels
                hjust = 0, nudge_x = 1, nudge_y = 4,
                size = 3, color = "red", fontface = "bold")
    
    })
    
    
    
    
    
    
    
    
    
    
    
    
######################CUARTA SOLAPA###########################      


#output$map <- renderLeaflet({ 
#  leaflet() %>% addProviderTiles(providers$Esri.NatGeoWorldMap) 
#})

  
    
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
    

    
      output$logo <-
        renderText({
          c(
            '<img src="',
            "./oeiocts.jpg",
            '">'
          )
        })
      
      
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
#  titlePanel( fluidRow(column(width = 6, h2("Publicaciones Cientificas relacionadas con Covid-19")), 
#                  column(width = 6, tags$img(src="https://raw.githubusercontent.com/juansokil/Covid-19/master/Covid-19/oeiocts.jpeg", 
#                                             height='100',width='100', align="left", href="https://observatoriocts.oei.org.ar/")))),
    titlePanel( fluidRow(column(width = 6, h2("Publicaciones Cientificas relacionadas con Covid-19")), 
                    column(width = 6, tags$a(
                                             href="https://observatoriocts.oei.org.ar",  
                                             tags$img(src="https://raw.githubusercontent.com/juansokil/Covid-19/master/Covid-19/oeiocts.jpeg", 
                                                      height='100',width='100', align="left")
                    )))),
                    mainPanel(
                      tabsetPanel(type = "tabs",
                               tabPanel("Publicaciones Total",
                                        fluidRow(column(6, plotOutput("plot1")),
                                                 column(6, plotOutput("plot3"))),
                                        fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                               tabPanel("Publicaciones por Paises",
                                        fluidRow(column(12, plotOutput("plot5")),
                                        fluidRow(column(12, dataTableOutput(outputId = "table2"))))),
                               tabPanel("Comparativo paises",
                                        fluidRow(column(6, pickerInput(inputId = "country", label = "Seleccione los paises a comparar", choices = listado_paises, selected = c("China","United States"), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE))), 
                                        fluidRow(column(12, plotOutput("plot2")),
                                        fluidRow(column(12, dataTableOutput(outputId = "table3"))))),
                               tabPanel("Grafo de Colaboracion", visNetworkOutput(outputId = "network"))
                             #  tabPanel("Grafo de Colaboracion2", plotOutput(outputId = "map")),
                               #tabPanel("Conceptos", varSelectInput("variable", "Seleccione :", variable_selector)),
                              #tabPanel("Mapa", leafletOutput(outputId = "map"))
                              )))
                        
    



shinyApp(ui, server)


