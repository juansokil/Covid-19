
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(DT)  
library(countrycode)
library(stringr)
library(igraph)
library(tidyr)
library(readr)
library(visNetwork)
library(maps)
library(RColorBrewer)
library(ggiraph)


rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


#setwd('./github/covid-19/scripts')
###Levanto los datos viejos###
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
edges <- data.frame(source = aristas$source, target = aristas$target, weight = aristas$count)
edges <- edges %>%
  filter(as.character(source) != as.character(target))

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
  
  
  output$plot1b <- renderGirafe({ggplot1 <- pubmed_data %>%
    group_by(Date)  %>%
    summarize(dia=n_distinct(PMID))  %>% 
    mutate(total = cumsum(dia))  %>% 
    ggplot(aes(x=Date, y=total)) + 
    ylab("total de papers") +
    xlab("Date") +
    geom_line(size=2, alpha=0.6) +
    geom_smooth(method = "loess", size=2, alpha=0.3)   +  
    geom_point_interactive(aes(x=Date, y=total, size=3, alpha=0.8, tooltip = paste0(Date,": ",total))) +
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Publicaciones cientificas acumuladas en PubMed")  
  
  
  girafe(ggobj = ggplot1, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  

  
  
  ##########################################GRAFICO NRO2###########################
  
  
  output$plot2b <- renderGirafe({
    ggplot3 <- pubmed_data %>%
      filter(!is.na(iso))  %>%
      filter(iso!='')  %>%
      group_by(country, iso) %>%
      summarize(cantidad=n_distinct(PMID)) %>%
      arrange(desc(cantidad)) %>%
      head(15) %>%
      ggplot(aes(reorder(country, +cantidad), cantidad, tooltip = paste0(country,": ", cantidad ))) + geom_bar_interactive(stat='identity') + coord_flip() +
      theme_minimal() + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
      #scale_color_manual(name = paises ,values = jColors) +
      ggtitle("Publicaciones cientificas acumuladas en PubMed por pais") +
      geom_text(aes(label = cantidad, color='red', size=10))
    
    
    girafe(ggobj = ggplot3, 
           options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  
  
  ##########################################GRAFICO NRO3###########################
  
  
  output$plot3b <- renderGirafe({ggplot3 <-
     pais() %>%
      arrange(country, Date) %>%
      group_by(country, Date) %>%
      summarize(dia=n_distinct(PMID)) %>% 
      mutate(total = cumsum(dia))  %>% 
      ggplot(aes(x=Date, y=total, color=country)) + 
      ylab("Comparar paises") +
      xlab("Date") +
      #geom_line(size=2, alpha=1) +
      #geom_point(size=3, alpha=1) +
      geom_line_interactive(size = 3, alpha=1)  +
      geom_point_interactive(aes(x=Date, y=total, size=4, alpha=0.8, tooltip = paste0(country, "\n",Date,": ",total))) +
      #geom_smooth(method = "loess", size=2, alpha=0.3)  +  
      scale_color_manual(values = jColors) +
    theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
      ggtitle("Comparacion de Publicaciones cientificas  \n en PubMed entre paises") +
    guides(size=FALSE, alpha=FALSE)
    
  girafe(ggobj = ggplot3, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  

  
  ##########################################GRAFICO NRO4###########################
  
  
  output$network <- renderVisNetwork({
    E(g)$width <- E(g)$weight/8
    
    visIgraph(g, type="full", layout = 'layout.norm', layoutMatrix = as.matrix(cbind(countries_coord$longitude, countries_coord$latitude*-1)))  %>%  
      visLegend() %>% 
      #visEdges( width = weight) %>% 
      visNodes(scaling = list(min = 10, max = 100)) %>% 
      visOptions(highlightNearest = T,nodesIdSelection = T)
    
  })
  
  
  
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
                         fluidRow(column(12, girafeOutput("plot1b"))),
                         fluidRow(column(12, downloadButton("download1", label = "Descargar datos")))),
                #fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                tabPanel("Publicaciones cientificas por pais",
                         fluidRow(column(12, girafeOutput("plot2b"))),
                         fluidRow(column(12, downloadButton("download2", label = "Descargar datos")))),
                #fluidRow(column(12, dataTableOutput(outputId = "table2")))),
                tabPanel("Comparacion entre paises", 
                         fluidRow(column(6, pickerInput(inputId = "country", label = "Seleccione los paises a comparar", choices = listado_paises, selected = c("China","United States"), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion", 'count-selected-text' = "{0} seleccionados."), multiple = TRUE))), 
                         fluidRow(column(12, girafeOutput("plot3b"))),
                         fluidRow(column(12, downloadButton("download3", label = "Descargar datos")))),
                #fluidRow(column(12, dataTableOutput(outputId = "table3")))),
                tabPanel("Colaboracion entre paises", visNetworkOutput(outputId = "network"))
                
    )))







shinyApp(ui, server)


