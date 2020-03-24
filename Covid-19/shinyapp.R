
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
  select(country) %>%
distinct(country)


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
countries <- read_delim("~/GitHub/Covid-19/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

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
            theme_bw()
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
            theme_bw()
    })
    
    output$table1 <- renderDT(pubmed_data %>%
            group_by(Date)  %>%
            summarize(Dia=n_distinct(PMID))  %>% 
            mutate(Acumulado = cumsum(Dia)),  
            options = list(pageLength = 10))
            
    
######################SEGUNDA SOLAPA###########################    
  
    
    output$plot5 <- renderPlot({
      pubmed_data %>%
        filter(!is.na(iso))  %>%
        group_by(country, iso) %>%
        summarize(cantidad=n_distinct(PMID)) %>%
        arrange(desc(cantidad)) %>%
        head(15) %>%
        ggplot(aes(reorder(country, +cantidad), cantidad)) + geom_bar(stat='identity') + coord_flip() +  
        theme_bw()
    })
    

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
            theme_bw()
    })
    
    output$table2 <- renderDT(pubmed_data %>%
                                filter(!is.na(iso))  %>%
                                group_by(country, iso) %>%
                                summarize(cantidad=n_distinct(PMID)) %>%
                                arrange(desc(cantidad)),  
                              options = list(pageLength = 10))
    
     
    
######################TERCERA SOLAPA###########################      

    edges <- data.frame(from = c("AR","BR"), to = c("CN","US"))
    
    output$network <- renderVisNetwork({
      visIgraph(g, type="full", layout = 'layout.norm', layoutMatrix = as.matrix(cbind(countries_coord$longitude, countries_coord$latitude*-1)))
    
      
    })
        
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Aplicacion ejemplo"),
    #sidebarLayout(pickerInput(inputId = "country", label = "Seleccione los topicos",  choices = unique(pubmed_data$country), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE),
                  # Show a plot of the generated distribution
                  mainPanel(
                      tabsetPanel(type = "tabs",
                               tabPanel("Totales",
                                        fluidRow(column(6, plotOutput("plot1")),
                                                 column(6, plotOutput("plot3"))),
                                        fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                               tabPanel("Paises",
                                        fluidRow(column(6, pickerInput(inputId = "country", label = "Seleccione los topicos", choices = listado_paises, options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE))), 
                                        fluidRow(column(6, plotOutput("plot5")),
                                                 column(6, plotOutput("plot2"))),
                                        fluidRow(column(12, dataTableOutput(outputId = "table2")))),
                               tabPanel("Grafo", visNetworkOutput(outputId = "network")))
                        ))
    






shinyApp(ui, server)




