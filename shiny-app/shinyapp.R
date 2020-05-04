
library(data.table)
library(DT)  
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(countrycode)
library(stringr)
library(igraph)
library(visNetwork)
library(tidyr)
library(readr)
library(leaflet)
library(maps)
library(RColorBrewer)
library(ggiraph)
library(leaflet.minicharts)
library(htmltools)
library(treemapify)



rsconnect::setAccountInfo(name='observatorio-cts',
                          token='EFB80756D7A13A771E3F6419EA165929',
                          secret='DfBCakMJl3pZLkwvNS5aGELaMZ95GK5A72rGazAh')


pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", country="character",  Inst="character", Date="Date"))


keyword_cors <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/palabras_cors.csv", header = TRUE, sep = "\t", row.names = 1)
keyword_cant <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/palabras_cant.csv", header = TRUE, sep = "\t", row.names = 1)


###Levanto los datos viejos################REVISAR ESTO MAÑANA####
#pubmed_data <- read.table("../Bases/pubmed_data_bck.csv", header = TRUE, sep = "\t", row.names = 1,
#                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))

#pubmed_data <- pubmed_data_reduce

pubmed_data$Date <- as.Date(with(pubmed_data, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")
pubmed_data$lat <- as.numeric(as.character(pubmed_data$lat))
pubmed_data$long <- as.numeric(as.character(pubmed_data$long))





####Calcula cantidades#####
edges_for_plot <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/edges_for_plot.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(dia="Date"))
edges_for_plot_ud <- edges_for_plot %>%
  filter(dia == max(dia))

#######################ARMA NODOS CANTIDADES #######################
  nodos <- pubmed_data %>%
    select(iso, PMID) %>%
    unique()  %>%
    filter(!is.na(iso)) %>%
    filter(iso!='')  %>%
    group_by(iso) %>%
    summarize(totales=n_distinct(PMID))
  nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = nodos$totales, count=nodos$totales)     # size 


######################Listado PAISES###############
listado_paises <- pubmed_data %>%
  filter(!is.na(iso))  %>%
  filter(iso!='')  %>%
  group_by(country, iso) %>%
  summarize(cantidad=n_distinct(PMID)) %>%
  arrange(desc(cantidad))  %>%
  select(country)

  
listado_iso_country <- pubmed_data %>%
    select(iso, country) %>%
    filter(!is.na(iso))  %>%
    unique()
  
  
  variable_selector <- pubmed_data %>%
    select(PMID, chloroquine, hydroxychloroquine, remdesivir, ritonavir, lopinavir, favipiravir,  azithromycin, tocilizumab, antivirals, interferon, antibodies, vaccine) %>%
    gather(key = "variable", value = "valor", -c(PMID)) %>%
    select(variable)  %>%
    unique()
  

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
vector_colores <- as.character(sample(color, nrow(listado_paises)))
vector_colores2 <- as.character(sample(color, nrow(variable_selector)))


jColors <- vector_colores
jColors2 <- vector_colores2
names(jColors) <- listado_paises$country
names(jColors2) <- variable_selector$variable





###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)
countries_coord <- nodes %>%
  left_join(countries, by=c('id'='country'))

merge_pais <- pubmed_data %>%
  select(iso, country) %>%
  unique()

countries_coord <- countries_coord %>%
  left_join(merge_pais, by=c('id'='iso'))




resultados <- pubmed_data %>%
  select(Date, PMID, chloroquine, hydroxychloroquine, remdesivir, ritonavir, lopinavir, favipiravir, interferon, azithromycin, tocilizumab, antibodies, vaccine, antivirals) %>%
  gather(key = "variable", value = "valor", -c(Date, PMID)) %>%
  filter(valor ==1)  %>%
  unique()  %>%
  arrange(variable, Date)   %>%
  select(variable, Date, PMID) %>%
  as_tibble()



casos <- pubmed_data %>%
  filter(!is.na(iso))  %>%
  filter(iso!='')  %>%
  arrange(country, iso, Date) %>%
  group_by(country, iso, Date) %>%
  summarize(dia=n_distinct(PMID)) %>% 
  mutate(total = cumsum(dia))


countries_coord2 <- casos %>%
  left_join(countries, by=c('iso'='country'))




###############################################
###############################################
###############################################
###############################################
###############################################
####################SERVER#####################
###############################################
###############################################
###############################################
###############################################
###############################################


server <- function(input, output, session) {
  
  pais <- reactive({
    a <- pubmed_data %>% filter(country %in% input$country)
    a <- data.frame(a)
    return(a)
  })    
  
  fecha <- reactive({
    a <- pubmed_data  %>% filter(Date <= input$Date)
    a <- data.frame(a)
    return(a)
    })    
  
  seleccion <- reactive({
  a <- resultados
  a <- a %>% filter(variable %in% input$variable)
  a <- data.frame(a)
  return(a)
  })    
  
  

    
  #################################PRIMER SOLAPA###########################
  output$plot1a <- renderGirafe({ggplot1a <- pubmed_data %>%
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

      girafe(ggobj = ggplot1a, 
             options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  output$plot1b <- renderGirafe({ggplot1b <-pubmed_data %>%
      group_by(Date)  %>%
      summarize(dia=n_distinct(PMID))  %>% 
      ggplot(aes(x=Date, y=dia)) + 
      ylab("Papers x Dia") +
      xlab("Date") +
      geom_line(size=2, alpha=0.6) +
      geom_smooth(method = "loess", size=2, alpha=0.3)   +  
      geom_point_interactive(aes(x=Date, y=dia, size=3, alpha=0.8, tooltip = paste0("Dia: ",Date,"\n Cantidad:",dia))) +
      #geom_point(size=3, alpha=0.8) +
      theme_minimal() + 
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5)) +
      ggtitle("Publicaciones cientificas por dia en PubMed")
  
  
  girafe(ggobj = ggplot1b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  
  
  })
  
  output$table1 <- renderDT(pubmed_data %>%
                              group_by(Date)  %>%
                              summarize(Dia=n_distinct(PMID)) %>% 
                              mutate(Acumulado = cumsum(Dia)),
                            extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar", filename= 'publicaciones')),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  
  
  ######################SEGUNDA SOLAPA###########################    
  
  output$plot2a <- renderGirafe({
    ggplot2a <- pubmed_data %>%
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
    
    
    girafe(ggobj = ggplot2a, 
           options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  })
  
  output$plot2b <- renderGirafe({
    ggplot2b <- 
  pubmed_data %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
    group_by(country, iso) %>%
    summarize(cantidad=n_distinct(PMID)) %>%
    arrange(desc(cantidad)) %>%
    #head(15) %>%
  ggplot(aes(area = cantidad, fill = country, label = country, tooltip = paste0(country,": ", cantidad ))) +
    geom_treemap() + 
    geom_treemap_text(place = "centre", reflow = TRUE, min.size = 2) +
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position = "none") +
      ggtitle("Publicaciones cientificas acumuladas en PubMed por pais") 
  
  
    girafe(ggobj = ggplot2b, 
           options = list(opts_selection(type = "single", only_shiny = FALSE)) )
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

  
  
  
  output$plot3a <- renderGirafe({ggplot3a <- 
    pais() %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
      arrange(country, Date) %>%
      group_by(country, Date) %>%
      summarize(dia=n_distinct(PMID)) %>% 
      mutate(total = cumsum(dia))  %>% 
      ggplot(aes(x=Date, y=total, color=country)) + 
      ylab("Comparar paises") +
      xlab("Date") +
      geom_line_interactive(size = 3, alpha=1)  +
      geom_point_interactive(aes(x=Date, y=total, size=4, alpha=0.8, tooltip = paste0("Pais: ",country, "\n Dia: ",Date,"\n Cantidad:",total))) +
    theme_minimal() + 
      scale_color_manual(values = jColors) +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
  ggtitle("Comparacion de Publicaciones cientificas  \n en PubMed entre paises") +
    guides(size=FALSE, alpha=FALSE) 
  
  
  girafe(ggobj = ggplot3a, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  
  
  output$plot3b <- renderGirafe({ggplot3b <- 
    pais() %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
    arrange(country, Date) %>%
    group_by(country, Date) %>%
    summarize(dia=n_distinct(PMID)) %>% 
    mutate(total = cumsum(dia))  %>% 
    ggplot(aes(x=Date, y=total, color=country)) + 
    ylab("Comparar paises") +
    xlab("Date") +
    geom_line_interactive(size = 3, alpha=1)  +
    geom_point_interactive(aes(x=Date, y=total, size=4, alpha=0.8, tooltip = paste0("Pais: ",country, "\n Dia: ",Date,"\n Cantidad:",total))) +
    theme_minimal() + 
    scale_color_manual(values = jColors) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
    ggtitle("Comparacion de Publicaciones cientificas  \n en PubMed entre paises (LOG2)") +
    guides(size=FALSE, alpha=FALSE)  + 
    scale_y_continuous(trans='log2')
  
  
  girafe(ggobj = ggplot3b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  

  
#  leaflet() %>% addTiles()  %>%
  #    addMinicharts(countries_coord2$longitude, countries_coord2$latitude,chartdata = countries_coord2$total, labelText = countries_coord2$iso, showLabels = TRUE, width = 100, height = 100, layerId = countries_coord2$country, time = countries_coord2$Date) 
  
  
  
  
  
  output$table3 <- renderDT(pais() %>%
                              filter(!is.na(iso))  %>%
                              filter(iso!='')  %>%
                              arrange(country, Date) %>%
                              group_by(country, Date) %>%
                              summarize(dia=n_distinct(PMID)) %>% 
                              mutate(total = cumsum(dia)) %>%
                              arrange(Date),extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           #buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  

  ######################CUARTA SOLAPA###########################      

  
  output$map4 <- renderLeaflet({ 
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Espere por favor',
                 detail = 'La carga Inicial de datos puede demorar algunos segundos')

    
    leaflet() %>% addTiles()  %>%
      addCircles(data = countries_coord,lng = ~longitude, lat = ~latitude,  weight = 1, radius = ~sqrt(count) * 40000, color='red',  label=~paste0(country,': ',count)) %>%
      #      addMinicharts(countries_coord$longitude, countries_coord$latitude,chartdata = countries_coord$count, labelText = countries_coord$id, showLabels = TRUE, width = 100, height = 100, layerId = unique(countries_coord$country)) %>%
      addFlows(lng0 = edges_for_plot_ud$x, lat0 = edges_for_plot_ud$y, lng1 = edges_for_plot_ud$xend, lat1 = edges_for_plot_ud$yend, time= edges_for_plot_ud$dia, dir = 0, color='purple', flow=edges_for_plot_ud$weight,  minThickness = 0.1, maxThickness = 15, opacity=0.7) 
  })
  
  
  
  output$table4 <- renderDT(
    
    edges_for_plot <- edges_for_plot_ud  %>% 
      left_join (listado_iso_country, by=c('source'='iso'))  %>% 
      left_join (listado_iso_country, by=c('target'='iso')) %>%
      select(source=country.x, target=country.y, weight) %>% 
                              arrange(desc(weight)) %>%
      filter(!is.na(source) & !is.na(target)),
                            extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar", filename= 'publicaciones')),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  
  
  
  
  
  ######################QUINTA SOLAPA###################
  
  output$plot5a <- renderGirafe({ggplot5a <- 
    seleccion()  %>%
    group_by(variable, Date)  %>% 
    summarize(dia=n_distinct(PMID)) %>% 
    mutate(total = cumsum(dia))  %>% 
    ggplot(aes(x=Date, y=total, color=variable)) + 
    ylab("total de papers") +
    xlab("Date") +
    geom_line_interactive(size = 3, alpha=1)  +
    geom_point_interactive(aes(x=Date, y=total, size=4, alpha=0.8, tooltip = paste0(variable, "\n Dia: ",Date,"\n Cantidad:",total))) +
    theme_minimal() + 
        scale_color_manual(values = jColors2) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
    ggtitle("Comparacion de Conceptos en PubMed") +
    guides(size=FALSE, alpha=FALSE) 

  
  
  
  girafe(ggobj = ggplot5a, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  
  
  })
  
  
  output$plot5b <- renderGirafe({ggplot5b <- 
    seleccion() %>%
    group_by(variable, Date)  %>% 
    summarize(dia=n_distinct(PMID)) %>% 
    mutate(total = cumsum(dia))  %>% 
    ggplot(aes(x=Date, y=total, color=variable)) + 
    ylab("total de papers") +
    xlab("Date") +
    geom_line_interactive(size = 3, alpha=1)  +
    geom_point_interactive(aes(x=Date, y=total, size=4, alpha=0.8, tooltip = paste0(variable, "\n Dia: ",Date,"\n Cantidad:",total))) +
  theme_minimal() + 
    scale_color_manual(values = jColors2) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
    ggtitle("Comparacion de Conceptos en PubMed (LOG2)") +
    guides(size=FALSE, alpha=FALSE)  + 
    scale_y_continuous(trans='log2')
  
    
  
  
  girafe(ggobj = ggplot5b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  
  
  })
  
  
  
  
  
  

  
  output$table5a <- renderDT(seleccion() %>%
                               group_by(variable, Date)  %>% 
                               summarize(dia=n_distinct(PMID)) %>% 
                               mutate(total = cumsum(dia))  %>% 
                              arrange(desc(total)), extensions = 'Buttons', options = list(pageLength = 10, dom = 'Bfrtip',
                 buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar", filename= 'publicaciones')),
                 exportOptions = list(modifiers = list(page = "all")
                 )), server = FALSE)
  
  
  
  
  output$network6a <- renderVisNetwork({
    
    g <- keyword_cors %>%
      graph_from_data_frame(directed=FALSE, vertices=keyword_cant)  
    #g<- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    
    rw <- cluster_walktrap(g, weights = E(g)$weight, steps = 3,merges = TRUE, modularity = TRUE, membership = TRUE)
    cluster = rw$membership
    g <- set_vertex_attr(g, name="group", value = cluster)
    
    E(g)$width <- E(g)$weight*30
    V(g)$size <- sqrt(V(g)$size)*3
    
    
    visIgraph(g, type="full", layout = "layout_nicely", physics = TRUE)  %>%  
      visLegend(enabled = FALSE) %>% 
      visEdges(color = "black") %>% 
      visNodes(shape = "dot", scaling = list(min = 1, max = 40), shadow = list(enabled = TRUE, size = 20))  %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 3, hover = T),nodesIdSelection = T) %>%
      visLayout(randomSeed = 12) # to have always the same network  
    
  
  })
  
  
  
  
  
  

  
  
  output$logo <-
    renderText({c('<img src="',"./oeiocts.jpg",'">')})
  
  
  
  
  
  
  
  
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(title = "Publicaciones Cientificas relacionadas con Covid-19",
  titlePanel( fluidRow(column(width = 6, h2("Publicaciones Cientificas relacionadas con Covid-19")))),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Publicaciones Total",
                         fluidRow(column(6, girafeOutput("plot1a")),column(6, girafeOutput("plot1b"))),
                         fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                tabPanel("Comparativo paises",
                         fluidRow(column(12, pickerInput(inputId = "country", label = "Seleccione los paises a comparar", 
                                                        choices = listado_paises, selected = c("China","United States"), 
                                                        options = list('actions-box' = TRUE, size = 8,
                                                        'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), 
                                                        multiple = TRUE))), 
                         fluidRow(column(6, girafeOutput("plot3a")),column(6, girafeOutput("plot3b"))),
                         #fluidRow(column(12, girafeOutput("plot3"))),
                         fluidRow(column(12, dataTableOutput(outputId = "table3")))),
                tabPanel("Grafo de Colaboracion", 
                        #fluidRow(column(12,  sliderInput(inputId = "Date", "Evolucion Colaboracion:", min=first(listado_dias$Date), max=last(listado_dias$Date), value=max(listado_dias$Date), timeFormat="%Y-%m-%d", animate = TRUE))), 
                        fluidRow(column(12, leafletOutput(outputId = "map4"))),
                        fluidRow(column(6, dataTableOutput("table2")),column(6, dataTableOutput("table4"))),
                        ),
                tabPanel("Avances Cientificos", 
                         fluidRow(column(12, pickerInput(inputId = "variable", label = "Seleccione los conceptos a comparar", 
                                                        choices = variable_selector, selected = c("vaccine", "antivirals", "antibodies"), 
                                                        options = list('actions-box' = TRUE, size = 8,
                                                                       'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), 
                                                        multiple = TRUE))), 
                         fluidRow(column(6, girafeOutput("plot5a")),column(6, girafeOutput("plot5b"))),
                         fluidRow(column(12, dataTableOutput("table5a")))
                ),
                tabPanel("Mapa Conceptual", visNetworkOutput(outputId = "network6a"))
          )
)
)

shinyApp(ui, server)


