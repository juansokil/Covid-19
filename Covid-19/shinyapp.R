
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(DT)  
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
#install.packages("ggiraphExtra")
#install.packages("leaflet.minicharts")
library(leaflet.minicharts)

#rsconnect::setAccountInfo(name='juanpablosokil', 
#                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
#                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')



rsconnect::setAccountInfo(name='observatorio-cts',
                          token='EFB80756D7A13A771E3F6419EA165929',
                          secret='DfBCakMJl3pZLkwvNS5aGELaMZ95GK5A72rGazAh')


#setwd('./github/covid-19/scripts')
###Levanto los datos viejos###


#pubmed_data <- read.table("../bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1, colClasses=c(Title="character", Abstract="character", country="character", afil="character", Date="Date"))

#pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", Abstract="character", country="character", afil="character", Date="Date"))
pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(country="character",  Date="Date"))
edges_for_plot <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/edges_for_plot.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(dia="Date"))

edges_for_plot_ud <- edges_for_plot %>%
  filter(dia == max(dia))

#######################ARMA NODOS #######################
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

listado_dias <- pubmed_data %>%
  select(Date)  %>%
  unique() %>%
  arrange(Date)

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
vector_colores <- as.character(sample(color, nrow(listado_paises)))

jColors <- vector_colores
names(jColors) <- listado_paises$country

###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)
countries_coord <- nodes %>%
  left_join(countries, by=c('id'='country'))

#maptheme <- theme(panel.grid = element_blank()) +
#  theme(axis.text = element_blank()) +
#  theme(axis.ticks = element_blank()) +
#  theme(axis.title = element_blank()) +
#  theme(legend.position = "bottom") +
#  theme(panel.grid = element_blank()) +
#  theme(panel.background = element_rect(fill = "#596673")) +
#  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
#country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
#                               data = map_data('world'),
#                               fill = "#CECECE", color = "#515151",
#                               size = 0.15)




#https://datascience.blog.wzb.eu/2018/05/31/three-ways-of-visualizing-a-graph-on-a-map/

###################SERVER#####################
server <- function(input, output, session) {
  
  pais <- reactive({
    a <- pubmed_data  %>% filter(country %in% input$country)
    a <- data.frame(a)
    return(a)
  })    
  
  
#  fecha <- reactive({
#    a <- edges_for_plot  %>% filter(dia == input$dia)
#    a <- data.frame(a)
#    return(a)
#  })    
  
  
    fecha <- reactive({
      a <- pubmed_data  %>% filter(Date <= input$Date)
      a <- data.frame(a)
      return(a)
    })    
  
  ######################PRIMER SOLAPA###########################
  output$plot1 <- renderGirafe({ggplot1 <- pubmed_data %>%
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
  
  output$plot3 <- renderGirafe({ggplot2 <-pubmed_data %>%
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
  
  
  girafe(ggobj = ggplot2, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  
  
  })
  
  output$table1 <- renderDT(publicaciones <- pubmed_data %>%
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
  
  output$plot5 <- renderGirafe({
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

  
  
  
  output$plot2 <- renderGirafe({ggplot3 <- 
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
      #geom_line(size=3, alpha=1) +
      geom_line_interactive(size = 3, alpha=1)  +
      geom_point_interactive(aes(x=Date, y=total, size=4, alpha=0.8, tooltip = paste0("Pais: ",country, "\n Dia: ",Date,"\n Cantidad:",total))) +
      #geom_point(size=4, alpha=1) +
      #geom_smooth(method = "loess", size=2, alpha=0.3)  +  
    theme_minimal() + 
      scale_color_manual(values = jColors) +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
  ggtitle("Comparacion de Publicaciones cientificas  \n en PubMed entre paises") +
    guides(size=FALSE, alpha=FALSE)
  
  
  girafe(ggobj = ggplot3, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  
  
  
  
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
  
  
  #output$network <- renderVisNetwork({
  #  E(g)$width <- E(g)$weight/8
  #  
  #  visIgraph(g, type="full", layout = 'layout.norm', layoutMatrix = as.matrix(cbind(countries_coord$longitude, countries_coord$latitude*-1)))  %>%  
  #    visLegend() %>% 
  #    #visEdges( width = weight) %>% 
  #    visNodes(scaling = list(min = 10, max = 100)) %>% 
  #    visOptions(highlightNearest = T,nodesIdSelection = T)
  #})
  
  
  
  #output$map <- renderPlot({ 
    
    #######################ARMA NODOS #######################
  #  nodos <- fecha() %>%
  #    select(iso, PMID) %>%
  #    unique()  %>%
  #    filter(!is.na(iso)) %>%
  #    filter(iso!='')  %>%
  #    group_by(iso) %>%
  #    summarize(totales=n_distinct(PMID))
    
  
    #######################ARMA VERTICES #######################
    
  #  byHand <- fecha() %>%
  #    select(PMID, iso) %>%
  #    unique() %>%
  #    filter(!is.na(iso))  %>%
  #    filter(iso!='')  %>%
  #    group_by(PMID) %>% mutate(paises = paste(iso, collapse = ","))
    
    #### identifico el primero de los autores###
  #  byHand$primer_pais = as.character(lapply(strsplit(as.character(byHand$paises), split=","), "[", 1))
  #  arreglado <- byHand %>% mutate(paises = strsplit(as.character(paises), ",")) %>% unnest(paises)
  #  aristas_previo <- arreglado[c(3:4)]
  #  colnames(aristas_previo) <- c("source","target")
  #  aristas <- aristas_previo %>% group_by(source, target) %>% summarize(count=n())
    
  #  nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = nodos$totales, count=nodos$totales)     # size 
  #  edges <- data.frame(source = aristas$source, target = aristas$target, weight = aristas$count)
  #  edges <- edges %>%
  #    filter(as.character(source) != as.character(target))
    
  #countries_coord <- nodes %>%
  #  left_join(countries, by=c('id'='country'))

  #  edges_for_plot  <- edges %>%
  #    inner_join(countries_coord %>% select(id, longitude, latitude), by = c('source' = 'id')) %>%
  #    rename(x = longitude, y = latitude) %>%
  #    inner_join(countries_coord %>% select(id, longitude, latitude), by = c('target' = 'id')) %>%
  #    rename(xend = longitude, yend = latitude) 
    #g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
    #g<- simplify(g, remove.multiple = TRUE)
    
    #E(g)$width <- E(g)$weight/8
    
  #  ggplot(countries_coord) + country_shapes +
  #    geom_curve(aes(x = x, y = y, xend = xend, yend = yend, size=weight/12), data = edges_for_plot, alpha = 0.5) +
  #    #geom_segment(aes(x = x, y = y, xend = xend, yend = yend, size=weight/12, color="blue") , data = edges_for_plot ,alpha = 0.5) +
  #    geom_point(aes(x = longitude, y = latitude, size=count),           # draw nodes
  #               shape = 21, fill = 'white',  
  #               color = 'black', stroke = 0.5) +
  #    scale_size_continuous(guide = FALSE, range = c(2, 20)) +    # scale for node size
  #    geom_text(aes(x = longitude, y = latitude, label = id),             # draw text labels
  #              hjust = 0, nudge_x = 1, nudge_y = 4,
  #              size = 3, color = "red", fontface = "bold")
  #  
  #})
  

  output$table4 <- renderDT(edges_for_plot_ud %>%
                              select(dia, source, target, weight),extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copiar datos"),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  


  ######################CUARTA SOLAPA###########################      
###VER####
#  output$map2 <- renderLeaflet({ 
#    leaflet() %>% addTiles()  %>%
    #addFlows(lng0 = edges_for_plot$x, lat0 = edges_for_plot$y, lng1 = edges_for_plot$xend, lat1 = edges_for_plot$yend, time= edges_for_plot$dia, dir = 0, color='purple', flow=edges_for_plot$weight,  minThickness = 0.05, maxThickness = 10, opacity=0.9) 
   # %>%
#    addCircles(data = countries_coord,lng = ~longitude, lat = ~latitude,  weight = 1, radius = ~sqrt(count) * 30000, color='red' ) 
#  })

  
  output$map2 <- renderLeaflet({ 
    leaflet() %>% addTiles()  %>%
      addCircles(data = countries_coord,lng = ~longitude, lat = ~latitude,  weight = 1, radius = ~sqrt(count) * 30000, color='red',  label=~paste0(id,'\n',count)) %>%
      addFlows(lng0 = edges_for_plot_ud$x, lat0 = edges_for_plot_ud$y, lng1 = edges_for_plot_ud$xend, lat1 = edges_for_plot_ud$yend, time= edges_for_plot_ud$dia, dir = 0, color='purple', flow=edges_for_plot_ud$weight,  minThickness = 0.05, maxThickness = 10, opacity=0.9) 
  })
  

  
  #https://cran.r-project.org/web/packages/leaflet.minicharts/vignettes/introduction.html
  #https://mran.microsoft.com/snapshot/2017-05-28/web/packages/leaflet.minicharts/leaflet.minicharts.pdf
  #https://rstudio.github.io/leaflet/shiny.html
  #https://www.r-bloggers.com/covid-19-shiny-plotly-dashboard/
  
  
    
  #leafletProxy(mapId = "outmap") %>%
  #  clearGroup(curr_date()) %>%
  #  addCircles(data = data_for_display,
  #             lng = ~Long, lat = ~Lat,
  #             radius = ~active_scaled,
  #             popup = ~text,
  #             fillColor = ~color, stroke = FALSE, fillOpacity = 0.5,
  #             group = stringr::str_match(date_to_choose, "\\d{4}\\-\\d{2}\\-\\d{2}")[1,1]
  #  )

  
  
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
ui <- fluidPage(title = "Publicaciones Cientificas relacionadas con Covid-19",
  #titlePanel( fluidRow(column(width = 6, h2("Publicaciones Cientificas relacionadas con Covid-19")), 
  #                     column(width = 6, tags$a(
  #                       href="https://observatoriocts.oei.org.ar",  
  #                       tags$img(src="https://raw.githubusercontent.com/juansokil/Covid-19/master/Covid-19/oeiocts.jpeg", 
  #                                height='100',width='100', align="left")
                       #)))),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Publicaciones Total",
                         fluidRow(column(6, girafeOutput("plot1")),
                                  column(6, girafeOutput("plot3"))),
                         fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                tabPanel("Publicaciones por Paises",
                         fluidRow(column(12, girafeOutput("plot5")),
                                  fluidRow(column(12, dataTableOutput(outputId = "table2"))))),
                tabPanel("Comparativo paises",
                         fluidRow(column(6, pickerInput(inputId = "country", label = "Seleccione los paises a comparar", choices = listado_paises, selected = c("China","United States"), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE))), 
                         fluidRow(column(12, girafeOutput("plot2")),
                                  fluidRow(column(12, dataTableOutput(outputId = "table3"))))),
                #tabPanel("Grafo de Colaboracion", 
                #         fluidRow(column(12,  sliderInput(inputId = "Date", "Evolucion Colaboracion:", min=first(listado_dias$Date), max=last(listado_dias$Date), value=max(listado_dias$Date), timeFormat="%Y-%m-%d", animate = TRUE))), 
                #         fluidRow(column(12, plotOutput(outputId = "map", height = "500px")))),
                tabPanel("Grafo de Colaboracion2", 
                        #fluidRow(column(12,  sliderInput(inputId = "Date", "Evolucion Colaboracion:", min=first(listado_dias$Date), max=last(listado_dias$Date), value=max(listado_dias$Date), timeFormat="%Y-%m-%d", animate = TRUE))), 
                        fluidRow(column(12, leafletOutput(outputId = "map2"))))
                        #fluidRow(column(12, dataTableOutput(outputId = "table4")))
    )))



shinyApp(ui, server)


