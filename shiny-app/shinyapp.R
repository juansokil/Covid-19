
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
#install.packages("geojsonio")
#library(geojsonio)






pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", country="character",  Inst="character", Date="Date"))



pubmed_data$iso <- str_to_lower(pubmed_data$iso)

listado_articulos <- pubmed_data %>%
  select(PMID, Date, country, Title) %>%
  unique()

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
edges_for_plot$source <- str_to_lower(edges_for_plot$source)
edges_for_plot$target <- str_to_lower(edges_for_plot$target)

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

listado_paises2 <- pubmed_data %>%
  mutate(pais = country)  %>%
  filter(!is.na(iso))  %>%
  filter(iso!='')  %>%
  group_by(pais, iso) %>%
  summarize(cantidad=n_distinct(PMID)) %>%
  arrange(desc(cantidad))  %>%
  ungroup() %>%
  select(pais)




listado_iso_country <- pubmed_data %>%
  select(iso, country) %>%
  filter(!is.na(iso))  %>%
  unique()


#variable_selector <- pubmed_data %>%
#  select(PMID, azithromycin,  favipiravir, hydroxychloroquine, interferon, lopinavir, remdesivir, ritonavir,   tocilizumab,  vaccine, antibodies) %>%
#    gather(key = "variable", value = "valor", -c(PMID)) %>%
#    select(variable)  %>%
#    unique()


color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
vector_colores <- as.character(sample(color, nrow(listado_paises)))
#vector_colores2 <- as.character(sample(color, nrow(variable_selector)))


jColors <- vector_colores
#jColors2 <- vector_colores2
names(jColors) <- listado_paises$country
#names(jColors2) <- variable_selector$variable





###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

countries$country <- str_to_lower(countries$country)

countries_coord <- nodes %>%
  left_join(countries, by=c('id'='country'))

merge_pais <- pubmed_data %>%
  select(iso, country) %>%
  unique()

countries_coord <- countries_coord %>%
  left_join(merge_pais, by=c('id'='iso'))


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
  
  
  pais2 <- reactive({
    a <- pubmed_data %>% 
      mutate(pais = country) %>% 
      filter(pais %in% input$pais)
    a <- data.frame(a)
    return(a)
  })    
  
  fecha <- reactive({
    a <- pubmed_data  %>% filter(Date <= input$Date)
    a <- data.frame(a)
    return(a)
  })    
  
  
  
  
  #################################PRIMER SOLAPA###########################
  output$plot1a <- renderGirafe({ggplot1a <- pubmed_data %>%
    group_by(Date)  %>%
    summarize(dia=n_distinct(PMID))  %>% 
    mutate(total = cumsum(dia))  %>% 
    ggplot(aes(x=Date, y=total)) + 
    ylab("Papers cumulative") +
    xlab("Date") +
    geom_line_interactive(size=2, alpha=1) +
    geom_smooth(method = "loess", size=2, alpha=0.3, color='black')   +  
    geom_point_interactive(aes(x=Date, y=total, size=1, alpha=0.1, tooltip = paste0("Date: ",Date,"\n Acumulado: ",total))) +
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Publicaciones Acumuladas en PubMed") 
  
  girafe(ggobj = ggplot1a, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  
  
  output$plot1b <- renderGirafe({ggplot1b <-pubmed_data %>%
    group_by(Date)  %>%
    summarize(dia=n_distinct(PMID))  %>% 
    ggplot(aes(x=Date, y=dia)) + 
    ylab("Papers per Day") +
    xlab("Date") +
    geom_line(size=1, alpha=0.3) +
    geom_point_interactive(aes(x=Date, y=dia, size=2, alpha=0.6, tooltip = paste0("Date: ",Date,"\n Count: ",dia))) +
    geom_smooth(method = "loess", size=2, alpha=0.6, se=FALSE, color='black')   +    
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), legend.position="none", plot.title = element_text(hjust = 0.5)) +
    ggtitle("Publicaciones en PubMed por dia")
  
  
  girafe(ggobj = ggplot1b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  
  })
  
  output$table1 <- renderDT(pubmed_data %>%
                              group_by(Date)  %>%
                              summarize(Day=n_distinct(PMID)) %>% 
                              mutate(Cumulative = cumsum(Day)),
                            extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar", filename= 'papers')),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  
  
  ######################SEGUNDA SOLAPA###########################    
  
  
  output$plot2a <- renderGirafe({ggplot2a <- 
    pais() %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
    arrange(country, Date) %>%
    group_by(country, Date) %>%
    summarize(Day=n_distinct(PMID)) %>% 
    mutate(Cumulative = cumsum(Day))  %>% 
    ggplot(aes(x=Date, y=Cumulative, color=country)) + 
    ylab("Comparar paises") +
    xlab("Date") +
    geom_line_interactive(size = 2, alpha=1)  +
    geom_smooth(aes(group=country), method = "loess", size=2, alpha=0.3)   +    
    geom_point_interactive(aes(x=Date, y=Cumulative, size=1, alpha=0.2, tooltip = paste0("Country: ",country, "\n Date: ",Date,"\n Cumulative: ",Cumulative))) +
    theme_minimal() + 
    #scale_color_manual(values = jColors) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
    ggtitle("Cumulative papers in PubMed by country") +
    guides(size=FALSE, alpha=FALSE) 
  
  
  girafe(ggobj = ggplot2a, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  
  
  
  output$plot2b <- renderGirafe({ggplot2b <- 
    pais() %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
    arrange(country, Date) %>%
    group_by(country, Date) %>%
    summarize(Day=n_distinct(PMID)) %>% 
    mutate(total = cumsum(Day))  %>% 
    ggplot(aes(x=Date, y=Day, color=country)) + 
    ylab("Comparar paises") +
    xlab("Date") +
    geom_line(size=1, alpha=0.3) +
    geom_point_interactive(aes(x=Date, y=Day, size=2, alpha=0.6, tooltip = paste0("Country: ",country, "\n Date: ",Date,"\n Count: ",Day))) +
    geom_smooth(method = "loess", size=2, alpha=0.6, se = FALSE,  aes(fill = country))   +  
    theme_minimal() + 
    #scale_color_manual(values = jColors) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.title = element_blank()) +
    ggtitle("Papers in PubMed per day by country") +
    guides(size=FALSE, alpha=FALSE) 
  
  
  
  
  
  girafe(ggobj = ggplot2b, 
         options = list(opts_selection(type = "single", only_shiny = FALSE)) )
  
  })
  
  
  output$table2 <- renderDT(pais() %>%
                              filter(!is.na(iso))  %>%
                              filter(iso!='')  %>%
                              arrange(country, Date) %>%
                              group_by(country, Date) %>%
                              summarize(Day=n_distinct(PMID)) %>% 
                              mutate(Cumulative = cumsum(Day)) %>%
                              arrange(Date),extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE)
  
  
  
  
  
  
  
  ######################TERCERA SOLAPA###########################      
  
  
  
  output$map3 <- renderLeaflet({ 
    progress <- Progress$new(session, min=1, max=10000)
    on.exit(progress$close())
    
    progress$set(message = 'Loading Data')
    
    
    WorldCountry <-geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
    
    
    
    
    
    #  bins <- c(0,1,2,3,4,5,6,7,8,9,10,Inf)
    #  pal <- colorBin("YlOrRd", domain = countries_coord$count, bins = bins)
    leaflet() %>% addTiles()  %>%
      #    leaflet(WorldCountry) %>% addTiles()  %>% addPolygons(fillColor = ~pal(countries_coord$count),
      #                                                          weight = 2,
      #                                                      opacity = 0.8,
      #                                                      color = 'white',
      #                                                      dashArray = '1',
      #                                                      fillOpacity = 0.4,
      #                                                      highlight = highlightOptions(
      #                                                        weight = 2,
      #                                                        color = "#666",
      #                                                        dashArray = "",
      #                                                        fillOpacity = 0.7,
    #                                                        bringToFront = TRUE)
    #                                                      , label = countries_coord$country,
    #labelOptions = labelOptions(
    #  style = list("font-weight" = "normal", padding = "3px 8px"),
    #  textsize = "15px",
    #  direction = "auto")) %>%
    addCircles(data = countries_coord,lng = ~longitude, lat = ~latitude,  weight = 1, radius = ~sqrt(count) * 12000, color='blue',  label=~paste0(country,': ',count)) %>%
      #     addMinicharts(countries_coord$longitude, countries_coord$latitude,chartdata = countries_coord$count, labelText = countries_coord$id, showLabels = TRUE, width = 100, height = 100, layerId = unique(countries_coord$country)) %>%
      addFlows(lng0 = edges_for_plot_ud$x, lat0 = edges_for_plot_ud$y, lng1 = edges_for_plot_ud$xend, lat1 = edges_for_plot_ud$yend, time= edges_for_plot_ud$dia, dir = 0, color='purple', flow=edges_for_plot_ud$weight,  minThickness = 0.4, maxThickness = 4, opacity=0.3) 
    
  })
  
  
  
  
  output$table6 <- renderDT(pais2() %>%
                              filter(!is.na(iso))  %>%
                              filter(iso!='') %>%
                              select(Date, Title, PMID, country)  %>%
                              #mutate(Link=paste0('https://pubmed.ncbi.nlm.nih.gov/',PMID,'/')) %>%
                              mutate(Link=paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",PMID,"/' target='_blank' >Ver Articulo</a>")) %>%
                              select(Date, PMID,country, Title, Link)   %>%
                              unique() %>%
                              arrange(desc(Date))  ,extensions = 'Buttons',
                            options = list(pageLength = 10,
                                           dom = 'Bfrtip',
                                           buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                           exportOptions = list(modifiers = list(page = "all")
                                           )
                            ), server = FALSE, escape = FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$table3a <- renderDT(pubmed_data %>%
                               filter(!is.na(iso))  %>%
                               filter(iso!='')  %>%
                               group_by(country, iso) %>%
                               summarize(Count=n_distinct(PMID)) %>%
                               arrange(desc(Count)),extensions = 'Buttons',
                             options = list(pageLength = 10,
                                            dom = 'Bfrtip',
                                            buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                                            exportOptions = list(modifiers = list(page = "all")
                                            )
                             ), server = FALSE)
  
  
  
  output$table3b <- renderDT(
    
    edges_for_plot <- edges_for_plot_ud  %>% 
      left_join (listado_iso_country, by=c('source'='iso'))  %>% 
      left_join (listado_iso_country, by=c('target'='iso')) %>%
      select(Source=country.x, Target=country.y, Weight=weight) %>% 
      arrange(desc(Weight)) %>%
      filter(!is.na(Source) & !is.na(Target)),
    extensions = 'Buttons',
    options = list(pageLength = 10,
                   dom = 'Bfrtip',
                   buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                   exportOptions = list(modifiers = list(page = "all")
                   )
    ), server = FALSE)
  
  
  
  ######################CUARTASOLAPA###################
  
  output$network4 <- renderVisNetwork({
    
    g <- keyword_cors %>%
      graph_from_data_frame(directed=FALSE, vertices=keyword_cant)  
    #g<- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
    
    rw <- cluster_walktrap(g, weights = E(g)$weight, steps = 3,merges = TRUE, modularity = TRUE, membership = TRUE)
    cluster = rw$membership
    g <- set_vertex_attr(g, name="group", value = cluster)
    
    E(g)$width <- E(g)$weight/1000
    V(g)$size <- sqrt(V(g)$size)*2
    
    
    #visIgraph(g, type="full", layout = "layout_nicely", physics = TRUE)  %>%  
    #  visLegend(enabled = FALSE) %>% 
    #  visEdges(color = "black") %>% 
    #  visNodes(shape = "dot", scaling = list(min = 1, max = 40), shadow = list(enabled = TRUE, size = 20))  %>% 
    #  visOptions(highlightNearest = list(enabled = T, degree = 3, hover = T),nodesIdSelection = T) %>%
    #  visLayout(randomSeed = 12) # to have always the same network  
    
    
    
    
    
    output$table4a <- renderDT(
      keyword_cors <- keyword_cors %>%
        group_by(weight) %>%
        summarize(item1=first(item1),item2=first(item2))  %>%
        mutate(weight=round(weight, 3)) %>%
        select(word1=item1, word2=item2, pmi=weight) %>%
        arrange(desc(pmi)),
      extensions = 'Buttons',
      options = list(pageLength = 10,
                     dom = 'Bfrtip',
                     buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                     exportOptions = list(modifiers = list(page = "all")
                     )
      ), server = FALSE)
    
    
    
    output$table4b <- renderDT(
      keyword_cant <- keyword_cant %>%
        select(word=palabra, count=size) %>%
        arrange(desc(count)),
      extensions = 'Buttons',
      options = list(pageLength = 10,
                     dom = 'Bfrtip',
                     buttons = list("copy", list(extend = "collection", buttons = c("csv", "excel"), text = "Descargar")),
                     exportOptions = list(modifiers = list(page = "all")
                     )
      ), server = FALSE)
    
    
    
    
    ##############################################################
    ##########################GRAPH PRUNE#########################
    ##############################################################
    
    
    ## identify communities##
    lou <- cluster_louvain(g)
    
    ############5 comunidades mas grandes###########
    size <- as.data.frame(sizes(lou))
    
    importantes <- size %>%
      arrange(desc(Freq)) %>%
      head(5) %>%
      select(Community.sizes)
    
    importantes <- as.vector(unlist(importantes))
    
    g2 <- induced.subgraph(g, which(membership(lou) %in% importantes))
    
    
    ####Minimum Spanning Tree####
    min_spanning_tree <- mst(g2, weights = E(g2)$weight)
    
    
    ####Graph visualization####
    visIgraph(min_spanning_tree, type="full", layout = "layout_nicely", physics = TRUE)  %>%  
      #visIgraph(g, type="full", layout = "layout_nicely", physics = TRUE)  %>%  
      visLegend(enabled = FALSE) %>% 
      visEdges(color = "black") %>% 
      visNodes(shape = "dot", scaling = list(min = 1, max = 10), shadow = list(enabled = TRUE, size = 20))  %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 3, hover = T),nodesIdSelection = T)  %>%
      visLayout(randomSeed = 12) # to have always the same network  
    
    
    
    
  })
  
  
  
  
  
  
  
  output$logo <-
    renderText({c('<img src="',"./oeiocts.jpg",'">')})
  
  
  
  
  
  
  
  
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Publicaciones sobre Covid-19"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Publicaciones",
                         h4('Evolucion Acumulada y diaria de Publicaciones'),
                         fluidRow(column(6, girafeOutput("plot1a")),column(6, girafeOutput("plot1b"))),
                         fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                tabPanel("Comparacion entre paises",
                         h4('Evolucion Acumulada y diaria de Publicaciones a nivel pais'),
                         fluidRow(column(12, selectizeInput('country', label = "Seleccione paises a comparar", choices = listado_paises, selected = c("United States","China","Italy"), multiple = TRUE))                ), 
                         fluidRow(column(6, girafeOutput("plot2a")),column(6, girafeOutput("plot2b"))),
                         fluidRow(column(12, dataTableOutput(outputId = "table2")))),
                tabPanel("Mapa de Colaboracion", 
                         h4('Firma conjunta de publicaciones cientificas'),
                         fluidRow(column(12, leafletOutput(outputId = "map3"))),
                         fluidRow(column(6, dataTableOutput("table3a")),column(6, dataTableOutput("table3b")))
                ),
                tabPanel("Mapa Conceptual", 
                         h4('Principales conceptos extraidos de Titulo y Resumen de publicaciones'),
                         visNetworkOutput(outputId = "network4"),
                         fluidRow(column(6, dataTableOutput("table4a")),column(6, dataTableOutput("table4b")))),
                tabPanel("Navegador", 
                         h4('Acceda a las las publicaciones de cada pais'),
                         fluidRow(column(12, selectizeInput('pais', label = "Seleccione Paises", choices = listado_paises2, selected = c("Argentina","Uruguay","Chile","Colombia"), multiple = TRUE))), 
                         fluidRow(column(12, leafletOutput(outputId = "map"))),
                         fluidRow(column(12,DT:: dataTableOutput("table6")))
                ),
                h5('Datos extraidos de https://pubmed.ncbi.nlm.nih.gov/'),
                h6('Estrategia de Busqueda: COVID-19"[All Fields] OR "severe acute respiratory syndrome coronavirus 2"[Supplementary Concept] OR "severe acute respiratory syndrome coronavirus 2"[All Fields] OR "2019-nCoV"[All Fields] OR "SARS-CoV-2"[All Fields] OR "2019nCoV"[All Fields] OR (("Wuhan"[All Fields] AND ("coronavirus"[MeSH Terms] OR "coronavirus"[All Fields])) AND 2019/12[PDAT] : 2030[PDAT])) AND 2020[EDAT] : 2021[EDAT]" ')
    )))



shinyApp(ui, server)



