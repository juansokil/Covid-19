
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
library(maps)
library(RColorBrewer)
library(ggiraph)
#install.packages("ggiraphExtra")
#install.packages("leaflet.minicharts")
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
#install.packages("magick")
library(magick)


pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(country="character",  Date="Date"))
edges_for_plot <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/edges_for_plot.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(dia="Date"))
edges_for_plot_ud <- edges_for_plot %>%
  filter(dia == '2020-04-10')

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

###Levanta Coordenadas###
countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)
countries_coord <- nodes %>%
  left_join(countries, by=c('id'='country'))

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




    ggplot(countries_coord) + country_shapes +
      geom_curve(aes(x = x, y = y, xend = xend, yend= yend), data = edges_for_plot_ud, alpha = 0.5) +
      #geom_segment(aes(x = x, y = y, xend = xend, yend = yend, size=weight/12, color="blue") , data = edges_for_plot ,alpha = 0.5) +
      geom_point(aes(x = longitude, y = latitude, size=count),           # draw nodes
                 shape = 21, fill = 'white',  
                 color = 'black', stroke = 0.5) +
      scale_size_continuous(guide = FALSE, range = c(2, 20)) +    # scale for node size
      geom_text(aes(x = longitude, y = latitude, label = id),             # draw text labels
                hjust = 0, nudge_x = 1, nudge_y = 4,
                size = 3, color = "red", fontface = "bold")
    
  
    # Example image### Levanta la imagen - le pega el texto###
    descarga01 <- image_read("./descarga01.png")
    descarga01 <- image_annotate(descarga01, "17-01-2020", size = 30, color = "red", location = "+500+400")
    image_write(descarga01, path = "./ok_descarga01.png", format = "png")
    
    
    