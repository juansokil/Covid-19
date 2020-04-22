library(leaflet)
library(dplyr)
library(readr)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)
library(htmltools)
library(leaflet.minicharts)


###Levanta Coordenadas###
trials <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/trials.txt", "\t", escape_double = FALSE, trim_ws = TRUE)


countries_coord2 <- trials %>%
  group_by(country, ISO2, latitude, longitude) %>%
  summarize(count=n_distinct(NCTNumber))  %>%
  filter(!is.na(ISO2))


###################SERVER#####################
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({ 

leaflet() %>% addTiles()  %>%
  #  addCircles(data = countries_coord2,lng = ~longitude, lat = ~latitude,  weight = 1, radius = ~sqrt(count) * 60000, color='red' )  %>%
  addMinicharts(countries_coord2$longitude, countries_coord2$latitude,chartdata = countries_coord2$count,showLabels = TRUE,width = 35) %>%
    addCircleMarkers(data = countries_coord2,lng = ~longitude, lat = ~latitude,  weight = ~as.numeric(count),  color='purple', layerId = ~unique(country), label=~paste0(country,': ',count), opacity = 0.6, popup = ~htmlEscape(country))
  })
  

trial <- reactive({
  a <- trials  %>% filter(country %in% input$map_marker_click$id)
  a <- data.frame(a)
  a <- a %>%
    select(NCTNumber, country)  %>%
    #mutate(Link=paste0('https://pubmed.ncbi.nlm.nih.gov/',PMID,'/')) %>%
    mutate(Link=paste0("<a href='https://clinicaltrials.gov/ct2/show/",NCTNumber,"/' target='_blank' >Ver Articulo</a>")) %>%
    #select(Date, Title, Link)   %>%
    
    
    unique() %>%
    arrange(desc(NCTNumber))
  return(a)
})    




output$table1 <- renderDT(publicaciones <- trial(), server = FALSE, escape = FALSE)



}

ui <- fluidPage(mainPanel(
  fluidRow(column(12, leafletOutput("map", height = "600", width = "800")),
           fluidRow(column(12, dataTableOutput("table1", width = "800")))
  ))) 





shinyApp(ui, server)


