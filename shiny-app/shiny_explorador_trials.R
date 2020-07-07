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
trials <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/trials2.txt", "\t", escape_double = FALSE, trim_ws = TRUE)



countries_coord2 <- trials %>%
  group_by(country, ISO2, latitude, longitude) %>%
  #summarize(count=n_distinct(NCTNumber))  %>%
  summarize(count=n_distinct(Link))  %>%
  filter(!is.na(ISO2))  %>%
  filter(!is.na(latitude))
  
cantidades <- trials %>%
  group_by(country, ISO2) %>%
  #filter(ISO2 %in% c('ES', 'PT', 'AR','BR','UY','PY','BO','CL','PE','EC','CO','VE','PA','CR','DO','CU','NI','HN','GT','MX','SV','PR'))  %>%
  #summarize(cantidad=n_distinct(NCTNumber))  %>%
  summarize(cantidad=n_distinct(Link))  %>%
  filter(!is.na(ISO2))   %>%
  arrange(desc(cantidad))



###################SERVER#####################
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({ 

leaflet() %>% addTiles()  %>%
    addCircleMarkers(data = countries_coord2,lng = ~longitude, lat = ~latitude,  weight = ~as.numeric(sqrt(count)*3),  color='purple', layerId = ~unique(country), label=~paste0(country,': ',count), opacity = 0.6, popup = ~htmlEscape(country))
  })
  

trial <- reactive({
  a <- trials  %>% filter(country %in% input$map_marker_click$id)
  a <- data.frame(a)
  a <- a %>%
    #select(Title, country, NCTNumber)  %>%
    select(Title, country, ISO2, Link)  %>%
    #mutate(Link=paste0("<a href='https://clinicaltrials.gov/ct2/show/",NCTNumber,"/' target='_blank' >Ver Ensayo</a>")) %>%
    mutate(Link=paste0("<a href='",Link,"' target='_blank' >Ver Ensayo</a>")) %>%
    select(Title, country, Link)   %>%
    unique() 
  return(a)
})    


output$table1 <- renderDT(publicaciones <- trial(), server = FALSE, escape = FALSE)

}

ui <- fluidPage(mainPanel(
  fluidRow(column(12, leafletOutput("map")),
           fluidRow(#column(6, dataTableOutput("table2")),
                    column(12, dataTableOutput("table1"))))
  )) 





shinyApp(ui, server)

