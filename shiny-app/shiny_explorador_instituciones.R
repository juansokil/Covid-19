


library(readr)
library(leaflet)
library(xlsx)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)
library(htmltools)
library(stringr)
library(ggiraph)
library(ggplot2)
library(leaflet.minicharts)
library(tibble)
library(lubridate)




##########Levanta datos#####

pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce_inst.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", country="character", Inst="character", Date="Date"))
#pubmed_data <- read.table("C:/Users/Juan/Documents/GitHub/Covid-19/bases/pubmed_data_reduce_inst2.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", country="character", Inst="character", Date="Date"))


pubmed_data$Date <- as.Date(with(pubmed_data, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")
pubmed_data$lat <- as.numeric(as.character(pubmed_data$lat))
pubmed_data$long <- as.numeric(as.character(pubmed_data$long))

pubmed_data$lat <- round(pubmed_data$lat, digits = 2)
pubmed_data$long <- round(pubmed_data$long, digits = 2)

pubmed_data <- pubmed_data %>%
  filter(!is.na(lat))  %>%
  arrange(Inst, lat, long)

dsdad=pubmed_data %>% group_by(Inst) %>% summarize(n_distinct(lat))


cantidades <- pubmed_data %>%
  group_by(Inst, lat, long)  %>%
  summarize(cantidad=n_distinct(PMID)) %>%
  arrange(Inst, lat, long)




server <- function(input, output, session){
  
  output$map <- renderLeaflet({ 
    leaflet() %>% addTiles()  %>%
      addCircleMarkers(data = cantidades,lng = ~long, 
                       lat = ~lat,  weight = ~sqrt(cantidad)*5,  
                       color='purple', layerId = ~unique(Inst), 
                       label=~paste0(Inst,': \n',cantidad), opacity = 0.6, popup = ~htmlEscape(Inst))
  })
  
  
  
  
  pubs <- reactive({
    a <- pubmed_data  %>% filter(Inst %in% input$map_marker_click$id)
    a <- data.frame(a)
    a <- a %>%
      select(Date, Title, PMID, iso)  %>%
      #mutate(Link=paste0('https://pubmed.ncbi.nlm.nih.gov/',PMID,'/')) %>%
      mutate(Link=paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",PMID,"/' target='_blank' >Ver Articulo</a>")) %>%
      select(Date, Title, Link)   %>%
      unique() %>%
      arrange(desc(Date))  
    
    a$Link <- str_replace(a$Link, "https://pubmed.ncbi.nlm.nih.gov/NA/", "")
    
    
    return(a)
  })    
  
  
  
  output$table1 <- renderDT(publicaciones <- pubs(), server = FALSE, escape = FALSE)
  
  
  
}



###################




ui <- function(id) {
  fluidPage(
    fluidRow(column(12, leafletOutput("map", height = "600", width = "800"))),
    fluidRow(column(12,DT::dataTableOutput("table1", width = "800"))))
}



shinyApp(ui, server)










