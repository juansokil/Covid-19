library(readr)
library(leaflet)
library(xlsx)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(data.table)
library(DT)
library(htmltools)


rsconnect::setAccountInfo(name='observatorio-cts',
                          token='EFB80756D7A13A771E3F6419EA165929',
                          secret='DfBCakMJl3pZLkwvNS5aGELaMZ95GK5A72rGazAh')

##########Levanta datos#####

pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data_reduce_inst.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", country="character", Inst="character", Date="Date"))

pubmed_data$Date <- as.Date(with(pubmed_data, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")
pubmed_data$lat <- as.numeric(as.character(pubmed_data$lat))
pubmed_data$long <- as.numeric(as.character(pubmed_data$long))


cantidades <- pubmed_data %>%
  group_by(Inst, lat, long)  %>%
  summarize(cantidad=n_distinct(PMID)) %>%
  arrange(desc(cantidad))


###################SERVER#####################
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({ 
    leaflet() %>% addTiles()  %>%
      addCircleMarkers(data = cantidades,lng = ~long, lat = ~lat,  weight = ~as.numeric(cantidad),  color='purple', layerId = ~unique(Inst), label=~paste0(Inst,': ',cantidad), opacity = 0.6, popup = ~htmlEscape(Inst))
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
    return(a)
  })    
  
  
  
output$table1 <- renderDT(publicaciones <- pubs(), server = FALSE, escape = FALSE)
  

  
}

ui <- fluidPage(mainPanel(
  fluidRow(column(12, leafletOutput("map", height = "600", width = "800")),
           fluidRow(column(12, dataTableOutput("table1", width = "800")))
  )))


shinyApp(ui, server)




