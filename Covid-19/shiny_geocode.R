library(leaflet)
library(xlsx)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(data.table)


rsconnect::setAccountInfo(name='observatorio-cts',
                          token='EFB80756D7A13A771E3F6419EA165929',
                          secret='DfBCakMJl3pZLkwvNS5aGELaMZ95GK5A72rGazAh')

##########Levanta datos#####


bla <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/georef.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(V2="numeric",  V3="numeric",  cantidad="numeric"))

#bla <- read.xlsx2("../Bases/georef.xlsx", sheetName = "Sheet1")
#bla$V2 <- as.numeric(as.character(bla$V2))
#bla$V3 <- as.numeric(as.character(bla$V3))
#bla$cantidad <- as.numeric(as.character(bla$cantidad))



###################SERVER#####################
server <- function(input, output, session) {
  
  
  output$map <- renderLeaflet({ 
    leaflet() %>% addTiles()  %>%
      addCircles(data = bla,lng = ~V3, lat = ~V2,  weight = ~as.numeric(cantidad)*3,  color='purple',  label=~paste0(V1,'\n',cantidad), opacity = 0.6)
    #    addMarkers(data = bla,lng = ~V3, lat = ~V2,  label=~paste0(V1,'\n',cantidad)) 
  })
  
  
  
}

ui <- fluidPage(mainPanel(
  fluidRow(column(12, leafletOutput("map"))
  )))


shinyApp(ui, server)




