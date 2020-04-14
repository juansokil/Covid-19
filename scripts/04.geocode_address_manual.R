# Geocoding a csv column of "addresses" in R

#load ggmap

library(dplyr)
library(tidygeocoder)

base_ibero <- read.xlsx2("../Bases/base_ibero.xlsx", sheetName = "Sheet1")


instituciones_lista = base_ibero %>%
  filter(!Inst =='') %>%
  group_by(Inst) %>%
  summarize(cantidad =n_distinct(PMID)) %>%
  arrange(desc(cantidad)) %>%
  #Filtro los que tienen mas de uno#
  head(47) 

instituciones <- as.character(unique(instituciones_lista$Inst))

prueba <- geo_osm(instituciones[2])  
direccion_georef <- cbind(instituciones[2], as.numeric(prueba$lat), as.numeric(prueba$long))


for (i in 1:length(instituciones)) {
#for (i in 2:5) {
  prueba <- geo_osm(instituciones[i])  
  #print('geocodificando',instituciones[i])
  if (nrow(prueba)==1){
    fila <- cbind(instituciones[i], as.numeric(prueba$lat), as.numeric(prueba$long))
    direccion_georef <- rbind(direccion_georef, fila)
}}

bla <- as.data.frame((direccion_georef))
bla$V2 <- as.numeric(as.character(bla$V2))
bla$V3 <- as.numeric(as.character(bla$V3))

bla <- bla %>%
  left_join(instituciones_lista, by=c('V1'='Inst')) %>%
  unique()


write.xlsx2(bla, '../Bases/georef.xlsx', sheetName="Sheet1",col.names=TRUE, row.names=FALSE, append=FALSE)
bla <- read.xlsx2("../Bases/georef.xlsx", sheetName = "Sheet1")




leaflet() %>% addTiles()  %>%
  addCircles(data = bla,lng = ~V3, lat = ~V2,  weight = ~cantidad*3,  color='purple',  label=~paste0(V1), opacity = 0.6) 

