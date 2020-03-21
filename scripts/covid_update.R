####Carga####
#install.packages("RISmed")
#install.packages("qdap")

#https://www.r-bloggers.com/pubmed-search-shiny-app-using-rismed/

library(RISmed)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(qdap)
library(readr)
library(udpipe)
library(tidyverse)
library(xlsx)
library(readxl)
library(data.table)





####Busqueda en PubMed
FechaFiltro = "2020-03-19"

search_topic <- 'COVID-19'
search_query <- EUtilsSummary(search_topic, retmax=2000, mindate=2020,maxdate=2021, db='pubmed')
summary(search_query)

# see the ids of our returned query
QueryId(search_query)

# get actual data from PubMed
records<- EUtilsGet(search_query)

# Save an object to a file
saveRDS(records, file = "../records/pubmed.rds")
# Restore the object
#records <- readRDS(file = "../records/pubmed.rds")

#### arma data.frame ####
pubmed_data <- data.frame('PMID'=PMID(records),
                          'Title'=ArticleTitle(records),
                          'Abstract'=AbstractText(records),
                          'YearPubmed'=YearPubmed(records), 
                          'MonthPubmed'=MonthPubmed(records),
                          'DayPubmed'=DayPubmed(records))


pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
pubmed_data$Abstract <- tolower(pubmed_data$Abstract)
pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)
pubmed_data$Abstract <- gsub("/", " ", pubmed_data$Abstract, fixed = TRUE)


####Agrego la fecha####
pubmed_data$Date <- as.Date(with(pubmed_data, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")

####Evolucion diaria####
pubmed_data %>%
  select(PMID, Title, Abstract, Date) %>%
  group_by(Date)  %>%
  summarize(cantidad=n_distinct(PMID))  %>%
  ggplot(aes(Date, cantidad)) + geom_bar(stat='identity')


#######Trabajo con Afiliaciones########

Afiliaciones=Affiliation(records)
###Las junta en una sola columna
afil = sapply(Afiliaciones, paste, collapse=" ; ")
###Base completa###
publicaciones = cbind(pubmed_data, afil)


#######Crea Tabla afiliaciones - Id ####
afiliaciones = publicaciones %>% 
  select(PMID, afil) %>% 
  separate_rows(afil, sep = " ; " , convert = TRUE)

afiliaciones$country <- sub('.*,\\s*', '', afiliaciones$afil)
afiliaciones$country <- gsub(".", "", afiliaciones$country, fixed = TRUE)


###Filtro#####
pubmed_data <- pubmed_data %>%
  filter(Date >= FechaFiltro) 

####Solo mantengo los nuevos####
afiliaciones = afiliaciones %>% right_join(pubmed_data) %>% unique() 

####GUARDO EL ARCHIVO PARA CLEAN####
#####ESTO ES UN CLEAN MANUAL PARA LOS CASOS QUE NO LOGRAN ENCONTRAR####
write.xlsx2(afiliaciones, '../clean/afiliaciones_clean.xlsx', sheetName="Sheet1",
            col.names=TRUE, row.names=FALSE, append=FALSE)


###ESTA ES LA BASE CLEANEADA#####
afiliaciones_total <- read.xlsx2("../clean/afiliaciones_clean.xlsx", sheetName = "Sheet1")
afiliaciones_total <- afiliaciones_total %>%
  select(PMID, afil, country)


####JUNTO A LA BASE DE PUBMED_DATA###
pubmed_data$PMID <- as.numeric(as.character(pubmed_data$PMID))
afiliaciones_total$PMID <- as.numeric(as.character(afiliaciones_total$PMID))
pubmed_data <- pubmed_data %>% 
  left_join(afiliaciones_total)

###Filtro#####
pubmed_data <- pubmed_data %>%
  filter(Date >= FechaFiltro) 


####Agrego terminos importantes####
pubmed_data$chloroquine <- str_detect(pubmed_data$Abstract, "chloroquine")
pubmed_data$remdesivir <- str_detect(pubmed_data$Abstract, "remdesivir")



glimpse(pubmed_data)

#####################EN ESTE PUNTO TENGO ARMADA LA BASE NUEVA################
#####################TENGO QUE LEVANTAR LA BASE ACUMULADA y HACER UN APPEND################


###Levanto los datos viejos###
pubmed_data_old <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
pubmed_data_old$Date <- as.Date(with(pubmed_data_old, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")


glimpse(pubmed_data)
glimpse(pubmed_data_old)

###Guardo un backup de la base
write.table(pubmed_data_old, file = "../Bases/pubmed_data_old.csv", sep = "\t", qmethod = "double")

####Junto la guardada con la nueva
pubmed_data2 <- rbind(pubmed_data_old, pubmed_data)

####Evolucion diaria### CHEQUEO#
pubmed_data2 %>%
  select(PMID, Title, Abstract, Date) %>%
  group_by(Date)  %>%
  summarize(cantidad=n_distinct(PMID))  %>%
  ggplot(aes(Date, cantidad)) + geom_bar(stat='identity')



###Guardo la base completa###
write.table(pubmed_data2, file = "../Bases/pubmed_data.csv", sep = "\t", qmethod = "double")

dias = pubmed_data2 %>%
  group_by(Date) %>%
  summarize(n_distinct(PMID))




