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
library(countrycode)

setwd('./github/covid-19/scripts')


####Busqueda en PubMed
FechaFiltro = "2020-03-23"

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
pubmed_data$ritonavir <- str_detect(pubmed_data$Abstract, "ritonavir")
pubmed_data$lopinavir <- str_detect(pubmed_data$Abstract, "lopinavir")
pubmed_data$favipiravir <- str_detect(pubmed_data$Abstract, "favipiravir")
pubmed_data$vaccine <- str_detect(pubmed_data$Abstract, "vaccine")

# Convert all to numeric
cols <- sapply(pubmed_data2, is.logical)
pubmed_data2[,cols] <- lapply(pubmed_data2[,cols], as.numeric)


#####################EN ESTE PUNTO TENGO ARMADA LA BASE NUEVA################
#####################TENGO QUE LEVANTAR LA BASE ACUMULADA y HACER UN APPEND################


###Levanto los datos viejos###
pubmed_data_old <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
pubmed_data_old$Date <- as.Date(with(pubmed_data_old, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")



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

pubmed_data2$chloroquine <- str_detect(pubmed_data2$Abstract, "chloroquine")
pubmed_data2$remdesivir <- str_detect(pubmed_data2$Abstract, "remdesivir")
pubmed_data2$ritonavir <- str_detect(pubmed_data2$Abstract, "ritonavir")
pubmed_data2$lopinavir <- str_detect(pubmed_data2$Abstract, "lopinavir")
pubmed_data2$favipiravir <- str_detect(pubmed_data2$Abstract, "favipiravir")
pubmed_data2$vaccine <- str_detect(pubmed_data2$Abstract, "vaccine")

# Convert all to numeric
cols <- sapply(pubmed_data2, is.logical)
pubmed_data2[,cols] <- lapply(pubmed_data2[,cols], as.numeric)



iso <- countrycode(unique(pubmed_data3$country), "country.name", "iso2c")
country <- countrycode(unique(pubmed_data3$country), "country.name", "country.name")
listado_paises <- as.data.frame(cbind(iso, country))
View(listado_paises)

pubmed_data3$country <- str_replace(pubmed_data3$country, "Brasil", "Brazil")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Mixico", "Mexico")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Milan", "Italy")
pubmed_data3$country <- str_replace(pubmed_data3$country, "us", "United States")
pubmed_data3$country <- str_replace(pubmed_data3$country, "us", "United States")
pubmed_data3$country <- str_replace(pubmed_data3$country, "MauritiUnited States", "Mauritius")
pubmed_data3$country <- str_replace(pubmed_data3$country, "RUnited Statessia", "Russia")
pubmed_data3$country <- str_replace(pubmed_data3$country, "RUnited Statessian Federation", "Russia")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Russian Federation", "Russia")

pubmed_data3$country <- str_replace(pubmed_data3$country, "Kingdom of Saudi Arabia", "Saudi Arabia")
pubmed_data3$country <- str_replace(pubmed_data3$country, "UK", "United Kingdom")

pubmed_data3$country <- str_replace(pubmed_data3$country, "AUnited Statestralia", "Australia")
pubmed_data3$country <- str_replace(pubmed_data3$country, "AUnited Statestria", "Austria")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Montreal", "Canada")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Korea", "South Korea")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Hong Kong", "Hong Kong SAR China")
pubmed_data3$country <- str_replace(pubmed_data3$country, "Republic of Congo", "Congo - Brazzaville")

pubmed_data3$country <- str_replace(pubmed_data3$country, "South South Korea", "South Korea")







pubmed_data3 <- pubmed_data3 %>%
  left_join(listado_paises, by = c("country"="country"))
View(pubmed_data3)


vvvvv = pubmed_data3 %>%
  group_by(country,iso.x) %>%
  summarize(n_distinct(PMID))

View(vvvvv)

###Guardo la base completa###
write.table(pubmed_data2, file = "../Bases/pubmed_data.csv", sep = "\t", qmethod = "double")

dias = pubmed_data2 %>%
  group_by(Date) %>%
  summarize(n_distinct(PMID))


#pubmed_data2 %>%
#  filter(vaccine == 1) %>%
#  summarise(n_distinct(PMID))



