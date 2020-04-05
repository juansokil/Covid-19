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
library(igraph)
#install.packages("visNetwork")
library(visNetwork)
library(stringr)

setwd('./github/covid-19/scripts')


####Busqueda en PubMed
FechaFiltro = "2020-04-05"

search_topic <- 'COVID-19'
#search_topic <- 'COVID-19|hydroxychloroquine+COVID-19|chloroquine+COVID-19'
search_query <- EUtilsSummary(search_topic, retmax=5000, mindate=2020,maxdate=2021, db='pubmed')

summary(search_query)

# see the ids of our returned query
#QueryId(search_query)

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


dias = pubmed_data %>%
  select(PMID, Title, Abstract, Date) %>%
  group_by(Date)  %>%
  summarize(cantidad=n_distinct(PMID))
View(dias)



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
pubmed_data$hydroxychloroquine <- str_detect(pubmed_data$Abstract, "hydroxychloroquine")
pubmed_data$remdesivir <- str_detect(pubmed_data$Abstract, "remdesivir")
pubmed_data$ritonavir <- str_detect(pubmed_data$Abstract, "ritonavir")
pubmed_data$lopinavir <- str_detect(pubmed_data$Abstract, "lopinavir")
pubmed_data$favipiravir <- str_detect(pubmed_data$Abstract, "favipiravir")
pubmed_data$vaccine <- str_detect(pubmed_data$Abstract, "vaccine")


# Convert all to numeric
cols <- sapply(pubmed_data, is.logical)
pubmed_data[,cols] <- lapply(pubmed_data[,cols], as.numeric)

pubmed_data$country <- str_replace(pubmed_data$country, "UK", "United Kingdom")
pubmed_data$country <- str_replace(pubmed_data$country, "Hong Kong", "Hong Kong SAR China")



iso <- countrycode(unique(pubmed_data$country), "country.name", "iso2c")
country <- countrycode(unique(pubmed_data$country), "country.name", "country.name")
listado_paises <- as.data.frame(cbind(iso, country))



pubmed_data <- pubmed_data %>%
  left_join(listado_paises, by = c("country"="country"))



####Ordeno los datos####
pubmed_data <- pubmed_data %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, vaccine)




#####################EN ESTE PUNTO TENGO ARMADA LA BASE NUEVA################
#####################TENGO QUE LEVANTAR LA BASE ACUMULADA y HACER UN APPEND################


###Levanto los datos viejos###
pubmed_data_old <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
pubmed_data_old$Date <- as.Date(with(pubmed_data_old, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")


####Ordeno los datos####
pubmed_data_old <- pubmed_data_old %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, vaccine)


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


pubmed_data2$country <- str_replace(pubmed_data2$country, "Côte d'Ivoire", "Ivory Coast")




####Levanto los datos viejos##### SI QUIERO MODIFICAR ALGO SOBRE LA BASE TOTAL, SIN UPDATE############
#pubmed_data2 <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
#                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))


####Agrego terminos importantes####

pubmed_data2$TitleAbstract <- paste(pubmed_data2$Title,pubmed_data2$Abstract,sep=" /t ")


pubmed_data2$chloroquine <- str_detect(pubmed_data2$TitleAbstract, "chloroquine")
pubmed_data2$hydroxychloroquine <- str_detect(pubmed_data2$TitleAbstract, "hydroxychloroquine")
pubmed_data2$remdesivir <- str_detect(pubmed_data2$TitleAbstract, "remdesivir")
pubmed_data2$ritonavir <- str_detect(pubmed_data2$TitleAbstract, "ritonavir")
pubmed_data2$lopinavir <- str_detect(pubmed_data2$TitleAbstract, "lopinavir")
pubmed_data2$favipiravir <- str_detect(pubmed_data2$TitleAbstract, "favipiravir")
pubmed_data2$vaccine <- str_detect(pubmed_data2$TitleAbstract, "vaccine")





# Convert all to numeric
cols <- sapply(pubmed_data2, is.logical)
pubmed_data2[,cols] <- lapply(pubmed_data2[,cols], as.numeric)

pubmed_data2 <- pubmed_data2 %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, vaccine)


###Guardo la base completa###
write.table(pubmed_data2, file = "../Bases/pubmed_data.csv", sep = "\t", qmethod = "double")

###########Guardo la basereduce
pubmed_data_reduce <- pubmed_data2 %>%
  select(PMID, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, vaccine)
write.table(pubmed_data_reduce, file = "../Bases/pubmed_data_reduce.csv", sep = "\t", qmethod = "double")


bla = pubmed_data2 %>%
  filter (vaccine == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)

bla = pubmed_data2 %>%
  filter (hydroxychloroquine == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)

bla = pubmed_data2 %>%
  filter (chloroquine == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)

bla = pubmed_data2 %>%
  filter (lopinavir == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)

bla = pubmed_data2 %>%
  filter (remdesivir == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)


bla = pubmed_data2 %>%
  filter (ritonavir == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)







###https://www.r-bloggers.com/covid-19-shiny-plotly-dashboard/











#############CLEAN A MANOPLA DE COSAS VIEJAS#################

#write.xlsx2(pubmed_data2, '../clean/base_completa_clean.xlsx', sheetName="Sheet1",
#            col.names=TRUE, row.names=FALSE, append=FALSE)

pubmed_data2_copy <- read.xlsx2("../clean/base_completa_clean.xlsx", sheetName = "Sheet1")
pubmed_data2_copy$Date <- as.Date(with(pubmed_data2_copy, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")



dias = pubmed_data2_copy %>%
  select(PMID, Title, Abstract, Date) %>%
  group_by(Date)  %>%
  summarize(cantidad=n_distinct(PMID))
View(dias)



###Guardo la base completa###
write.table(pubmed_data2_copy, file = "../Bases/pubmed_data_nv.csv", sep = "\t", qmethod = "double")


