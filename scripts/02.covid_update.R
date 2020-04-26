####Carga####
#install.packages("RISmed")
#install.packages("qdap")

#https://www.r-bloggers.com/pubmed-search-shiny-app-using-rismed/

#options(java.parameters = "-Xmx8g")

library(RISmed)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(xlsx)
library(data.table)
library(countrycode)
library(igraph)

setwd('./github/covid-19/scripts')


####Busqueda en PubMed
FechaFiltro = Sys.Date()
FechaFiltro
FechaFiltroInicio = "2020/04/26"
FechaFiltroFin = "2020/04/26"

search_topic <- 'COVID-19'
search_query <- EUtilsSummary(search_topic, retmax=10000, mindate=paste0(FechaFiltroInicio),maxdate=paste0(FechaFiltroFin), db='pubmed')


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

dias


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

###Filtro#####
pubmed_data <- pubmed_data %>%
  filter(Date >= FechaFiltro) 






################################################################
################################################################
################################################################
#######CLEAN UP#################################################
################################################################
################################################################
################################################################

####Remove Numbers##############
afiliaciones$afil <- str_replace_all(afiliaciones$afil, "[:digit:]", "")
####Delete mails####
afiliaciones$afil <- gsub("[^\\s]*@[^\\s]*","", afiliaciones$afil, perl = TRUE) 
afiliaciones$afil <- str_remove(afiliaciones$afil, "Electronic address:")


#######get country####### 
afiliaciones$country <- sub('.*,\\s*', '', afiliaciones$afil)

####delete punctuation####
afiliaciones$country <- gsub(".", " ", afiliaciones$country, fixed = TRUE)
afiliaciones$country <- gsub(";", " ", afiliaciones$country, fixed = TRUE)

###Trim####################
afiliaciones$country <- str_trim(afiliaciones$country)

####Delete duplicates affiliations####
afiliaciones <- afiliaciones %>% unique()

####Join with PubMed Data###
afiliaciones <- afiliaciones %>% right_join(pubmed_data) %>% unique() 


################################################################
################################################################
################################################################
################################################################
################################################################
#############################Country - Clean####################
################################################################
################################################################

afiliaciones$country <- gsub(".*United States.*", "United States", afiliaciones$country)
afiliaciones$country <- str_replace(afiliaciones$country, "USA", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "New York", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Washington", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "California", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Massachusetts", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Pennsylvania", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Oklahoma", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Ohio", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Texas", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Maryland", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Illinois", "United States")
afiliaciones$country <- str_replace(afiliaciones$country, "Michigan", "United States")


afiliaciones$country <- gsub(".*China.*", "China", afiliaciones$country)
afiliaciones$country <- gsub(".*Beijing.*", "China", afiliaciones$country)
afiliaciones$country <- gsub(".*Wuhan.*", "China", afiliaciones$country)
afiliaciones$country <- gsub(".*Hong Kong*", "Hong Kong", afiliaciones$country)

afiliaciones$country <- str_replace(afiliaciones$country, "UK", "United Kingdom")
afiliaciones$country <- str_replace(afiliaciones$country, "England", "United Kingdom")
afiliaciones$country <- str_replace(afiliaciones$country, "Scotland", "United Kingdom")
afiliaciones$country <- str_replace(afiliaciones$country, "London", "United Kingdom")

afiliaciones$country <- str_replace(afiliaciones$country, "Istanbul", "Turkey")

afiliaciones$country <- str_replace(afiliaciones$country, "Republic of Singapore", "Singapore")

afiliaciones$country <- gsub(".*Korea.*", "South Korea", afiliaciones$country)




afiliaciones$country <- str_replace(afiliaciones$country, "Deutschland", "Germany")
afiliaciones$country <- str_replace(afiliaciones$country, "España", "Spain")
afiliaciones$country <- str_replace(afiliaciones$country, "Milan", "Italy")

afiliaciones$country <- str_replace(afiliaciones$country, "Kingdom of Saudi Arabia", "Saudi Arabia")

afiliaciones$country <- gsub(".*Netherlands*", "Netherlands", afiliaciones$country)

afiliaciones$country <- str_replace(afiliaciones$country, "Viet Nam", "Vietnam")

afiliaciones$country <- gsub(".*Hong Kong*", "Hong Kong SAR China", afiliaciones$country)

afiliaciones$country <- str_replace(afiliaciones$country, "Wellington", "New Zealand")
afiliaciones$country <- str_replace(afiliaciones$country, "Christchurch", "New Zealand")
afiliaciones$country <- str_replace(afiliaciones$country, "Auckland", "New Zealand")


afiliaciones$country <- gsub(".*Spain.*", "Spain", afiliaciones$country)

afiliaciones$country <- gsub(".*Brazil.*", "Brazil", afiliaciones$country)
afiliaciones$country <- gsub(".*Brasil.*", "Brazil", afiliaciones$country)

afiliaciones$country <- gsub(".*Italia.*", "Italy", afiliaciones$country)
afiliaciones$country <- gsub(".*Italy.*", "Italy", afiliaciones$country)

afiliaciones$country <- gsub(".*Singapore.*", "Singapore", afiliaciones$country)
afiliaciones$country <- gsub(".*Bangkok.*", "Thailand", afiliaciones$country)
afiliaciones$country <- gsub(".*India.*", "India", afiliaciones$country)
afiliaciones$country <- gsub(".*Iran.*", "Iran", afiliaciones$country)


##################TRIM######################
afiliaciones$country <- str_trim(afiliaciones$country)
afiliaciones$Inst <- ''
afiliaciones$lat <- ''
afiliaciones$long <- ''




#########################GUARDO EL ARCHIVO PARA CLEAN####
#####ESTO ES UN CLEAN MANUAL PARA LOS CASOS QUE NO LOGRAN ENCONTRAR####
write.xlsx2(afiliaciones, '../clean/afiliaciones_clean.xlsx', sheetName="Sheet1",
            col.names=TRUE, row.names=FALSE, append=FALSE)




###ESTA ES LA BASE CLEANEADA#####
afiliaciones_total <- read.xlsx2("../clean/afiliaciones_clean.xlsx", sheetName = "Sheet1")
afiliaciones_total <- afiliaciones_total %>%
  select(PMID, afil, country, Inst, lat, long)



####JUNTO A LA BASE DE PUBMED_DATA###
pubmed_data$PMID <- as.numeric(as.character(pubmed_data$PMID))
afiliaciones_total$PMID <- as.numeric(as.character(afiliaciones_total$PMID))
pubmed_data <- pubmed_data %>% 
  left_join(afiliaciones_total)

###Filtro#####
pubmed_data <- pubmed_data %>%
  filter(Date >= FechaFiltro) 

pubmed_data$lat <- as.numeric(as.character(pubmed_data$lat))
pubmed_data$long <- as.numeric(as.character(pubmed_data$long))


####Agrego terminos importantes####
pubmed_data$chloroquine <- str_detect(pubmed_data$Abstract, "chloroquine")
pubmed_data$hydroxychloroquine <- str_detect(pubmed_data$Abstract, "hydroxychloroquine")
pubmed_data$remdesivir <- str_detect(pubmed_data$Abstract, "remdesivir")
pubmed_data$ritonavir <- str_detect(pubmed_data$Abstract, "ritonavir")
pubmed_data$lopinavir <- str_detect(pubmed_data$Abstract, "lopinavir")
pubmed_data$favipiravir <- str_detect(pubmed_data$Abstract, "favipiravir")
pubmed_data$vaccine <- str_detect(pubmed_data$Abstract, "vaccine")
pubmed_data$interferon <- str_detect(pubmed_data$Abstract, "interferon")
pubmed_data$azithromycin <- str_detect(pubmed_data$Abstract, "azithromycin")
pubmed_data$tocilizumab <- str_detect(pubmed_data$Abstract, "tocilizumab")


# Convert all to numeric
cols <- sapply(pubmed_data, is.logical)
pubmed_data[,cols] <- lapply(pubmed_data[,cols], as.numeric)



########################################
########################################
####VER ESTO ANTES DE EJECUTARLO#####
pubmed_data$treatment <-   apply(X = pubmed_data[,11:19], MARGIN = 1, FUN = max, na.rm = TRUE)
########################################
########################################


pubmed_data$country <- str_replace(pubmed_data$country, "Hong Kong", "Hong Kong SAR China")


iso <- countrycode(unique(pubmed_data$country), "country.name", "iso2c")
country <- countrycode(unique(pubmed_data$country), "country.name", "country.name")
listado_paises <- as.data.frame(cbind(iso, country))



pubmed_data <- pubmed_data %>%
  left_join(listado_paises, by = c("country"="country"))



####Ordeno los datos####
pubmed_data <- pubmed_data %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, interferon, azithromycin, tocilizumab, treatment, vaccine, Inst, lat, long)



#####################EN ESTE PUNTO TENGO ARMADA LA BASE NUEVA################
#####################TENGO QUE LEVANTAR LA BASE ACUMULADA y HACER UN APPEND################


###Levanto los datos viejos################REVISAR ESTO MAÑANA####
pubmed_data_old <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
pubmed_data_old$Date <- as.Date(with(pubmed_data_old, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")


####Ordeno los datos####
pubmed_data_old <- pubmed_data_old %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, interferon, azithromycin, tocilizumab, treatment, vaccine, Inst, lat, long)


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




####Agrego terminos importantes####

pubmed_data2$TitleAbstract <- paste(pubmed_data2$Title,pubmed_data2$Abstract,sep=" /t ")


pubmed_data2$chloroquine <- str_detect(pubmed_data2$TitleAbstract, "chloroquine")
pubmed_data2$hydroxychloroquine <- str_detect(pubmed_data2$TitleAbstract, "hydroxychloroquine")
pubmed_data2$remdesivir <- str_detect(pubmed_data2$TitleAbstract, "remdesivir")
pubmed_data2$ritonavir <- str_detect(pubmed_data2$TitleAbstract, "ritonavir")
pubmed_data2$lopinavir <- str_detect(pubmed_data2$TitleAbstract, "lopinavir")
pubmed_data2$favipiravir <- str_detect(pubmed_data2$TitleAbstract, "favipiravir")
pubmed_data2$interferon <- str_detect(pubmed_data2$TitleAbstract, "interferon")
pubmed_data2$azithromycin <- str_detect(pubmed_data2$TitleAbstract, "azithromycin")
pubmed_data2$tocilizumab <- str_detect(pubmed_data2$TitleAbstract, "tocilizumab")
pubmed_data2$vaccine <- str_detect(pubmed_data2$TitleAbstract, "vaccine")

# Convert all to numeric
cols <- sapply(pubmed_data2, is.logical)
pubmed_data2[,cols] <- lapply(pubmed_data2[,cols], as.numeric)
pubmed_data2$treatment <-   apply(X = pubmed_data2[,11:19], MARGIN = 1, FUN = max, na.rm = TRUE)


  
pubmed_data2 <- pubmed_data2 %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir, interferon, azithromycin, tocilizumab, treatment, vaccine, Inst, lat, long)



###Guardo la base completa###
write.table(pubmed_data2, file = "../Bases/pubmed_data.csv", sep = "\t", qmethod = "double")




###########Guardo la basereduce
pubmed_data_reduce <- pubmed_data2 %>%
  select(PMID, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir,interferon, azithromycin, tocilizumab, treatment, vaccine, Title, Inst, lat, long) %>%
  unique()
write.table(pubmed_data_reduce, file = "../Bases/pubmed_data_reduce.csv", sep = "\t", qmethod = "double")


####Guarda la base de inst####
pubmed_data_reduce_inst <- pubmed_data_reduce %>%
filter(lat!='') 
write.table(pubmed_data_reduce_inst, file = "../Bases/pubmed_data_reduce_inst.csv", sep = "\t", qmethod = "double")

########################ARMA GRAFO##################


listado_dias <- pubmed_data2 %>%
  select(Date)  %>%
  unique() %>%
  arrange(Date) %>%
  as.list()


countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)
edges_for_plot <- c()

#dia=18280
##########ARMA GRAFO#############
for (dia in listado_dias$Date){
  print(dia)
  
  ###Filtro#####
  pubmed_data3 <- pubmed_data2 %>%
    filter(Date <= dia)  %>%
    arrange(Date)  %>%
    select(PMID, Date, country, iso)  %>%
    unique()
  
  
  
  #######################ARMA NODOS #######################
  nodos <- pubmed_data3 %>%
    select(iso, PMID) %>%
    unique()  %>%
    filter(!is.na(iso)) %>%
    filter(iso!='')  %>%
    group_by(iso) %>%
    summarize(totales=n_distinct(PMID))
  
  #######################ARMA VERTICES #######################
  byHand <- pubmed_data3 %>%
    select(PMID, iso) %>%
    unique() %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
    group_by(PMID) %>% mutate(paises = paste(iso, collapse = ","))  %>%
    select(PMID,paises) %>%
    unique()
  
  
  #### identifico el primero de los autores###
  byHand$primer_pais = as.character(lapply(strsplit(as.character(byHand$paises), split=","), "[", 1))
  arreglado <- byHand %>% mutate(paises = strsplit(as.character(paises), ",")) %>% unnest(paises)
  aristas_previo <- arreglado[c(2:3)]
  colnames(aristas_previo) <- c("source","target")
  aristas <- aristas_previo %>% group_by(source, target) %>% summarize(count=n())
  
  nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = nodos$totales, count=nodos$totales)     # size 
  edges <- data.frame(source = aristas$source, target = aristas$target, weight = aristas$count)
  edges <- edges %>%
    filter(as.character(source) != as.character(target)) %>% 
    mutate(dia=as.Date(dia, origin = "1970-01-01"))
  
  countries_coord <- nodes %>%
    left_join(countries, by=c('id'='country'))
  
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  g<- simplify(g, remove.multiple = TRUE)
  
  relaciones_dia  <- edges %>%
    inner_join(countries_coord %>% select(id, longitude, latitude), by = c('source' = 'id')) %>%
    rename(x = longitude, y = latitude) %>%
    inner_join(countries_coord %>% select(id, longitude, latitude), by = c('target' = 'id')) %>%
    rename(xend = longitude, yend = latitude) 
  
  
  #relaciones_dia$fecha <- dia
  
  edges_for_plot <- rbind(edges_for_plot,relaciones_dia)
}


edges_for_plot_ud <- edges_for_plot %>%
  filter(dia == max(dia))

write.table(edges_for_plot, file = "../Bases/edges_for_plot_historico.csv", sep = "\t", qmethod = "double")
write.table(edges_for_plot_ud, file = "../Bases/edges_for_plot.csv", sep = "\t", qmethod = "double")






############################################
############################################
############################################


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



bla = pubmed_data2 %>%
  filter (favipiravir == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)


bla = pubmed_data2 %>%
  filter (interferon == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)


bla = pubmed_data2 %>%
  filter (azithromycin == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)

bla = pubmed_data2 %>%
  filter (tocilizumab == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)



bla = pubmed_data2 %>%
  filter (treatment == 1) %>%
  group_by(Date) %>%
  summarize(dia=n_distinct(PMID))  %>% 
  mutate(total = cumsum(dia))
View(bla)


View(dias)






#############################################################################
#############################################################################
###############PARA CORRECCIONES GENERALES EN LA BASE########################
#############################################################################
#############################################################################

####Levanto los datos viejos##### SI QUIERO MODIFICAR ALGO SOBRE LA BASE TOTAL, SIN UPDATE############
##pubmed_data2 <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
#write.xlsx2(pubmed_data2, '../clean/base_completa_clean.xlsx', sheetName="Sheet1",col.names=TRUE, row.names=FALSE, append=FALSE)
#pubmed_data2 <- read.xlsx2("../clean/base_completa_clean.xlsx", sheetName = "Sheet1")
#pubmed_data2$Date <- as.Date(with(pubmed_data2, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")
###Guardo la base completa###
#write.table(pubmed_data2, file = "../Bases/pubmed_data_bck.csv", sep = "\t", qmethod = "double")
#############################################################################
#############################################################################
#############################################################################
#############################################################################




