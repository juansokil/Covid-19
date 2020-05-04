
#####################################################################
#######################Load Libraries################################
#####################################################################
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
library(tidytext)
library(ggraph)
library(widyr)
library(udpipe)
library(visNetwork)

setwd('./github/covid-19/scripts')

#####################################################################
###########################Load UDPIPE model#########################
#####################################################################
#udmodel <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model("../models/english-ewt-ud-2.4-190531.udpipe")

###############################RISMED################################

#####################################################################
####################Search in pubmed - daily#########################
#####################################################################
FechaFiltro = Sys.Date()
FechaFiltroFormato = gsub("-", '/', FechaFiltro, fixed = T)
FechaFiltroInicio = FechaFiltroFormato
FechaFiltroFin = FechaFiltroFormato

search_topic <- 'COVID-19'
search_query <- EUtilsSummary(search_topic, retmax=10000, mindate=paste0(FechaFiltroInicio),maxdate=paste0(FechaFiltroFin), db='pubmed')
#search_query <- EUtilsSummary(search_topic, retmax=10000, mindate='2020/05/04',maxdate='2020/05/04', db='pubmed')
#search_query <- EUtilsSummary(search_topic, retmax=10000, mindate=2020,maxdate=2020, db='pubmed')
summary(search_query)


####################################################################
#################get data from PubMed###############################
####################################################################
records<- EUtilsGet(search_query)

# Save an object to a file
#saveRDS(records, file = "../records/pubmed.rds")
# Restore the object
#records <- readRDS(file = "../records/pubmed.rds")

############################
#### Create data.frame #####
############################
pubmed_data <- data.frame('PMID'=PMID(records),
                          'Title'=ArticleTitle(records),
                          'Abstract'=AbstractText(records),
                          'YearPubmed'=YearPubmed(records), 
                          'MonthPubmed'=MonthPubmed(records),
                          'DayPubmed'=DayPubmed(records))

####Abstract Clean####
pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
pubmed_data$Abstract <- tolower(pubmed_data$Abstract)
pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)
pubmed_data$Abstract <- gsub("/", " ", pubmed_data$Abstract, fixed = TRUE)

####Create Date####
pubmed_data$Date <- as.Date(with(pubmed_data, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")

####Daily Control###
pubmed_data %>%
  select(PMID, Title, Abstract, Date) %>%
  group_by(Date)  %>%
  summarize(cantidad=n_distinct(PMID))


############################
####### Affiliations #######
############################

Afiliaciones=Affiliation(records)
afil = sapply(Afiliaciones, paste, collapse=" ; ")

###Join dataframes
publicaciones = cbind(pubmed_data, afil)

#######Create Table Affiliations - PMID ####
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

###########################################################
#############SAVE AFFILIATION TABLE To EXCEL FILE##########
###########################################################

write.xlsx2(afiliaciones, '../clean/afiliaciones_clean.xlsx', sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)


###########################################################
###########################################################
#####OPTIONAL - MANUAL CLEAN UP FOR THE AFFILIATIONS#######
###########################################################
###########################################################

###########################################################
#############LOAD AFFILIATION TABLE FROM EXCEL FILE########
###########################################################

afiliaciones_total <- read.xlsx2("../clean/afiliaciones_clean.xlsx", sheetName = "Sheet1")

afiliaciones_total <- afiliaciones_total %>%
  select(PMID, afil, country, Inst, lat, long)

###########################################################
#################JOIN DATA - FULL DATAFRAME################
###########################################################
pubmed_data$PMID <- as.numeric(as.character(pubmed_data$PMID))
afiliaciones_total$PMID <- as.numeric(as.character(afiliaciones_total$PMID))
pubmed_data <- pubmed_data %>% 
  left_join(afiliaciones_total)

###FILTER - KEEP ONLY DAY DATA#####
pubmed_data <- pubmed_data %>%
  filter(Date >= FechaFiltro) 

pubmed_data$lat <- as.numeric(as.character(pubmed_data$lat))
pubmed_data$long <- as.numeric(as.character(pubmed_data$long))

###########################################################
####BINARY VARIABLES / DRUGS / ANTIVIRALS / ANTIBODIES#####
###########################################################
pubmed_data$chloroquine <- str_detect(pubmed_data$Abstract, "chloroquine")
pubmed_data$hydroxychloroquine <- str_detect(pubmed_data$Abstract, "hydroxychloroquine")
pubmed_data$remdesivir <- str_detect(pubmed_data$Abstract, "remdesivir")
pubmed_data$ritonavir <- str_detect(pubmed_data$Abstract, "ritonavir")
pubmed_data$lopinavir <- str_detect(pubmed_data$Abstract, "lopinavir")
pubmed_data$favipiravir <- str_detect(pubmed_data$Abstract, "favipiravir")
pubmed_data$azithromycin <- str_detect(pubmed_data$Abstract, "azithromycin")
pubmed_data$tocilizumab <- str_detect(pubmed_data$Abstract, "tocilizumab")
pubmed_data$interferon <- str_detect(pubmed_data$Abstract, "interferon")
pubmed_data$antibodies <- str_detect(pubmed_data$Abstract, "antibodies")
pubmed_data$vaccine <- str_detect(pubmed_data$Abstract, "vaccine")

#### Convert all to numeric ####
cols <- sapply(pubmed_data, is.logical)
pubmed_data[,cols] <- lapply(pubmed_data[,cols], as.numeric)

#### Create antivirals variable###
pubmed_data$antivirals <-   apply(X = pubmed_data[,11:18], MARGIN = 1, FUN = max, na.rm = TRUE)

####Country Controls####
pubmed_data$country <- str_replace(pubmed_data$country, "Hong Kong", "Hong Kong SAR China")
pubmed_data$country <- str_trim(pubmed_data$country)

####Join to countrys georeferences####
iso <- countrycode(unique(pubmed_data$country), "country.name", "iso2c")
country <- countrycode(unique(pubmed_data$country), "country.name", "country.name")
listado_paises <- as.data.frame(cbind(iso, country))

pubmed_data <- pubmed_data %>%
  left_join(listado_paises, by = c("country"="country"))

####Arrange Data####
pubmed_data <- pubmed_data %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir,  azithromycin, tocilizumab, antivirals, interferon, antibodies, vaccine, Inst, lat, long)

####################################################################
####################################################################
####################################################################
########AT THIS POINT I HAVE THE FINAL DAILY DATA.FRAME ############
##########I HAVE TO BIND TO THE HISTORICAL DATA ####################
####################################################################
####################################################################
####################################################################

###Historical Database####
pubmed_data_old <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
pubmed_data_old$Date <- as.Date(with(pubmed_data_old, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")

####Arrange Data####
pubmed_data_old <- pubmed_data_old %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir,  azithromycin, tocilizumab, antivirals, interferon, antibodies, vaccine, Inst, lat, long)

####Daily Backup####
write.table(pubmed_data_old, file = "../Bases/pubmed_data_old.csv", sep = "\t", qmethod = "double")


######Full Data.Base####
########################
####Historic + Daily####

pubmed_data2 <- rbind(pubmed_data_old, pubmed_data)


###############################
####Daily Papers - Control ####
###############################

pubmed_data2 %>%
  select(PMID, Title, Abstract, Date) %>%
  group_by(Date)  %>%
  summarize(cantidad=n_distinct(PMID))  %>%
  ggplot(aes(Date, cantidad)) + geom_bar(stat='identity')


pubmed_data2$country <- str_replace(pubmed_data2$country, "Côte d'Ivoire", "Ivory Coast")
pubmed_data2$country <- str_trim(pubmed_data2$country)



###########################################################
####BINARY VARIABLES / DRUGS / ANTIVIRALS / ANTIBODIES#####
###########################################################
###########################################################
###MANY PAPERS DONT HAVE ABSTRACT##########################
############# I COLLAPSE TITLE + ABSTRACT##################
###########################################################
###########################################################

pubmed_data2$TitleAbstract <- paste(pubmed_data2$Title,pubmed_data2$Abstract,sep=" /t ")
pubmed_data2$chloroquine <- str_detect(pubmed_data2$TitleAbstract, "chloroquine")
pubmed_data2$hydroxychloroquine <- str_detect(pubmed_data2$TitleAbstract, "hydroxychloroquine")
pubmed_data2$remdesivir <- str_detect(pubmed_data2$TitleAbstract, "remdesivir")
pubmed_data2$ritonavir <- str_detect(pubmed_data2$TitleAbstract, "ritonavir")
pubmed_data2$lopinavir <- str_detect(pubmed_data2$TitleAbstract, "lopinavir")
pubmed_data2$favipiravir <- str_detect(pubmed_data2$TitleAbstract, "favipiravir")
pubmed_data2$azithromycin <- str_detect(pubmed_data2$TitleAbstract, "azithromycin")
pubmed_data2$tocilizumab <- str_detect(pubmed_data2$TitleAbstract, "tocilizumab")
pubmed_data2$interferon <- str_detect(pubmed_data2$TitleAbstract, "interferon")
pubmed_data2$antibodies <- str_detect(pubmed_data2$TitleAbstract, "antibodies")
pubmed_data2$vaccine <- str_detect(pubmed_data2$TitleAbstract, "vaccine")

# Convert all to numeric
cols <- sapply(pubmed_data2, is.logical)
pubmed_data2[,cols] <- lapply(pubmed_data2[,cols], as.numeric)

#### Create antivirals variable###
pubmed_data2$antivirals <-   apply(X = pubmed_data2[,11:18], MARGIN = 1, FUN = max, na.rm = TRUE)

####Arrange Data####
pubmed_data2 <- pubmed_data2 %>%
  select(PMID, Title, Abstract, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, afil, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir,  azithromycin, tocilizumab, antivirals, interferon, antibodies, vaccine, Inst, lat, long)


####Save full database####
write.table(pubmed_data2, file = "../Bases/pubmed_data.csv", sep = "\t", qmethod = "double")

############################
####Save reduce database####
######for shiny#############
############################
pubmed_data_reduce <- pubmed_data2 %>%
  select(PMID, Title, YearPubmed, MonthPubmed, DayPubmed, Date, country, iso, chloroquine, hydroxychloroquine, remdesivir,ritonavir, lopinavir, favipiravir,azithromycin, tocilizumab, antivirals, interferon, antibodies, vaccine, Inst, lat, long) %>%
  unique()
write.table(pubmed_data_reduce, file = "../Bases/pubmed_data_reduce.csv", sep = "\t", qmethod = "double")

###################################
####Save Iberoamerican database####
######for shiny####################
###################################
pubmed_data_reduce_inst <- pubmed_data_reduce %>%
filter(lat!='') 

write.table(pubmed_data_reduce_inst, file = "../Bases/pubmed_data_reduce_inst.csv", sep = "\t", qmethod = "double")


######################################################
######################################################
########################ARMA GRAFO##################
######################################################
######################################################

####Create a list of days####
listado_dias <- pubmed_data2 %>%
  select(Date)  %>%
  unique() %>%
  arrange(Date) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  as.list() 


countries <- read_delim("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/countries.txt", "\t", escape_double = FALSE, col_types = cols(name = col_skip()), trim_ws = TRUE)

######################################
##########HISTORIAL GRAPH#############
######################################
edges_for_plot <- c()


for (dia in listado_dias$Date){
  
  print(dia)
  
  pubmed_data3 <- pubmed_data2 %>%
    filter(Date <= dia)  %>%
    arrange(Date)  %>%
    select(PMID, Date, country, iso)  %>%
    unique()
  
  #############################################################
  #######################NODES - VERTEX #######################
  #############################################################
  nodos <- pubmed_data3 %>%
    select(iso, PMID, country) %>%
    unique()  %>%
    filter(!is.na(iso)) %>%
    filter(iso!='')  %>%
    group_by(iso) %>%
    summarize(totales=n_distinct(PMID))
  
  byHand <- pubmed_data3 %>%
    select(PMID, iso, country) %>%
    unique() %>%
    filter(!is.na(iso))  %>%
    filter(iso!='')  %>%
    group_by(PMID) %>% mutate(paises = paste(iso, collapse = ","))  %>%
    select(PMID,paises) %>%
    unique()
  
  byHand$primer_pais = as.character(lapply(strsplit(as.character(byHand$paises), split=","), "[", 1))
  arreglado <- byHand %>% mutate(paises = strsplit(as.character(paises), ",")) %>% unnest(paises)
  aristas_previo <- arreglado[c(2:3)]
  
  
  colnames(aristas_previo) <- c("source","target")
  aristas <- aristas_previo %>% group_by(source, target) %>% summarize(count=n())
  
  ####CREATE NODES####
  nodes <- data.frame(id = unique(nodos$iso),label = paste(unique(nodos$iso)), value = nodos$totales, count=nodos$totales)     # size 
  
  
  #############################################################
  ############################EDGES ###########################
  #############################################################
  edges <- data.frame(source = aristas$source, target = aristas$target, weight = aristas$count)
  edges <- edges %>%
    filter(as.character(source) != as.character(target)) %>% 
    mutate(dia=as.Date(dia, origin = "1970-01-01"))
  
  countries_coord <- nodes %>%
    left_join(countries, by=c('id'='country'))
  
  ####Create Graph####
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  #g<- simplify(g, remove.multiple = TRUE)
  
  relaciones_dia  <- edges %>%
    inner_join(countries_coord %>% select(id, longitude, latitude), by = c('source' = 'id')) %>%
    rename(x = longitude, y = latitude) %>%
    inner_join(countries_coord %>% select(id, longitude, latitude), by = c('target' = 'id')) %>%
    rename(xend = longitude, yend = latitude) 
  
  ####Append daily data####
  edges_for_plot <- rbind(edges_for_plot,relaciones_dia)
}


edges_for_plot %>% 
  


####Create a dataframe with the final relations####
edges_for_plot_ud <- edges_for_plot %>%
  filter(dia == max(dia))



############################
####Save historical data####
############################
write.table(edges_for_plot, file = "../Bases/edges_for_plot_historico.csv", sep = "\t", qmethod = "double")

############################
####Save data for shiny#####
############################
write.table(edges_for_plot_ud, file = "../Bases/edges_for_plot.csv", sep = "\t", qmethod = "double")


####################################################
####################################################
####################################################
###############CONCEPTUAL MAP#######################

####Just Keep the daily papers with abstract
pubmed_reduce = pubmed_data2 %>%
  filter(Date == FechaFiltro)   %>%
  select(PMID, Abstract)   %>%
  unique() %>%
  filter(!Abstract =='')
  
  
####LEMMATIZACION######
x <- udpipe_annotate(udmodel_english, x = pubmed_reduce$Abstract, trace = TRUE, doc_id = pubmed_reduce$PMID)

########keep important variables only########
base_lemmatizada_diaria <- as.data.frame(x) %>%
  select(doc_id, paragraph_id, sentence_id, token, lemma, upos)

########Load Historical Data########
base_lemmatizada_historica <- read.table("../Bases/base_lemmatizada_total.csv", header = TRUE, sep = "\t", row.names = 1)

########Merge daily data with historical data########
base_lemmatizada_total <- rbind(base_lemmatizada_diaria, base_lemmatizada_historica)

########keep important variables only########
base_lemmatizada_total <- base_lemmatizada_total %>%
  select(doc_id, paragraph_id, sentence_id, token, lemma, upos)

########Save Processed Data base ########
write.table(base_lemmatizada_total, file = "../Bases/base_lemmatizada_total.csv", sep = "\t", qmethod = "double")
#base_lemmatizada_total <- read.table("../Bases/base_lemmatizada_total.csv", header = TRUE, sep = "\t", row.names = 1)



########################################
########################################
########## FILTER NOUNS ################
########################################
########################################

data_lemmatizada <- base_lemmatizada_total %>% 
  select(doc_id, sentence_id, lemma, upos)  %>% 
  group_by(doc_id, sentence_id) %>% 
  filter(upos %in% c('NOUN'))  %>%
  #filter(upos %in% c('NOUN','VERB'))  %>%
  summarise(text = str_c(lemma, collapse = " "))   %>% 
select(doc_id, sentence_id, text)

my_stop_words <- tibble(word = c("protected","article","copyright", "rights", "reserved", 
                                "covid", "coronavirus", "cov", "sars", "acute", "respiratory", "severe", "syndrome","sar","mer","corona","virus","covid","pneumonia","pandemic", 
                                "wuhan","hubei","china","seafood", "market","korea","province","city","december","january","february","march","april","may","june",
                                "world","health","organization",
                                "media","news",
                                "review","literature","search","database",
                                "aim","study","data"))


##########################################################
####################CREATE TOKENS#########################
##########################################################

nasa_title <- data_lemmatizada %>%
  unnest_tokens(output=word, input=text, token = "ngrams", n = 2) %>%
  unique() %>%
  mutate(idfinal = paste0(doc_id,'_',sentence_id)) %>%
  separate(word, c("word1", "word2"), sep = " ")  %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% my_stop_words$word) %>%
  #unite(word, word1, word2, sep = " ")


# create bi-grams and clean them up.
bigrams <- df %>%
  select(`Review Text`) %>%
  unnest_tokens(bigram, `Review Text`, token = "ngrams", n = 2) %>%
  filter(bigram %in% ngram_list) %>%
  separate(bigram, c("word1", "word2"), sep = " ")  




#papers_ibero <- pubmed_data2 %>%
#  filter(iso %in% c('ES','PT','BR','CO','MX','CL','PE','EC','UY')) %>%
#  select(PMID) %>%
#  unique()
#nasa_title <- nasa_title %>% inner_join(papers_ibero, by = c('doc_id'='PMID'))


###############################################################
#############CREATE CO-OCURRENCE BY DOCUMENT AND SENTENCE######
###############################################################
keyword_cors <- nasa_title %>% 
  group_by(word) %>%
  filter(n() >= 5) %>%
  #filter(n() >= 30) %>%
  pairwise_cor(word, idfinal, sort = TRUE, upper = FALSE) %>%
  unique() %>%
  filter(correlation > .02 & correlation <= 1)  %>%
  rename(weight=correlation)

#### Quantity ####
keyword_quantity <- nasa_title %>% 
  group_by(word) %>%
  summarize(size= n_distinct(doc_id))



#### weight ####
keyword_cant = keyword_cors %>%
gather(key = "topico", value = "palabra", -c(weight)) %>%
  select(palabra)  %>%
  unique() %>%
  as_tibble() %>%
  left_join(keyword_quantity, by =c('palabra'='word'))


#############################################################
##########SAVE DATA FOR SHINY SEMANTIC MAP###################
#############################################################
write.table(keyword_cors, file = "../Bases/palabras_cors.csv", sep = "\t", qmethod = "double")
write.table(keyword_cant, file = "../Bases/palabras_cant.csv", sep = "\t", qmethod = "double")



#####Create Graph#####
g <- keyword_cors %>%
      graph_from_data_frame(directed=FALSE, vertices=keyword_cant)  

#g<- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)


####VER LOS ATRIBUTOS#####
#str(vertex.attributes(g))
#str(edge.attributes(g))

####Agrega Atributos###
#g <- set_vertex_attr(g, "Degree", value = degree(g))
#g <- set_vertex_attr(g, "Intermediacion", value = betweenness(g, directed = FALSE))
#g <- set_vertex_attr(g, "size", value = keyword_cant$`n_distinct(doc_id)`)

####Cluster Algorithm####
fg <- fastgreedy.community(as.undirected(g))
rw <- cluster_walktrap(g, weights = E(g)$weight, steps = 3,merges = TRUE, modularity = TRUE, membership = TRUE)
cluster = rw$membership

#####Agregate Vertex Attribute####
g <- set_vertex_attr(g, name="group", value = cluster)



##############################################################
##########################GRAPH PRUNE#########################
##############################################################

####Minimum Spanning Tree####
min_spanning_tree <- mst(g, weights = E(g)$weight)

#str(vertex.attributes(g))
#str(edge.attributes(g))

E(g)$width <- E(g)$weight*5
V(g)$size <- sqrt(V(g)$size)*4

####Graph visualization####
visIgraph(g, type="full", layout = "layout_nicely", physics = TRUE)  %>%  
  visLegend(enabled = FALSE) %>% 
  visEdges(color = "black") %>% 
  visNodes(shape = "dot", scaling = list(min = 1, max = 40), shadow = list(enabled = TRUE, size = 20))  %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 3, hover = T),nodesIdSelection = T)  %>%
  visLayout(randomSeed = 12) # to have always the same network  




#http://uc-r.github.io/creating-text-features



#############################################################################
#############################################################################
###############GENERAL CORRECTIONS FOR DATABASE##############################
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