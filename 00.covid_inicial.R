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







####Busqueda en PubMed####
search_topic <- 'COVID-19'
search_query <- EUtilsSummary(search_topic, retmax=2000, mindate=2020,maxdate=2021, db='pubmed')
summary(search_query)

# see the ids of our returned query
QueryId(search_query)

# get actual data from PubMed
records<- EUtilsGet(search_query)

####Fecha del primer paper###
FechaFiltro = "2020-01-17"

# Save an object to a file
#saveRDS(records, file = "../records/pubmed.rds")
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


###Procesar las afiliaciones
afil = sapply(Afiliaciones, paste, collapse=" ; ")
publicaciones = cbind(pubmed_data, afil)

#######Crea Tabla afiliaciones - Id ####
afiliaciones = publicaciones %>% 
  select(PMID, afil) %>% 
  separate_rows(afil, sep = " ; " , convert = TRUE)

afiliaciones$country <- sub('.*,\\s*', '', afiliaciones$afil)


#####ESTO ES UN CLEAN MANUAL PARA LOS CASOS QUE NO LOGRAN ENCONTRAR####
write.xlsx2(afiliaciones, '../clean/afiliaciones_total.xlsx', sheetName="Sheet1",
            col.names=TRUE, row.names=FALSE, append=FALSE)


###LOS CASOS QUE NO SE PUDIERON IDENTIFICAR DEBEN SER CLASIFICADOS DESDE EL EXCEL###

###Levanto nuevamente los datos###
###ESTA ES LA BASE CLEANEADA#####
#afiliaciones_total <- read_delim("C:/Users/Juan/Dropbox/afiliaciones_total.xlsx", ";", escape_double = FALSE, locale = locale(encoding = "ASCII"), trim_ws = TRUE)
afiliaciones_total <- read.xlsx2("../clean/afiliaciones_total.xlsx", sheetName = "Sheet1")

####BaseCompletaPapers###
pubmed_data$PMID <- as.numeric(as.character(pubmed_data$PMID))
pubmed_data <- pubmed_data %>% 
  #right_join(papers_america_latina) %>% 
  #left_join(afiliaciones) %>% 
  left_join(afiliaciones_total)

###Filtro#####
pubmed_data <- pubmed_data %>%
  filter(Date >= FechaFiltro) 

####Agrego terminos importantes####
pubmed_data$chloroquine <- str_detect(pubmed_data$Abstract, "chloroquine")
pubmed_data$remdesivir <- str_detect(pubmed_data$Abstract, "remdesivir")

write.xlsx2(pubmed_data, './pubmed_data.xlsx', sheetName="Sheet1",
            col.names=TRUE, row.names=FALSE, append=FALSE)


pubmed_data2 <- read.xlsx2("../bases/pubmed_data.xlsx", sheetName = "Sheet1")











lista = names(mesh)
vacioTotal <- c()
vacioTotal <- data.frame(palabra=mesh['21']$`21`$Heading, descriptor=mesh['21']$`21`$Type, id=21)

vacio <- data.frame(palabra=mesh['27']$`27`$Heading, descriptor=mesh['27']$`27`$Type, id=27)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['32']$`32`$Heading, descriptor=mesh['32']$`32`$Type, id=32)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['51']$`51`$Heading, descriptor=mesh['51']$`51`$Type, id=51)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['55']$`55`$Heading, descriptor=mesh['55']$`55`$Type, id=55)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['176']$`176`$Heading, descriptor=mesh['176']$`176`$Type, id=176)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['199']$`199`$Heading, descriptor=mesh['199']$`199`$Type, id=199)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['260']$`260`$Heading, descriptor=mesh['260']$`260`$Type, id=260)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['262']$`262`$Heading, descriptor=mesh['262']$`262`$Type, id=262)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['294']$`294`$Heading, descriptor=mesh['294']$`294`$Type, id=294)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['296']$`296`$Heading, descriptor=mesh['296']$`296`$Type, id=296)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['331']$`331`$Heading, descriptor=mesh['331']$`331`$Type, id=331)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['348']$`348`$Heading, descriptor=mesh['348']$`348`$Type, id=348)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['350']$`350`$Heading, descriptor=mesh['350']$`350`$Type, id=350)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['351']$`351`$Heading, descriptor=mesh['351']$`351`$Type, id=351)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['352']$`352`$Heading, descriptor=mesh['352']$`352`$Type, id=352)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['353']$`353`$Heading, descriptor=mesh['353']$`353`$Type, id=353)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['355']$`355`$Heading, descriptor=mesh['355']$`355`$Type, id=355)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['377']$`377`$Heading, descriptor=mesh['377']$`377`$Type, id=377)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['378']$`378`$Heading, descriptor=mesh['378']$`378`$Type, id=378)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['380']$`380`$Heading, descriptor=mesh['380']$`380`$Type, id=380)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['392']$`392`$Heading, descriptor=mesh['392']$`392`$Type, id=392)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['414']$`414`$Heading, descriptor=mesh['414']$`414`$Type, id=414)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['415']$`415`$Heading, descriptor=mesh['415']$`415`$Type, id=415)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['431']$`431`$Heading, descriptor=mesh['431']$`431`$Type, id=431)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['432']$`432`$Heading, descriptor=mesh['432']$`432`$Type, id=432)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['469']$`469`$Heading, descriptor=mesh['469']$`469`$Type, id=469)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['483']$`483`$Heading, descriptor=mesh['483']$`483`$Type, id=483)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['493']$`493`$Heading, descriptor=mesh['493']$`493`$Type, id=493)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['496']$`496`$Heading, descriptor=mesh['496']$`496`$Type, id=496)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['498']$`498`$Heading, descriptor=mesh['498']$`498`$Type, id=498)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['499']$`499`$Heading, descriptor=mesh['499']$`499`$Type, id=499)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['517']$`517`$Heading, descriptor=mesh['517']$`517`$Type, id=517)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['518']$`518`$Heading, descriptor=mesh['518']$`518`$Type, id=518)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['521']$`521`$Heading, descriptor=mesh['521']$`521`$Type, id=521)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['522']$`522`$Heading, descriptor=mesh['522']$`522`$Type, id=522)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['527']$`527`$Heading, descriptor=mesh['527']$`527`$Type, id=527)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['532']$`532`$Heading, descriptor=mesh['532']$`532`$Type, id=532)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['543']$`543`$Heading, descriptor=mesh['543']$`543`$Type, id=543)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['548']$`548`$Heading, descriptor=mesh['548']$`548`$Type, id=548)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['558']$`558`$Heading, descriptor=mesh['558']$`558`$Type, id=558)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['559']$`559`$Heading, descriptor=mesh['559']$`559`$Type, id=559)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['560']$`560`$Heading, descriptor=mesh['560']$`560`$Type, id=560)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['567']$`567`$Heading, descriptor=mesh['567']$`567`$Type, id=567)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['568']$`568`$Heading, descriptor=mesh['568']$`568`$Type, id=568)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['569']$`569`$Heading, descriptor=mesh['569']$`569`$Type, id=569)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['576']$`576`$Heading, descriptor=mesh['576']$`576`$Type, id=576)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['577']$`577`$Heading, descriptor=mesh['577']$`577`$Type, id=577)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['578']$`578`$Heading, descriptor=mesh['578']$`578`$Type, id=578)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['579']$`579`$Heading, descriptor=mesh['579']$`579`$Type, id=579)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['589']$`589`$Heading, descriptor=mesh['589']$`589`$Type, id=589)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['598']$`598`$Heading, descriptor=mesh['598']$`598`$Type, id=598)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['1100']$`1100`$Heading, descriptor=mesh['1100']$`1100`$Type, id=1100)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['2103']$`2103`$Heading, descriptor=mesh['2103']$`2103`$Type, id=2103)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['2126']$`2126`$Heading, descriptor=mesh['2126']$`2126`$Type, id=2126)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['2132']$`2132`$Heading, descriptor=mesh['2132']$`2132`$Type, id=2132)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['2152']$`2152`$Heading, descriptor=mesh['2152']$`2152`$Type, id=2152)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['2153']$`2153`$Heading, descriptor=mesh['2153']$`2153`$Type, id=2153)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['2157']$`2157`$Heading, descriptor=mesh['2157']$`2157`$Type, id=2157)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['3108']$`3108`$Heading, descriptor=mesh['3108']$`3108`$Type, id=3108)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['3116']$`3116`$Heading, descriptor=mesh['3116']$`3116`$Type, id=3116)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['3142']$`3142`$Heading, descriptor=mesh['3142']$`3142`$Type, id=3142)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['3154']$`3154`$Heading, descriptor=mesh['3154']$`3154`$Type, id=3154)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['3155']$`3155`$Heading, descriptor=mesh['3155']$`3155`$Type, id=3155)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['3157']$`3157`$Heading, descriptor=mesh['3157']$`3157`$Type, id=3157)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4100']$`4100`$Heading, descriptor=mesh['4100']$`4100`$Type, id=4100)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4115']$`4115`$Heading, descriptor=mesh['4115']$`4115`$Type, id=4115)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4130']$`4130`$Heading, descriptor=mesh['4130']$`4130`$Type, id=4130)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4131']$`4131`$Heading, descriptor=mesh['4131']$`4131`$Type, id=4131)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4165']$`4165`$Heading, descriptor=mesh['4165']$`4165`$Type, id=4165)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4191']$`4191`$Heading, descriptor=mesh['4191']$`4191`$Type, id=4191)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4199']$`4199`$Heading, descriptor=mesh['4199']$`4199`$Type, id=4199)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['4200']$`4200`$Heading, descriptor=mesh['4200']$`4200`$Type, id=4200)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5101']$`5101`$Heading, descriptor=mesh['5101']$`5101`$Type, id=5101)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5103']$`5103`$Heading, descriptor=mesh['5103']$`5103`$Type, id=5103)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5110']$`5110`$Heading, descriptor=mesh['5110']$`5110`$Type, id=5110)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5111']$`5111`$Heading, descriptor=mesh['5111']$`5111`$Type, id=5111)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5112']$`5112`$Heading, descriptor=mesh['5112']$`5112`$Type, id=5112)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5114']$`5114`$Heading, descriptor=mesh['5114']$`5114`$Type, id=5114)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5115']$`5115`$Heading, descriptor=mesh['5115']$`5115`$Type, id=5115)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5116']$`5116`$Heading, descriptor=mesh['5116']$`5116`$Type, id=5116)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5122']$`5122`$Heading, descriptor=mesh['5122']$`5122`$Type, id=5122)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5139']$`5139`$Heading, descriptor=mesh['5139']$`5139`$Type, id=5139)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5140']$`5140`$Heading, descriptor=mesh['5140']$`5140`$Type, id=5140)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5146']$`5146`$Heading, descriptor=mesh['5146']$`5146`$Type, id=5146)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5149']$`5149`$Heading, descriptor=mesh['5149']$`5149`$Type, id=5149)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5151']$`5151`$Heading, descriptor=mesh['5151']$`5151`$Type, id=5151)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5152']$`5152`$Heading, descriptor=mesh['5152']$`5152`$Type, id=5152)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5153']$`5153`$Heading, descriptor=mesh['5153']$`5153`$Type, id=5153)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5160']$`5160`$Heading, descriptor=mesh['5160']$`5160`$Type, id=5160)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5173']$`5173`$Heading, descriptor=mesh['5173']$`5173`$Type, id=5173)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5182']$`5182`$Heading, descriptor=mesh['5182']$`5182`$Type, id=5182)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5183']$`5183`$Heading, descriptor=mesh['5183']$`5183`$Type, id=5183)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5185']$`5185`$Heading, descriptor=mesh['5185']$`5185`$Type, id=5185)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5186']$`5186`$Heading, descriptor=mesh['5186']$`5186`$Type, id=5186)
vacioTotal <- rbind(vacioTotal,vacio)
vacio <- data.frame(palabra=mesh['5195']$`5195`$Heading, descriptor=mesh['5195']$`5195`$Type, id=5195)
vacioTotal <- rbind(vacioTotal,vacio)
                    

Palabras <- vacioTotal %>%
  group_by(palabra) %>%
  summarize(cantidad=n_distinct(id))
View(Palabras)

Palabras2 <- vacioTotal %>%
  group_by(palabra, descriptor) %>%
  summarize(n_distinct(id))

#install.packages("wordcloud")
library(wordcloud)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("RColorBrewer")
library(RColorBrewer)

set.seed(1977) # for reproducibility 
wordcloud(words = Palabras$palabra, freq = Palabras$cantidad, min.freq = 3, max.words=100, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




write.csv(Palabras, 'palabras.csv')


###Grafico####
afiliaciones2 %>%
  filter(country %in% c('China.', 'USA.', 'UK.', 'Italy.', 'Singapore', 'Canada.', 'France.', 'Germany.', 'Switzerland.', 'Brazil.')) %>%
  group_by(country, date) %>%
  count(n_distinct(PMID)) %>%
  ggplot(aes(x=date, y=n, group=country, color=country)) + geom_line() + geom_smooth() +
  facet_wrap(~ country)


###Extrae Palabras####
abstractsOnly<-as.character(pubmed_data$Abstract)
abstractsOnly<-paste(abstractsOnly, sep="", collapse="")
abstractsOnly<-as.vector(abstractsOnly)
abstractsOnly<-strip(abstractsOnly)
stsp<-rm_stopwords(abstractsOnly, stopwords = qdapDictionaries::Top100Words)
ord<-as.data.frame(table(stsp))
ord<-ord[order(ord$Freq, decreasing=TRUE),]
head(ord,100)
View(ord)



