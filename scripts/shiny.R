library(data.table)
library(dplyr)
library(ggplot2)
#setwd('./github/covid-19/scripts')
###Levanto los datos viejos###
pubmed_data <- read.table("../Bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                              colClasses=c(Title="character", Abstract="character", country="character", afil="character"))
pubmed_data$Date <- as.Date(with(pubmed_data, paste(YearPubmed, MonthPubmed, DayPubmed,sep="-")), "%Y-%m-%d")

paises = pubmed_data %>%
  group_by(country) %>%
  summarize(n_distinct(PMID))


pubmed_data %>%
    group_by(Date)  %>%
    summarize(dia=n_distinct(PMID))  %>% 
    mutate(total = cumsum(dia))  %>% 
    ggplot(aes(x=Date, y=total)) + 
    ylab("Number of members") +
    xlab("Date") +
    geom_line(size=2, alpha=0.6) +
    geom_point(size=3, alpha=0.8) +
    geom_smooth(method = "loess", size=2, alpha=0.3) 


pubmed_data %>%
  filter(country %in% c('China','USA','Italy','Singapore')) %>%
  arrange(country, Date) %>%
  group_by(country, Date) %>%
  summarize(dia=n_distinct(PMID)) %>% 
  mutate(total = cumsum(dia))  %>% 
  ggplot(aes(x=Date, y=total, color=country)) + 
  ylab("Number of members") +
  xlab("Date") +
  geom_line(size=2, alpha=0.6) +
  geom_point(size=3, alpha=0.8) +
  geom_smooth(method = "loess", size=2, alpha=0.3) 


