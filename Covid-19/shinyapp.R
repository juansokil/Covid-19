
library(data.table)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)  
#install.packages("countrycode")
library(countrycode)
library(stringr)

rsconnect::setAccountInfo(name='juanpablosokil', 
                          token='7499F5689D7DC0540DB1D96DCC05DB0F', 
                          secret='YanlwsVRMkrfX3dy3tAXnHttmNZh1lcXZME/IISR')


#setwd('./github/covid-19/scripts')
###Levanto los datos viejos###
pubmed_data <- read.table("https://raw.githubusercontent.com/juansokil/Covid-19/master/bases/pubmed_data.csv", header = TRUE, sep = "\t", row.names = 1,
                          colClasses=c(Title="character", Abstract="character", country="character", afil="character", Date="Date"))



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


ddd = pubmed_data3 %>%
    group_by (country, iso) %>%
    summarize (n_distinct(PMID))
View(ddd)


server <- function(input, output, session) {
    
    pais <- reactive({
        a <- pubmed_data %>% filter(country %in% input$country)
        a <- data.frame(a)
        return(a)
    })    
    
    
    
    
    output$plot1 <- renderPlot({pubmed_data %>%
        group_by(Date)  %>%
        summarize(dia=n_distinct(PMID))  %>% 
        mutate(total = cumsum(dia))  %>% 
        ggplot(aes(x=Date, y=total)) + 
        ylab("total de papers") +
        xlab("Date") +
        geom_line(size=2, alpha=0.6) +
        geom_point(size=3, alpha=0.8) +
        geom_smooth(method = "loess", size=2, alpha=0.3)   +  
            theme_bw()
    })
    
    

    output$plot3 <- renderPlot({pubmed_data %>%
            group_by(Date)  %>%
            summarize(dia=n_distinct(PMID))  %>% 
            ggplot(aes(x=Date, y=dia)) + 
            ylab("Papers x Dia") +
            xlab("Date") +
            geom_line(size=2, alpha=0.6) +
            geom_point(size=3, alpha=0.8) +
            geom_smooth(method = "loess", size=2, alpha=0.3)   +  
            theme_bw()
    })
    output$table1 <- renderDT(pubmed_data %>%
            group_by(Date)  %>%
            summarize(Dia=n_distinct(PMID))  %>% 
            mutate(Acumulado = cumsum(Dia)),  
            options = list(pageLength = 10))
            
    

    output$plot2 <- renderPlot({
        pais() %>%
            arrange(country, Date) %>%
            group_by(country, Date) %>%
            summarize(dia=n_distinct(PMID)) %>% 
            mutate(total = cumsum(dia))  %>% 
            ggplot(aes(x=Date, y=total, color=country)) + 
            ylab("Comparar paises") +
            xlab("Date") +
            geom_line(size=2, alpha=0.6) +
            geom_point(size=3, alpha=0.8) +
            #geom_smooth(method = "loess", size=2, alpha=0.3)  +  
            theme_bw()
    })
    
    
    map <- ggplot(nodes) + country_shapes +
        geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
                   data = edges_for_plot, curvature = 0.33,
                   alpha = 0.5) +
        geom_point(aes(x = lon, y = lat),           # draw nodes
                   shape = 21, fill = 'white',
                   color = 'black', stroke = 0.5, size=4) +
        scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
        geom_text(aes(x = lon, y = lat, label = CountryCode),             # draw text labels
                  hjust = 0, nudge_x = 1, nudge_y = 4,
                  size = 6, color = "red", fontface = "bold")
    
    
}



# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Aplicacion ejemplo"),
    #sidebarLayout(pickerInput(inputId = "country", label = "Seleccione los topicos",  choices = unique(pubmed_data$country), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE),
                  # Show a plot of the generated distribution
                  mainPanel(
                      tabsetPanel(type = "tabs",
                               #tabPanel("Total", plotOutput(outputId = "plot1"),dataTableOutput(outputId = "table1")),
                               tabPanel("Totales",fluidRow(column(6, plotOutput("plot1")),column(6, plotOutput("plot3"))),fluidRow(column(12, dataTableOutput(outputId = "table1")))),
                               tabPanel("Paises",fluidRow(column(12, (pickerInput(inputId = "country", label = "Seleccione los topicos",  
                                                                                   choices = unique(pubmed_data$country), options = list('actions-box' = TRUE, size = 8,'selected-text-format' = "count > 3",'deselect-all-text' = "Ninguno", 'select-all-text' = "Todos",'none-selected-text' = "Sin Seleccion",'count-selected-text' = "{0} seleccionados."), multiple = TRUE))),
                                ), fluidRow(column(12, plotOutput(outputId = "plot2",height = "400px")),
                                )
                        )
    )))






shinyApp(ui, server)




