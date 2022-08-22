library(shiny)
runGitHub( "death-rate-by-country-with-Shiny", "zehraa8")
library(shinythemes) #for using default themes
setwd("C:/Users/User/Desktop/GitHub Çalışmalarım/deathrate/")
data<-read.csv("csvData.csv",sep =",")
# install.packages("countrycode")
library(countrycode) #for matching countries with their continents
library(highcharter)
library(tidyverse)
data$continent <- countrycode(sourcevar = data[, "country"],
                            origin = "country.name",
                            destination = "continent")
data$continent[is.na(data$continent)] = "Oceania"

colnames(data)<-c("country","Crude Death Rate (per 1k)","Death Rate (per 100k)","Total Deaths","continent")

ui <- fluidPage(theme = shinytheme("united"),

    titlePanel("Death Rate by Country"),

    
    fluidRow(
          column(4,selectizeInput("select1", "Choose one or more continent(s)", choices=data$continent, selected = NULL, multiple = T,
                         options = NULL)),
          column(4,sliderInput(
            "select2",
            "Choose the interval of number of total deaths",
            value = c(min(data$`Total Deaths`),max(data$`Total Deaths`)),
            min = min(data$`Total Deaths`),
            max = max(data$`Total Deaths`),
            step = 1
          )),
          column(4,radioButtons("select3", "Variable:",
                       c("Crude Death Rate (per 1k)" = "Crude Death Rate (per 1k)",
                         "Death Rate (per 100k)" = "Death Rate (per 100k)",
                         "Total Deaths"="Total Deaths")),
        ),
        

        # Show a plot of the generated distribution
        fluidRow(
           column(12,highchartOutput("map"))
        ),
        br(),
        helpText(a("Click Here to Download Data",href="https://worldpopulationreview.com/country-rankings/death-rate-by-country")
        )
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  my_theme <- hc_theme(
    chart = list(
      backgroundColor = "white",
      style = list(
        fontFamily = "Montserrat"
      )
    )
  )
    output$map <- renderHighchart({

      selectedData <- filter(data,continent %in% input$select1)
      selectedData<-selectedData %>% filter(`Total Deaths`>input$select2[1],`Total Deaths`<input$select2[2])
  
      highchart() %>%
        hc_add_series_map(
          worldgeojson, selectedData, value = input$select3, joinBy = c("name","country"),
          name = input$select3
        )  %>%
        hc_colorAxis(stops = color_stops()) %>%
        hc_title(text = paste("Worldwide", input$select3)) %>%
        hc_subtitle(text = "According to Countries") %>% hc_add_theme(my_theme)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


