library(shiny)
library(shinydashboard)
library(rCharts)
library(dygraphs)
library(leaflet)
library(dplyr)

# data
day_wise <- read.csv("day_wise.csv")
countries_long_lat <- read.csv("average-latitude-longitude-countries.csv")
worldometer_data <- read.csv("worldometer_data.csv")
colnames(countries_long_lat)[2] <- "Country.Region"
map_coordinates <- merge(countries_long_lat, worldometer_data, by = "Country.Region")
missing_coordinates <- data.frame(Country.Region=worldometer_data$Country.Region[!worldometer_data$Country.Region %in% map_coordinates$Country.Region],
                                  Latitude=c(38,60,32,51.5,24,47,49.45,7.32,37,31.56,41.50,-4.02,25,6.37,-26.30,16,7.51,35,NA,-21.07,49.37,-6.22,54,61.53,18.02,4.30,21.8,13.15,18.05,12.12,-8.55,18,12.20,-51.56,41.90),
                                  Longitude=c(-97,100,54,-0.13,54,29,15.30,-5.54,127.30,35.13,22,21.45,17,20.56,31.30,-24,29.41,38,NA,55.31,-2.36,34.53,-4.46,-6.58,-63.04,114.40,-72.3,-61.11,-63.03,-68.56,125.56,105,-68.26,-59.82,12.45),
                                  TotalCases=c(5032179,871894,320117,308134,61845,26628,17731,16447,14519,13398,11399,9309,4879,4620,2968,2734,2450,999,NA,671,597,509,336,266,160,141,129,56,53,31,25,20,13,13,12))
missing_coordinates <- missing_coordinates %>% na.omit()
map_coordinates <- map_coordinates %>% select(Country.Region,Latitude,Longitude,TotalCases)
map_coordinates <- rbind(map_coordinates,missing_coordinates)
countries <- unique(worldometer_data$Country.Region)
continents <- unique(worldometer_data$Continent)


# ui.r
header <- dashboardHeader(title = " ")

side <- dashboardSidebar(selectInput('country', 'Country/region', countries),
                         selectInput('continent', 'Continent', continents, selected = "Europe"),br(),p(),
                         br(),p(),
                         div(img(src="covid19disease.jpg")),br(),p("Author:"),
                         a(href= "https://stackoverflow.com/users/12382064/kaczdr",icon("stack-overflow", "fa-2x")),
                         a(href= "https://linkedin.com/in/kacper-zdrojewski-3b2043245",icon("linkedin", "fa-2x")),br(),br(),p("Data source:"),
                         div(a(href="https://worldometers.info/coronavirus",img(src="worldometers-fb.jpg"))),br(),
                         div(a(href="https://github.com/CSSEGISandData/COVID-19",img(src="jhu.png")))
)

body <- dashboardBody(
    includeCSS("custom.css"),
    fixedRow(
        column(width = 8, div(class="col-md-6" ,h2("Covid-19 pandemic analysis")))
    ),
    fixedRow(
        box(status = "info", width = 5, title = "Deaths and Recovered Cases Timeline", 
            solidHeader = TRUE,collapsible = TRUE,dygraphOutput("dygraph")),
        box(status = "info", width = 7, title = "Reported Total Cases by Continent", 
            solidHeader = TRUE,collapsible = TRUE,showOutput("Chart2", "morris"))
    ),
    fixedRow(
        column(width = 4,box(title = "Total Cases by Country", status = "warning",solidHeader = TRUE,plotOutput("plot2", height = "1px"),showOutput("Chart3", "nvd3"),htmlOutput("other"),width = 12)),
        column(width = 4,box(title = "Country-specific pandemic statistics", status = "warning",solidHeader = TRUE,plotOutput("plot1", height = "1px"),showOutput("Chart4", "HighCharts"),width = 12)),
        column(width = 4,box(title = "Map of Covid-19 Total Cases", status = "danger",solidHeader = TRUE,leafletOutput("map"),width = 12))
    )
)

ui <- dashboardPage(header, side, body) 

# server.r
server <- function(input, output, session) {
    
    continentData <- reactive({
        selectedContinent <- worldometer_data[worldometer_data$Continent == input$continent, ]
        if(input$continent == "North America")
            selectedContinent <- selectedContinent %>% filter(Continent=="North America") %>% filter(row_number() <= 0.5 * n())
        else if(input$continent == "Asia")
            selectedContinent <- selectedContinent %>% filter(Continent=="Asia") %>% filter(row_number() <= 0.5 * n())   
        else if(input$continent == "Europe")
            selectedContinent <- selectedContinent %>% filter(Continent=="Europe") %>% filter(row_number() <= 0.5 * n()) 
        else if(input$continent == "Africa")
            selectedContinent <- selectedContinent %>% filter(Continent=="Africa") %>% filter(row_number() <= 0.5 * n())
        return(selectedContinent)
    })
    
    countryData <- reactive({
        selectedCountry <- worldometer_data[worldometer_data$Country.Region == input$country, ]
        return(selectedCountry)
    })
    
    output$Chart2 <- renderChart({ 
        b <- worldometer_data %>% filter(Continent != "") %>% group_by(Continent) %>% summarise(TotalCases = sum(TotalCases))
        n1 <- mPlot(TotalCases ~ Continent, data = b, type = "Bar")
        n1$set(hideHover = "auto", dom = "Chart2") 
        #n1$set(width = 900)
        #n1$params$width <- 900
        return(n1)
    })
    
    output$dygraph <- renderDygraph({
        dayWiseCases <- cbind(day_wise$Deaths,day_wise$Recovered)
        colnames(dayWiseCases) <- c("Deaths","Recovered")
        rownames(dayWiseCases) <- format(as.Date(day_wise$Date), "%Y-%m-%d")
        dayWiseCases[,1:2] <- sapply(dayWiseCases[,1:2], as.numeric)
        dygraph(dayWiseCases) %>%
            dyAxis("y",axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}') %>%
            dyRangeSelector() %>%
            dyOptions(maxNumberWidth = 9468087) %>%
            dyLegend(width = 210)  
        
    })
    
    output$Chart3 <- renderChart2({
        nn <- nPlot(TotalCases ~ Country.Region, group = "Continent", data = continentData(), type = "multiBarHorizontalChart")
        nn$chart(
            color=c("grey","blue"),
            margin=list(left=150,right=50),
            showControls=FALSE
        )
        nn$set(width = session$clientData$output_plot2_width)
        return(nn)
    })
    
    output$other <- renderUI({
        p1 <- paste("Total Deaths: ",formatC(sum(continentData()$TotalDeaths,na.rm = T),format="d",big.mark=","))
        p2 <- paste("Total Recovered: ",formatC(sum(continentData()$TotalRecovered,na.rm = T),format="d",big.mark=","))
        p3 <- paste("Total Tests: ",formatC(sum(continentData()$TotalTests,na.rm = T),format="d",big.mark=","))
        HTML(paste(p1, p2,p3, sep = '<br/>'))
    })
    
    output$Chart4 <- renderChart2({
        dat <- data.frame(key = colnames(countryData())[c(6,8,10)], value = c(countryData()$TotalDeaths,countryData()$TotalRecovered,countryData()$ActiveCases))
        h1 <- hPlot(x = "key", y = "value", data = dat, type = "pie", title = countryData()$Country.Region)
        h1$set(width = session$clientData$output_plot1_width)
        return(h1)
    })
    
    output$map <- renderLeaflet({
        leaflet(map_coordinates) %>% addTiles() %>%
            fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) %>% addCircleMarkers(
                data = map_coordinates,
                lat = map_coordinates$Latitude,
                lng = map_coordinates$Longitude,
                radius = ~map_coordinates$TotalCases/100000, popup = paste0(map_coordinates$Country.Region)
            )
    })
    
}

shinyApp(ui = ui, server = server)
