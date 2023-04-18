library(shiny)
library(shinydashboard)
library(rCharts)
library(dygraphs)
library(dplyr)

# data
country_wise_latest <- read.csv("country_wise_latest.csv")
day_wise <- read.csv("day_wise.csv")
worldometer_data <- read.csv("worldometer_data.csv")
countries <- unique(worldometer_data$Country.Region)
continents <- unique(worldometer_data$Continent)


# ui.r
header <- dashboardHeader(title = " ")

side <- dashboardSidebar(selectInput('country', 'Country/region', countries),
                         selectInput('continent', 'Continent', continents, selected = "Europe"),br(),p(),
                         # add slider for months
                         br(),p(),
                         div(img(src="covid19disease.jpg")),br(),p("Author:"),
                         a(href= "https://stackoverflow.com/users/12382064/kaczdr",icon("fa fa-stack-overflow", "fa-2x")),
                         a(href= "https://linkedin.com/in/kacper-zdrojewski-3b2043245",icon("fa fa-linkedin-square", "fa-2x")),br(),br(),p("Data source:"),
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
        column(width = 4,box(title = "Total Cases by Country", status = "warning",solidHeader = TRUE,showOutput("Chart3", "nvd3"),htmlOutput("other"),width = 12)),
        column(width = 4,box(title = "Country-specific pandemic statistics", status = "warning",solidHeader = TRUE,showOutput("Chart4", "HighCharts"),width = 12))
    )
)

ui <- dashboardPage(header, side, body) 

# server.r
server <- function(input, output) {
    
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
    
    output$Chart2 <- renderChart2({ 
        b <- worldometer_data %>% filter(Continent != "") %>% group_by(Continent) %>% summarise(TotalCases = sum(TotalCases))
        n1 <- mPlot(TotalCases ~ Continent, data = b, type = "Bar")
        #n1$set(width = 900)
        n1$params$width <- 900
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
            margin=list(left=150),
            showControls=FALSE,
            width = 400
        )
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
        h1$chart(
            width = 500
        )
        return(h1)
    })
    
    # interaktywny summary z danych
}

shinyApp(ui = ui, server = server)
