#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shinythemes)
library(plotly)

weather_data = read.csv("KCI_Weather.csv")
#Check Data
summary(weather_data)

#Remove any variables that have over 2yrs of NA or missing data
weather_data = weather_data %>% 
  select_if(function(x) sum(is.na(x))<730)

#Remove any variable that is not numeric and while not NA has over 2yrs of empty data
weather_data = weather_data %>% 
  select_if(function(x) is.numeric(x) |sum((x==''))<730)

#For the attributes columns I only need any value before first comma
weather_data$PRCP_ATTRIBUTES = 
  substr(weather_data$PRCP_ATTRIBUTES,1,regexpr(",",weather_data$PRCP_ATTRIBUTES)-1)

weather_data$SNOW_ATTRIBUTES = 
  substr(weather_data$SNOW_ATTRIBUTES,1,regexpr(",",weather_data$SNOW_ATTRIBUTES)-1)

weather_data$SNWD_ATTRIBUTES = 
  substr(weather_data$SNWD_ATTRIBUTES,1,regexpr(",",weather_data$SNWD_ATTRIBUTES)-1)

weather_data$TMAX_ATTRIBUTES = 
  substr(weather_data$TMAX_ATTRIBUTES,1,regexpr(",",weather_data$TMAX_ATTRIBUTES)-1)

weather_data$TMIN_ATTRIBUTES = 
  substr(weather_data$TMIN_ATTRIBUTES,1,regexpr(",",weather_data$TMIN_ATTRIBUTES)-1)


#Remove the TMAX and TMIN attributes as they don't have any data after cleaning
weather_data = weather_data %>% 
  select_if(function(x) is.numeric(x) |sum((x==''))<18262)


#Convert chr for date to date

weather_data = weather_data %>% 
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

#Change NA for SNOW and SNWD to 0
weather_data[is.na(weather_data)] = 0

#Add separate values for DAY, YEAR, MONTH to data
weather_data = weather_data %>% 
  mutate(year = lubridate::year(DATE),
         month = lubridate::month(DATE),
         day = lubridate::day(DATE))

#Create Options for shiny
Weather = c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")




library(shiny)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("KC Weather"),
                
                
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "Weather", label = strong("Weather Interest"),
                                choices = unique(Weather),
                                selected = "TMAX"),
                    # Select date range to be plotted
                    dateRangeInput("DATE", strong("Date range"), start = "1973-11-29", end = "2022-11-28",
                                   min = "1973-11-29", max = "2022-11-28"),
                    br(),
                    #Select Day and Month to calculate averages over last 50yrs weather wise
                    numericInput("Month_avg", strong("Month for Average:"), 1, min = 1, max = 12),
                    numericInput("Day_avg", strong("Day for Averageage:"), 1, min = 1, max = 31),
                    
                  )
                ,
                # Output: Description, lineplot, and reference
                mainPanel(
                  tabsetPanel(
                    tabPanel("Basic Stats",
                  fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), plotlyOutput(outputId = "Densityplot", height = "350px"),
                              plotOutput(outputId = "Dotplot", height = "350px"))
                  ),
                  fluidRow(
                    plotlyOutput(outputId = "Scatterplot", height = "700px"))
                    ),
                  tabPanel("Averages", tableOutput("averages"))
                  )
               )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_data <- reactive({
    Weather  = switch(input$Weather,
                      'PRCP' = 'PRCP',
                      'SNOW' = "SNOW",
                      'SNWD' = "SNWD",
                      'TMAX' = "TMAX",
                      'TTMIN' = "TMIN")
    req(input$DATE)
    validate(need(!is.na(input$DATE[1]) & !is.na(input$DATE[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$DATE[1] < input$DATE[2], "Error: Start date should be earlier than end date."))
    weather_data %>%
      filter(
        DATE > as.POSIXct(input$DATE[1]) & DATE < as.POSIXct(input$DATE[2]
        ))
  })
  
  output$Scatterplot <- renderPlotly({
      ggplotly(
        ggplot(selected_data(),aes_string(x= "DATE", y = input$Weather)) +
          geom_point()+
          geom_smooth(method = loess, span =.1)
      )
  })
  output$Dotplot <- renderPlot({
  ggplot(selected_data(),aes_string(x = input$Weather)) +
    geom_dotplot(binwidth = .5, fill = "Sky Blue")
  })
  
  output$Densityplot <- renderPlotly({
    ggplotly(
      ggplot(selected_data(),aes_string(x = input$Weather)) +
      geom_density(size = .1))
  })
  
  output$averages <- renderTable({
  PRCP_avg =  weather_data %>% 
      filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarize('Avg_Preciptation' = mean(PRCP))
  
  
  SNOW_avg =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('Avg_Snow' = mean(SNOW))
  
  SNWD_avg =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('Avg_Snow_Depth' = mean(SNWD))
  
  TMAX_avg =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('Avg_High_Temp' = mean(TMAX))
  
  TMIN_avg =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('Avg_Low_Temp' = mean(TMIN))
  
  avg_table = data.frame(PRCP_avg, SNOW_avg, SNWD_avg, TMAX_avg, TMIN_avg)
  })

} 

# Create Shiny object
shinyApp(ui = ui, server = server)
