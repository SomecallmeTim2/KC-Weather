#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(plotly)
library(shiny)

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






# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("KC Weather"),
                helpText("Last 50 years from KCI Airport"),
                
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("TMAX = High Temp"),
                    helpText("TMIN = Low Temp"),
                    helpText("PRCP = Water equivalent precipitation"),
                    helpText("SNOW = Snow accumilation"),
                    helpText("SNWD = Snow Depth"),
                    # Select type of trend to plot
                    selectInput(inputId = "Weather", label = strong("Weather Interest:"),
                                choices = unique(Weather),
                                selected = "TMAX"),
              
                    # Select date range to be plotted
                    dateRangeInput("DATE", strong("Date range for Plots: (Min = 1973-11-29, Max = 2022-11-28)"), start = "2012-11-29", end = "2022-11-28",
                                   min = "1973-11-29", max = "2022-11-28"),
                    helpText("**Takes about 30 seconds if looking at 50yrs**"),
                    #Select Day and Month to calculate averages over last 50yrs weather wise
                    numericInput("Month_avg", strong("Month for Averages: (Min = 1, Max = 12)"), 1, min = 1, max = 12),
                    numericInput("Day_avg", strong("Day for Averages: (Min = 1, Max = 31)"), 1, min = 1, max = 31),
                    numericInput("Year_avg", strong("Year for Averages: (Min = 1973, Max = 2021)"), 1973, min = 1973, max = 2021 ),
                  )
                ,
                # Output: Description, lineplot, and reference
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plots",
                  fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "Densityplot", height = "350px"),
                              plotOutput(outputId = "Dotplot", height = "350px"))
                  ),
                  fluidRow(
                    plotlyOutput(outputId = "Scatterplot", height = "650px"))
                  ),
                  tabPanel("Averages/Calculations", tableOutput("averages"),
                           fluidRow(
                             plotlyOutput(outputId = "Scatteryear", height = "650px"))
                  
                  )
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
          geom_smooth(method = loess)
      )
  })
  output$Dotplot <- renderPlot({
  ggplot(selected_data(),aes_string(x = input$Weather)) +
    geom_dotplot(binwidth = .5, fill = "Sky Blue")
  })
  
  output$Densityplot <- renderPlot({
      ggplot(selected_data(),aes_string(x = input$Weather)) +
      geom_density(linewidth = 1)
  })
  
  output$Scatteryear <- renderPlotly({
    ggplotly(
      weather_data %>% 
        filter(year > 1972 & year < 2022) %>%
        group_by(year) %>% 
        summarise("Avg" = mean(get(input$Weather))) %>%
      ggplot(aes_string(x= "year", y = "Avg")) +
        geom_point()+
       geom_smooth(method = loess)
    )
  })
  
  
  output$averages <- renderTable({
    
  W_Type = c("50yr max",
             "50yr min",
            "50yr avg chosen day",
            "50yr % over chosen day",
            "Max over chosen day",
            "Min over chosen day",
             "Avg over chosen year", 
             "% of days over chosen year",
             "Max over chosen year",
             "Min over chosen year")
  
  #Max
  
  PRCP_max =  weather_data %>% 
    summarize('PRCP' = max(PRCP))
  
  SNOW_max =  weather_data %>% 
    summarise('SNOW' = max(SNOW))
  
  SNWD_max =  weather_data %>% 
    summarise('SNWD' = max(SNWD))
  
  TMAX_max =  weather_data %>% 
    summarise('TMAX' = max(TMAX))
  
  TMIN_max =  weather_data %>% 
    summarise('TMIN' = max(TMIN))
  
  #Min - don't need to do precipitations
  
  TMAX_min =  weather_data %>% 
    summarise('TMAX' = min(TMAX))
  
  TMIN_min =  weather_data %>% 
    summarise('TMIN' = min(TMIN))
  
  #50 yr avgs
    
  PRCP_avg_day =  weather_data %>% 
      filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarize('PRCP' = mean(PRCP))
  
  SNOW_avg_day =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNOW' = mean(SNOW))
  
  SNWD_avg_day =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNWD' = mean(SNWD))
  
  TMAX_avg_day =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('TMAX' = mean(TMAX))
  
  TMIN_avg_day =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('TMIN' = mean(TMIN))
  
  
  #Percent of days - don't need to do temps
  
  PRCP_day_perc = weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('PRCP' = (sum(PRCP >0))/n()*100)
  
  
  SNOW_day_perc = weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNOW' = (sum(SNOW >0))/n()*100)
  
  
  SNWD_day_perc = weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNWD' = (sum(SNWD >0))/n()*100)
  
  #Max for given day
  
  PRCP_avg_max =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarize('PRCP' = max(PRCP))
  
  SNOW_avg_max =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNOW' = max(SNOW))
  
  SNWD_avg_max =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNWD' = max(SNWD))
  
  TMAX_avg_max =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('TMAX' = max(TMAX))
  
  TMIN_avg_max =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('TMIN' = max(TMIN))
  
  #Min for given day
  
  PRCP_avg_min =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarize('PRCP' = min(PRCP))
  
  SNOW_avg_min =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNOW' = min(SNOW))
  
  SNWD_avg_min =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('SNWD' = min(SNWD))
  
  TMAX_avg_min =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('TMAX' = min(TMAX))
  
  TMIN_avg_min =  weather_data %>% 
    filter(month == as.numeric(input$Month_avg) & day == as.numeric(input$Day_avg)) %>% 
    summarise('TMIN' = min(TMIN))
  
  #Avg over chosen year

  PRCP_avg_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('PRCP' = mean(PRCP))
  
  SNOW_avg_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('SNOW' = mean(SNOW))
  
  SNWD_avg_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('SNWD' = mean(SNWD))
  
  TMAX_avg_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('TMAX' = mean(TMAX))
  
  TMIN_avg_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('TMIN' = mean(TMIN))
  
  #Percent of days - don't need to do temps
  
  PRCP_day = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('PRCP' = (sum(PRCP >0))/n()*100)
  
  
  SNOW_day = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('SNOW' = (sum(SNOW >0))/n()*100)
  
  
  SNWD_day = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('SNWD' = (sum(SNWD >0))/n()*100)
  
  #Max over chosen year
  
  PRCP_max_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('PRCP' = max(PRCP))
  
  SNOW_max_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('SNOW' = max(SNOW))
  
  SNWD_max_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('SNWD' = max(SNWD))
  
  TMAX_max_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('TMAX' = max(TMAX))
  
  TMIN_max_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('TMIN' = max(TMIN))
  
  #Min over chosen year - don't need to do precip

  
  TMAX_min_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('TMAX' = min(TMAX))
  
  TMIN_min_year = weather_data %>% 
    filter(year == as.numeric(input$Year_avg)) %>% 
    summarise('TMIN' = min(TMIN))
  
  
  PRCP = rbind(PRCP_max, "0", round(PRCP_avg_day,2),
               round(PRCP_day_perc,2),
               round(PRCP_avg_max,2),
               round(PRCP_avg_min,2),
               round(PRCP_avg_year,2),
               round(PRCP_day,2),
               round(PRCP_max_year,2), "0")
  SNOW = rbind(SNOW_max, "0", round(SNOW_avg_day,2),
               round(SNOW_day_perc,2),
               round(SNOW_avg_max,2),
               round(SNOW_avg_min,2),
               round(SNOW_avg_year,2),
               round(SNOW_day,2),
               round(SNOW_max_year,2), "0")
  SNWD = rbind(SNWD_max, "0", round(SNWD_avg_day,2),
               round(SNWD_day_perc,2),
               round(SNWD_avg_max,2),
               round(SNWD_avg_min,2),
               round(SNWD_avg_year,2),
               round(SNWD_day,2),
               round(SNWD_max_year,2), "0")
  TMAX = rbind(TMAX_max, TMAX_min, TMAX_avg_day,
               "NA",
               round(TMAX_avg_max,2),
               round(TMAX_avg_min,2),
               round(TMAX_avg_year,2), "NA", TMAX_max_year, TMAX_min_year)
  TMIN = rbind(TMIN_max, TMIN_min, TMIN_avg_day, 
               "NA",
               round(TMIN_avg_max,2),
               round(TMIN_avg_min,2),
               round(TMIN_avg_year,2), "NA", TMIN_max_year, TMIN_min_year)
averages = data.frame(PRCP, SNOW, SNWD, TMAX, TMIN, row.names = W_Type)
 
  
  }, rownames = TRUE)

} 

# Create Shiny object
shinyApp(ui = ui, server = server)
