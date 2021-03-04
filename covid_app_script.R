library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
theme_set(theme_minimal())

# Lisa's garden data
data("garden_harvest")

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

#Covid-19 data with necessary adjustments
covid_19_wdate <- covid19 %>% 
  filter(cases >= 20) %>% 
  group_by(state) %>% 
  mutate(twenty_day = min(date)) %>% 
  mutate(days_since_20 = date - twenty_day)

ui <- fluidPage(checkboxGroupInput("checkGroup", label = h3("Pick States to Compare"), 
                                   choices = list("Alabama"="Alabama", "Alaska" ="Alaska", 
                                                  "Arizona" ="Arizona","Arkansas"="Arkansas", 
                                                  "California" ="California", "Colorado"="Colorado", 
                                                  "Connecticut"="Connecticut","Delaware"="Delaware",
                                                  "Florida"="Florida", "Georgia"="Georgia", 
                                                  "Hawaii"="Hawaii","Idaho"="Idaho", "Illinois"="Illinois",
                                                  "Indiana"="Indiana", "Iowa"="Iowa",
                                                  "Kansas"="Kansas", "Kentucky"="Kentucky",
                                                  "Louisiana"="Louisiana", "Maine"="Maine",
                                                  "Maryland"="Maryland", "Massachusetts"="Massachusetts",
                                                  "Michigan"="Michigan", "Minnesota"="Minnesota",
                                                  "Mississippi"="Mississippi", "Missouri"="Missouri",
                                                  "Montana"="Montana", "Nebraska"="Nebraska",
                                                  "Nevada"="Nevada", "New Hampshire"="New Hampshire",
                                                  "New Mexico"="New Mexico","New York"="New York", 
                                                  "North Carolina"="North Carolina", 
                                                  "North Dakota"="North Dakota","Ohio"="Ohio",
                                                  "Oklahoma"="Oklahoma", "Oregon"="Oregon", "Pennsylvania"="Pennsylvania",
                                                  "Rhode Island"="Rhode Island","South Carolina"="South Carolina",
                                                  "South Dakota"="South Dakota", "Tennessee"="Tennessee",
                                                  "Texas"="Texas", "Utah"="Utah",
                                                  "Vermont"="Vermont", "Virginia"="Virginia",
                                                  "Washington"="Washington", "West Virginia"="West Virginia",
                                                  "Wisconsin"="Wisconsin", "Wyoming"="Wyoming"),
                                   selected = 1),
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "timeplot")
                )

server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid_19_wdate %>% 
      filter( state %in% c(input$checkGroup)) %>% 
      ggplot() +
      geom_line(aes(y = log10(cases), x= days_since_20, color = state))
  })
}


shinyApp(ui = ui, server = server)