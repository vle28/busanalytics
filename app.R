library(shiny)
library(leaflet)
library(tidyverse)
test <- read.csv("C:/Users/Computadora/Documents/SCHOOL/Spring 20/bus anal/test.csv")
test$Name = as.character(test$Name)
test$Service.Type = as.character(test$Service.Type)
test$Address = as.character(test$Address)
test$Phone.Number = as.character(test$Phone.Number)
test$Website = as.character(test$Website)
test <- test %>% 
  mutate(label = paste("Name: ", Name, "<br/>",
                       "Address: ", Address, "<br/>",
                       "Phone Number: ", Phone.Number, "<br/>",
                       "Website: ", Website, "<br/>"
  ))


ui <- fluidPage(
  titlePanel("Service Search"),
  sidebarLayout(
    sidebarPanel(
      selectInput("service", "Service:",
                  choices = unique(test$Service.Type))
    ),
    mainPanel(
      leafletOutput("mymap"),
      p()
    )
  )
)

server <- function(input, output, session) {
  
  filtered <- test %>% filter(Service.Type == input$service)
  
  output$mymap <- renderLeaflet({
    leaflet(data = test %>% filter(Service.Type == input$service)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(~Lon,
                 ~Lat,
                 popup = ~label,
                 label = ~Name)
    
  output$table <- renderTable(filtered)
  })
}

shinyApp(ui, server)