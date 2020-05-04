#Loads in necessary libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(readxl)
library(stringr)

#Loads data 
test3 <- read.csv("https://github.com/vle28/busanalytics/blob/master/app_data_1.csv?raw=True")

#Cleans data into usable format
test3 <- test3 %>% 
    mutate(label = paste("Name: ", Name, "<br/>",
                         "Website: ", Website, "<br/>", 
                         "Address: ", Address, "<br/>",
                        "Phone Number: ", Phone..General., "<br/>"
          ), service_type = as.list(Service.Type)
    )

#Establishes UI for front end 
ui <- fluidPage(
    titlePanel("Service Search"),
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("service", "Service:",
                        choices = c("Referral", 
                                    "Shelter", 
                                    "Legal", 
                                    "Medical", 
                                    "Housing", 
                                    "Support"))
        ),
        mainPanel(
            leafletOutput("mymap"),
            p()
        )
    )
)

#Creates map
server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
      
      if(length(input$service) == 0){
      mapped_df <- test3
      } 
      else {
      mapped_df <- test3 %>% filter(grepl(paste(input$service, collapse = "|"), Service.Type))
      }
      #Colors (open to change)
      pal <- colorFactor(c("cadetblue4", "darkgoldenrod2", "green2", "blue2", "orangered3", "gray36"), 
                         domain = c("Referral", "Shelter", "Legal", "Medical", "Housing", "Support"))
      
      #Calls leaflet mapping
      leaflet(data = mapped_df) %>%
           addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
           addCircleMarkers(
                      ~Lon,
                      ~Lat,
                      popup = ~label,
                      label = ~Name,
                      color = ~pal(Service.Type),
                      opacity = 1)
    })
}

shinyApp(ui, server)
