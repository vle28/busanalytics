library(shiny)
library(leaflet)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(stringr)

#read and clean data
nation_data <- read.csv("https://raw.githubusercontent.com/vle28/busanalytics/master/Nationwide_Data_Cleaned_v3%20-%20Nationwide_Data_Cleaned%20(2).csv", 
                        stringsAsFactors = F) %>% 
    #select essential columns to web app
    #select(1, 3:15) %>% 
    
    #If no phone, replace with "No Phone Number"
    #If no website, replace with "No Website"
    mutate(Cleaned_Phone = case_when(Cleaned_Phone == "" ~ "No Phone Number",
                                     T ~ Cleaned_Phone)
    ) %>% 
    #drop rows that can't be shown on map (have no lat lon)
    drop_na(Latitude) 

#create services column
nation_data <- nation_data %>% 
    unite("Services", 
          Cat1:Cat3, 
          sep = " ", 
          na.rm = T,
          remove = F)

#create label
nation_data <- nation_data %>% 
    mutate(label = paste("Name: ", Cleaned_Agency_Name, "<br/>", "<br/>",
                         "Resources: ", Services, "<br/>", "<br/>",
                         "Address: ", formatted_address, "<br/>", "<br/>",
                         "Phone Number: ", Cleaned_Phone, "<br/>", "<br/>"
    )) 

#Function for filtering input
filter_table <- function(locations, state, city) {
    
    #if nothing is checked or specified -> map all services
    if (length(state) == 0 & length(city) == 0) {
        filtered_df <- locations
    }
    
    #if only city specified -> display city facilities
    else if (length(state) == 0 & length(city) != 0) {
        filtered_df <- locations %>% 
            filter(Cleaned_City == city)
    }
    
    #if only state specified -> display state facilities
    else if (length(state) != 0 & length(city) == 0) {
        filtered_df <- locations %>% 
            filter(grepl(paste(state, collapse = "|"), State))
    }
    
    #else filter what is mapped by both
    else{
        filtered_df <- locations %>%
            filter(grepl(paste(state, collapse = "|"), State) & 
                       Cleaned_City == city)
    }
    return(filtered_df)
}

ui <- fluidPage(
    
    #background selection
    theme = shinytheme(theme = "slate"),
    
    titlePanel("Service Search"),
    sidebarLayout(
        sidebarPanel(
            
            #allows parameter specifications to scroll down
            style = "position:fixed;width:inherit;",
            
            selectInput("city", "City:",
                        choices = unique(nation_data$Cleaned_City), 
                        multiple = T),
            
            selectInput("state", "State:",
                               choices = unique(nation_data$State), 
                        multiple = T),
        ),
        mainPanel(
            leafletOutput("mymap"),
            tableOutput("table"),
            p()
        )
    )
)

server <- function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        
        #the map
        leaflet(data = filter_table(nation_data, input$state, input$city)) %>%
            
            #map aesthetics
            addProviderTiles(providers$CartoDB.Positron) %>% 
            
            #marker aesthetics
            addCircleMarkers(~Longitude,
                             ~Latitude,
                             popup = ~label,
                             label = ~Cleaned_Agency_Name, 
                             labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                         style = list(
                                                             "color" = "black",
                                                             "font-family" = "serif",
                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                             "font-size" = "14px",
                                                             "border-color" = "rgba(0,0,0,0.5)")
                                                         )
                             )
    })
    
    #output table 
    output$table <- renderTable(filter_table(nation_data, input$state, input$city) %>% 
                                    
                                    #only display essentials-can be changed easily 
                                    select(Cleaned_Agency_Name, Services, Cleaned_Phone, State)
    )
}

shinyApp(ui, server)