library(shiny)
library(leaflet)
library(tidyverse)

#read and clean data
df <- read.csv("https://github.com/vle28/busanalytics/blob/master/app_data.csv?raw=True", stringsAsFactors = F) %>% 
  #select essential columns to web app
  select(1, 3:15) %>% 
  
  #If no phone, replace with "No Phone Number"
  #If no website, replace with "No Website"
  mutate(Phone = case_when(Phone == "" ~ "No Phone Number",
                             T ~ Phone),
         Website = case_when(Website == "" ~ "No Website",
                             T ~ Website)
         ) %>% 
  
  #drop rows that can't be shown on map (have no lat lon)
  drop_na(Lat) 

#replace empty strings with NA so they can be united to create Services column
df$Service.Type.2[df$Service.Type.2==""] <- NA
df$Service.Type.3[df$Service.Type.3==""] <- NA

#create services column
df <- df %>% 
  unite("Services", 
        Service.Type.1:Service.Type.3, 
        sep = ", ", 
        na.rm = T,
        remove = F)

#create label
df <- df %>% 
  mutate(label = paste("Name: ", Name, "<br/>", 
                       "Services:", Services, "<br/>", 
                       "Address: ", Address, "<br/>",
                       "Phone Number: ", Phone, "<br/>",
                       "Website: ", Website, "<br/>"
  ))

#list of service choices for checkbox
service_choices <- unique(c(df$Service.Type.1, df$Service.Type.2, df$Service.Type.3))
#get rid of empty string in vector of choices
service_choices <- service_choices[!is.na(service_choices)]


ui <- fluidPage(
  titlePanel("Service Search"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("service", "Service:",
                         choices = service_choices)
    ),
    mainPanel(
      leafletOutput("mymap"),
      p()
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    #if nothing is checked map all services
    if (length(input$service) == 0){
      filtered_df <- df
    }
    
    #else filter what is mapped by what is checked
    else{
      filtered_df <- df %>%
        filter(grepl(paste(input$service, collapse = "|"), Services))
    }
    
    #the map
    leaflet(data = filtered_df) %>%
      
      #map aesthetics
      addProviderTiles(providers$CartoDB.Positron) %>% 
      
      #marker aesthetics
      addCircleMarkers(~Lon,
                 ~Lat,
                 popup = ~label,
                 label = ~Name)
  })
}

shinyApp(ui, server)