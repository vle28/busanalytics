library(shiny)
library(leaflet)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(stringr)

#read and clean data
df <- read.csv("https://github.com/vle28/busanalytics/blob/master/app_data.csv?raw=True", stringsAsFactors = F) %>% 
  #select essential columns to web app
  select(1:17, 26, 27) %>% 
  #don't include Informational Number because not a physical location
  filter(Name != "Informational Number") %>% 
  
  #If no phone, replace with "No Phone Number"
  #If no website, replace with "No Website"
  mutate(Phone = case_when(Phone == "" ~ "No Phone Number",
                           T ~ Phone),
         Website = case_when(Website == "" ~ "No Website",
                             T ~ Website), 
         Description = str_replace_all(Description,'"',"")
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
  mutate(label = paste("Name: ", Name, "<br/>", "<br/>",
                       "Services:", Services, "<br/>", "<br/>",
                       "Address: ", Address, "<br/>", "<br/>",
                       "Phone Number: ", Phone, "<br/>", "<br/>",
                       "Website: ", Website, "<br/>"
  ), Zip.Code = as.character(Zip.Code))


#list of service choices for checkbox
service_choices <- unique(c(df$Service.Type.1, df$Service.Type.2, df$Service.Type.3))
#get rid of empty string in vector of choices
service_choices <- service_choices[!is.na(service_choices)]

#list of victim profile choices for checkbox
profile_choices <- unique(df$Victim.Profile)
#reorder choices
profile_choices <- profile_choices[c(7, 3, 4, 10, 1, 9, 5, 8, 6, 2)]


#Function for filtering input
filter_table <- function(locations, services, zipcode, profiles) {
  
  #if nothing is checked or specified map all services
  #services - nothing, zip - nothing, profiles - nothing
  if (length(services) == 0 & zipcode == "" & length(profiles) == 0) {
    filtered_df <- locations
  }
  
  #if no zip code but box checked, show all with said service
  #services - something, zip - nothing, profiles - nothing
  else if (length(services) != 0 & zipcode == "" & length(profiles) == 0) {
    filtered_df <- locations %>% 
      filter(grepl(paste(services, collapse = "|"), Services))
  }
  
  #if nothing is checked but zip code entered, display only that zip code
  #services - nothing, zip: - something, profiles - nothing
  else if (length(services) == 0 & zipcode != "" & length(profiles) == 0) {
    filtered_df <- locations %>% 
      filter(Zip.Code == zipcode)
  }
  
  #services - nothing, zip: - nothing, profiles - something
  else if (length(services) == 0 & zipcode == "" & length(profiles) != 0) {
    filtered_df <- locations %>% 
      filter(grepl(paste(profiles, collapse = "|"), Victim.Profile))
  }
  
  #services - nothing, zip: - something, profiles - something
  else if (length(services) == 0 & zipcode != "" & length(profiles) != 0) {
    filtered_df <- locations %>% 
      filter(Zip.Code == zipcode &
               grepl(paste(profiles, collapse = "|"), Victim.Profile))
  }
  
  #services - something, zip: - nothing, profiles - something
  else if (length(services) != 0 & zipcode == "" & length(profiles) != 0) {
    filtered_df <- locations %>% 
      filter(grepl(paste(services, collapse = "|"), Services) &
               grepl(paste(profiles, collapse = "|"), Victim.Profile))
  }
  
  #services - something, zip: - something, profiles - nothing
  else if (length(services) == 0 & zipcode == "" & length(profiles) != 0) {
    filtered_df <- locations %>% 
      filter(grepl(paste(services, collapse = "|"), Services) & 
               Zip.Code == zipcode)
  }
  
  #else filter what is mapped by what is checked and has zip
  #services - something, zip: - something, profiles - something
  else{
    filtered_df <- df %>%
      filter(grepl(paste(services, collapse = "|"), Services) & 
               Zip.Code == zipcode &
               grepl(paste(profiles, collapse = "|"), Victim.Profile))
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
      
      searchInput(
        inputId = "zipcode", 
        label = "Zipcode:", 
        placeholder = "Ex: 93405", 
        btnSearch = icon("search"), 
        btnReset = icon("remove"), 
        width = "100%"
      ),
      
      checkboxGroupInput("service", "Service:",
                         choices = service_choices),
      selectInput("profile", "Victim Profile:",
                  choices = unique(profile_choices), 
                  multiple = T) 
      
      #checkboxGroupInput("profile", "Victim Profile:",
      #choices = profile_choices)
    ),
    mainPanel(
      leafletOutput("mymap", width = "100%", height = 600),
      tableOutput("table"),
      p()
    )
  )
)

server <- function(input, output, session) {
  
  output$leaf=renderUI({
    leafletOutput('mymap', width = "100%", height = 600)
  })
  
  output$mymap <- renderLeaflet({
    
    #the map
    leaflet(data = filter_table(df, input$service, input$zipcode, input$profile)) %>%
      
      #map aesthetics
      addProviderTiles(providers$CartoDB.Positron) %>% 
      
      #marker aesthetics
      addCircleMarkers(~Lon,
                       ~Lat,
                       popup = ~label,
                       label = ~Name, 
                       labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                   style = list(
                                                     "color" = "black",
                                                     "font-family" = "serif",
                                                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                     "font-size" = "14px",
                                                     "border-color" = "rgba(0,0,0,0.5)")
                       ))
  })
  
  #output table 
  output$table <- renderTable(filter_table(df, input$service, input$zipcode, input$profile) %>% 
                                
                                #only display essentials-can be changed easily 
                                select(Name, Services, Website, Phone, Description)
  )
}

shinyApp(ui, server)