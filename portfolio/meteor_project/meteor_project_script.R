# Final Project Draft Script ####
options(scipen = 999)

library(tidyverse)
library(shiny)
library(leaflet)
library(easystats)
library(patchwork)
library(plotly)
library(janitor)
library(shinydashboard)
library(data.table)

meteors <- read_csv("./meteorites.csv")

bolides <- read_csv("./bolides.csv")

# In the dataset, there is a correction needed to be made, Northwest Africa 7701, should say 2010, not 2101

meteors[meteors$name == "Northwest Africa 7701", 7] <- 2010
meteors[meteors$name == "Northwest Africa 7701", 7]

### Meteor Cleaning and Mapping ####
m_circles <- 
  meteors %>% 
  filter(!is.na(meteors$lat) & !is.na(meteors$long))

pal <- colorFactor("viridis", levels = m_circles$year)

leaflet() %>%
  addTiles() %>%
  setView(lng = 0.0, lat = 0.0, zoom = 0.5) %>% 
  addCircles(data = m_circles, radius = 100,
             popup = paste0("Name: ", m_circles$name, '<br>',
                            "ID: ", m_circles$id, '<br>',
                            m_circles$fall, " in: ", m_circles$year, '<br>', 
                            "Class: ", m_circles$class, '<br>',
                            "Mass: ", m_circles$mass, " grams", '<br>',
                            "Geolocation: ", m_circles$geolocation),
             color = ~pal(year))

# Next step isto let user select a year, class, or mass to view


### Bolides Cleaning and Mapping (May or may not use this dataset in the end product) ####
b_circles <-  
  bolides %>% 
  janitor::clean_names() %>% 
  filter(!is.na(bolides$latitude) & !is.na(bolides$longitude)) %>% 
  filter(!is.na(bolides$`altitude(km)`) & !is.na(bolides$`total impact energy`))

b_circles <- 
  b_circles %>%
  separate(col = latitude, into = c('lat', 'direction'), sep = -1, remove = FALSE) %>%
  mutate(long = as.numeric(lat)) %>% 
  select(-latitude) %>% 
  separate(col = longitude, into = c('long', 'direction2'), sep = -1, remove = FALSE) %>%
  mutate(long = as.numeric(long)) %>% 
  select(-longitude)

b_circles <- 
  within(b_circles, {
    lat <- ifelse(direction=="N", lat, (as.numeric(lat) * -1))
  }) %>% 
  select(-direction)

b_circles <- 
  within(b_circles, {
    long <- ifelse(direction2=="E", long, (as.numeric(long) * -1))
  }) %>% 
  select(-direction2)

b_circles$lat <- b_circles$lat %>% as.numeric
b_circles$long <- b_circles$long %>% as.numeric

pal2 <- colorFactor("viridis", levels = b_circles$total_impact_energy)

leaflet() %>%
  addTiles() %>%
  setView(lng = 0.0, lat = 0.0, zoom = 0.5) %>% 
  addCircles(data = b_circles, radius = 100,
             popup = paste0("Altitude: ", b_circles$altitude_km, " kilometers", '<br>',
                            "Total Impact Energy: ", b_circles$total_impact_energy, '<br>',
                            "GeoLocation: ", "(", b_circles$lat, ", ", b_circles$long, ")"),
             color = ~pal2(total_impact_energy))


### Exploring data ####

m_circles %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(x = year, y = mass, color = name_type)) + 
  geom_point()

# This graph shows the expected results, that there are more meteors being found from 1700s to current day.

max(m_circles$year, na.rm = TRUE)
min(m_circles$year, na.rm = TRUE)

max(m_circles$mass, na.rm = TRUE)
min(m_circles$mass, na.rm = TRUE)

place_names <- 
  m_circles %>% 
  filter(!is.na(year)) %>% 
  mutate(name = trimws(str_remove(name, "(\\s+[A-Za-z]+)?[0-9-]+")))

unique(place_names$name)

# Making Shiny App ####
data_list = list(
  "Meteors" = m_circles,
  "Bolides" = b_circles
)

## UI ####

ui <- fluidPage(
  titlePanel("Meteorite & Bolide Maps"),
  
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  
  sources_text <-  "Meteorite Classification Information From...",
  
  h6(textOutput("sources_text")),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("year_m", label = h5("Year"), min = 860, max = 2013, value = c(860, 2013), step = 10),
      
      dashboardSidebar(sidebarSearchForm(textId = "searchtext", buttonId = "searchbutton",
                                         label = "Search Class", icon = icon("search")))
    ),
      
      tableOutput("filtered_table"),
    
    position = "left"),
    
    mainPanel(
        leafletOutput("Meteors"))
  )


## SERVER ####
server <- function(input, output){
  filteredData <- reactive({
    m_circles %>%
      dplyr::filter(m_circles$year > input$year_m)
  })
  
   example_data1 <- read_csv("search_list.csv")

   example_data2 <- data.table(
     Meteorclass = example_data1$meteorclass,
     Description = example_data1$description)
  
  output$filtered_table <- renderTable({
    req(input$searchbutton == FALSE)
    example_data2[Meteorclass %like% input$searchtext]
  })
  
  observe({
    print(input$m_dictionary)
  })
  
  output$Meteors <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0.0, lat = 0.0, zoom = 0.5) %>% 
      addCircles(data = filteredData(), radius = 100,
                 popup = paste0("Name: ", m_circles$name, '<br>',
                                "ID: ", m_circles$id, '<br>',
                                m_circles$fall, " in: ", m_circles$year, '<br>', 
                                "Class: ", m_circles$class, '<br>',
                                "Mass: ", m_circles$mass, " grams", '<br>',
                                "Geolocation: ", m_circles$geolocation),
                 color = ~pal(year))
  })
  
  observe({
    leafletProxy("Meteors") %>%
      clearMarkers() %>%
      addCircles(data = filteredData(), 
                 popup = paste0("Name: ", m_circles$name, '<br>',
                                "ID: ", m_circles$id, '<br>',
                                m_circles$fall, " in: ", m_circles$year, '<br>', 
                                "Class: ", m_circles$class, '<br>',
                                "Mass: ", m_circles$mass, " grams", '<br>',
                                "Geolocation: ", m_circles$geolocation),
                 color = ~pal(year))
  })
  
}


shinyApp(ui, server)


