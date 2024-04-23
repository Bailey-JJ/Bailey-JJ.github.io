# Final Project Draft Script ####
options(scipen = 999)

library(tidyverse)
library(shiny)
library(leaflet)
library(easystats)
library(patchwork)
library(janitor)
library(shinydashboard)
library(data.table)

meteors <- read_csv("./meteorites.csv")

# In the data-set, there is a correction needed to be made, Northwest Africa 7701, should say 2010, not 2101

meteors[meteors$name == "Northwest Africa 7701", 7] <- 2010
meteors[meteors$name == "Northwest Africa 7701", 7]

### Meteor Cleaning and Mapping ####
m_circles <- 
  meteors %>% 
  filter(!is.na(meteors$lat) & !is.na(meteors$long))

m_circles %>% 
  print(n = 38401)

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

# Graph displaying the count for each class
m_circles %>% 
  ggplot(aes(x = class)) +
  geom_bar() 

m_circles %>% 
  group_by(class) %>%
  count() %>% 
  filter(n > 900) %>% 
  ggplot(aes(x = class, y = n)) +
  geom_col(aes(fill = class))
  

m_circles %>% 
  dplyr::filter(fall == "Fell") %>% 
  dplyr::filter(year > 1900) %>% 
  group_by(year) %>%
  count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_point()

m_circles %>% 
  group_by(class) %>% 
  count() %>%
  filter(n > 1000)

l5 <- 
  m_circles %>% 
    filter(class == "L5")

mean(l5$mass, na.rm = TRUE)
  
acap <- 
  m_circles %>% 
  filter(class == "Acapulcoite")

mean(acap$mass, na.rm = TRUE)


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

## UI ####

ui <- fluidPage(
  titlePanel("Meteorite & Bolide Maps"),
  
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
  
  sources_text <-  "Meteorite Classification Information From...",
  
  h6(textOutput("sources_text")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("class_m", label = h5("Class"), choices = m_circles$class),
      sliderInput("year_m", label = h5("Year"), min = 860, max = 2013, value = range(860, 2013), step = 10),
      sliderInput("mass_m", label = h5("Mass"), min = 0, max = 60000000, value = range(0.00, 60000000.00), step = 1500000.00),
      
      dashboardSidebar(sidebarSearchForm(textId = "searchtext", buttonId = "searchbutton",
                                         label = "Search Class", icon = icon("search"))),
      tableOutput("filtered_table")
    ),
    
    position = c("left", "right"),
    
    mainPanel(
      leafletOutput("Meteors"))
  ))


## SERVER ####
server <- function(input, output){
  filteredData <- reactive({
    m_circles %>% 
      dplyr::filter(class %like% input$class_m) %>% 
      dplyr::filter(year >= input$year_m[1] & year <= input$year_m[2]) %>% 
      dplyr::filter(mass >= input$mass_m[1] & mass <= input&mass_m[2])
  })
  
  
  output$Meteors <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      setView(lng = 0.0, lat = 0.0, zoom = 0.5) %>% 
      addCircles(data = filteredData(), 
                 popup = paste0("Name: ", m_circles$name, '<br>',
                                "ID: ", m_circles$id, '<br>',
                                m_circles$fall, " in: ", m_circles$year, '<br>', 
                                "Class: ", m_circles$class, '<br>',
                                "Mass: ", m_circles$mass, " grams", '<br>',
                                "Geolocation: ", m_circles$geolocation),
                 color = ~pal(year))
  })
  
  observeEvent(input$class_m, {
    m_popup <- paste0("Name: ", filteredData()$name, '<br>',
                      "ID: ", filteredData()$id, '<br>',
                      filteredData()$fall, " in: ", filteredData()$year, '<br>', 
                      "Class: ", filteredData()$class, '<br>',
                      "Mass: ", filteredData()$mass, " grams", '<br>',
                      "Geolocation: ", filteredData()$geolocation)
    leafletProxy("Meteors") %>%
      clearMarkers() %>%
      addCircles(data = filteredData(), 
                 popup = m_popup,
                 color = ~pal(year))
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
}

shinyApp(ui, server)
