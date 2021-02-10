library(shiny)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(sf)
library(viridis)

map <- st_read("eastafrica_kpi_summary.geojson")
map <- as_Spatial(map)
map$NAME <- paste0(map$country.x, "_", map$DESCRIPTIO)

# population range
popmin <- floor(min(map$population_sum, na.rm = TRUE))
popmax <- ceiling(max(map$population_sum, na.rm = TRUE))

# croparea range
cropareamin <- floor(min(map$croparea_sum, na.rm = TRUE))
cropareamax <- ceiling(max(map$croparea_sum, na.rm = TRUE))


# mapdata <- map %>% st_drop_geometry() %>% select(c(uid, croparea_sum, population_sum))
mapdata <- map@data %>% select(c(uid, croparea_sum, population_sum))

pal <- colorBin("YlOrRd", mapdata$population_sum, bins=5, na.color = "#bdbdbd")

ui <- bootstrapPage(
  absolutePanel(
    top = 10,
    right = 10,
    sliderInput(
      "population",
      "population_sum",
      popmin,
      popmax,
      value = c(popmin, popmax),
      step = 1000
    ),
    sliderInput(
      "croparea_sum",
      "croparea",
      cropareamin,
      cropareamax,
      value = c(cropareamin, cropareamax),
      step = 1000
    ),
    # pickerInput(
    #   "Type",
    #   "Type",
    #   choices = c("A", "B"),
    #   selected = c("A", "B"),
    #   multiple = T,
    #   options = list(`actions-box` = TRUE)
    # ),
  ),
  leafletOutput("map", width = "50%")
)

server <- function(input, output, session) {

  selected <- reactive({
    sdata <- mapdata %>%
      filter(population_sum >= input$population[1] & population_sum <= input$population[2]) %>%
      filter(croparea_sum >= input$croparea_sum[1] & croparea_sum <= input$croparea_sum[2])
    sdata
  })
  
  
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>% 
      setView(lng = 39, lat = 6, zoom = 5) %>%
      addTiles() %>%
      addLayersControl(
        position = "bottomright",
        options = layersControlOptions(collapsed = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLegend("bottomright", pal=pal, values=map$population_sum, title = "Title of legend")
  })
  
  
  observe({
    map@data <- left_join(map@data, selected(), by="uid")
    
    leafletProxy("mymap", data = map) %>%
      addTiles() %>% 
      clearShapes() %>% 
      addPolygons(data = map, fillColor = ~pal(map$population_sum), fillOpacity = 0.7, 
                  color = "white", weight = 2)
    
  })

}

shinyApp(ui, server)