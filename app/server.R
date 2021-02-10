library(shiny)
library(leaflet)
library(sf)

map <- st_read("eastafrica_kpi_summary.geojson")
map$NAME <- paste0(map$country.x, "_", map$DESCRIPTIO)

# ranges
popmin <- floor(min(map$population_sum, na.rm = TRUE))
popmax <- ceiling(max(map$population_sum, na.rm = TRUE))

#
cropareamin <- floor(min(map$croparea_sum, na.rm = TRUE))
cropareamax <- ceiling(max(map$croparea_sum, na.rm = TRUE))

ui <- fluidPage(
  titlePanel("Interactive tool for prioritization and targeting workflow for the Excellence in Agronomy (EiA) platform"),
  
  sidebarLayout(
    sidebarPanel(
      h3("KPI selection"),
      h6("Adjust each indicator value and press 'calculate' button"),
      sliderInput("popslider", 
                  label = h4("Population"), 
                  min = popmin, 
                  max = popmax, 
                  value = c(popmin, popmax)),
      sliderInput("cropareaslider", 
                  label = h4("Crop area"), 
                  min = cropareamin, 
                  max = cropareamax, 
                   value = c(cropareamin, cropareamax)),
      # sliderInput("provisionsslider", 
      #             label = h4("Provisions"), 
      #             min = 1, 
      #             max = 5, 
      #             value = 1),
      # sliderInput("safetyslider", 
      #             label = h4("Safety"), 
      #             min = 1, 
      #             max = 5, 
      #             value = 1),
      # sliderInput("physicalenvslider", 
      #             label = h4("Physical Environment"), 
      #             min = 1, 
      #             max = 5, 
      #             value = 1),
      actionButton("action", label = "Calculate")
    ),
    mainPanel(
      leafletOutput("mymap", height = "800")
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <-renderLeaflet({
    
    leaflet(data = MunScores2016) %>% addTiles() %>%
      addPolygons(fillColor = ~pal(Total_Score_2016), 
                  fillOpacity = 1, 
                  color = 'white', 
                  weight = 1,
                  popup = popup_dat) %>%
      addLegend("bottomright", # Legend position
                pal=pal, # color palette
                values=~Total_Score_2016, # legend values
                opacity = 0.7,
                title="Percentage difference from national average")
  })  
  
  observeEvent(input$action,
               output$mymap <-renderLeaflet({
                 Total_Score <- NA
                 Total_Score <- ((input$housingslider * MunScores2016$Housing_Score_2016 +
                                    input$populationslider * MunScores2016$Population_Score_2016 +
                                    input$provisionsslider * MunScores2016$Provisions_Score_2016 +
                                    input$safetyslider * MunScores2016$Safety_Score_2016 +
                                    input$physicalenvslider * MunScores2016$PhysicalEnvironment_Score_2016)/
                                   (input$housingslider + input$populationslider + input$provisionsslider + input$safetyslider + input$physicalenvslider))
                 #Create interactive map
                 leaflet(data = MunScores2016) %>% addTiles() %>%
                   addPolygons(fillColor = ~pal(Total_Score), 
                               fillOpacity = 1, 
                               color = 'white', 
                               weight = 1,
                               popup = paste0("<strong>Municipality:</strong>", 
                                              MunScores2016$Municipality_Name, 
                                              "<br><strong>Quality-of-life-o-meter says: </strong>", 
                                              Total_Score)) %>%
                   addLegend("bottomright", # Legend position
                             pal=pal, # color palette
                             values=~Total_Score_2016, # legend values
                             opacity = 0.7,
                             title="Weighted percentage difference from national average")
               })
               
  )
}  
shinyApp(ui = ui, server = server)