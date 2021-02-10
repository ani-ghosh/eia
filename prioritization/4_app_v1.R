library(data.table)
mydat <- data.table( londd=c(20, 38, 96, 32),
                     latdd=c(60, 56, 30, 31),
                     art=c("mountain", "water,sand", "sand", "forest"),
                     anwendung=c("a","b","c","d"))

#Set up ui
ui <- shinyUI(fluidPage(
  sidebarPanel(h5("", width=2),
               checkboxGroupInput(inputId="ArtFlag", label=h4("Art des Bodens"), 
                                  choices=setNames(object=c("mountain", "water", "sand", "forest"),
                                                   nm=c("mountain", "water", "sand", "forest"))),
               checkboxGroupInput(inputId="AnwendungFlag", label=h4("Anwendung"), 
                                  choices=setNames(object=c("a","b","c","d"),
                                                   nm=c("a","b","c","d"))),
               position="left"),
  #App mainPanel content and styles
  mainPanel(fluidRow(leafletOutput(outputId="lmap")))
  )
)

#Set up server
server <- function(input, output){
  #Build leaflet map
  lmap <- leaflet(data=mydat)%>%
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    fitBounds(~min(londd), ~min(latdd), ~max(londd), ~max(latdd))
  
  #Filter data
  datFilt <- reactive(mydat[art%in%input$ArtFlag & anwendung%in%input$AnwendungFlag])
  
  #Add markers based on selected flags
  observe({
    if(nrow(datFilt())==0) {print("Nothing selected");leafletProxy("lmap") %>% clearShapes()}
    else{ #print(paste0("Selected: ", unique(input$InFlags & input$InFlags2)))
      
      leafletProxy("lmap", data=datFilt())%>%clearShapes()%>%
        addCircleMarkers(lng=~londd, lat=~latdd,
                         clusterOptions=markerClusterOptions(), weight=3,
                         color="#33CC33", opacity=1, fillColor="#FF9900", 
                         fillOpacity=0.8)%>% clearShapes()
    }
  })
  
  output$lmap <- renderLeaflet(lmap)
}

#Run app
shinyApp(ui = ui, server = server)