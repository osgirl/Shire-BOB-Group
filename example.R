library(shiny)
library(leaflet)
library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
mapCounty = map("county", fill = TRUE, plot = FALSE)

shinyApp(
  ui = fluidPage(leafletOutput('myMap'),
                 br(),
                 leafletOutput('myMap2')),
  
  server <- function(input, output, session) {
    output$myMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addPolygons(lng = mapStates$x, 
                    lat = mapStates$y, 
                    fillColor = topo.colors(10, alpha = NULL), 
                    stroke = FALSE)
    })
    
    observeEvent(input$myMap_shape_click, {
      click <- input$myMap_shape_click
      if(is.null(click))
        return()       
      
      lat <- click$lat
      lon <- click$lng
      
      coords <- as.data.frame(cbind(lon, lat))
      point <- SpatialPoints(coords)
      mapStates_sp <- map2SpatialPolygons(mapStates, IDs = mapStates$names)
      i <- point [mapStates_sp, ]
      selected <- mapStates_sp [i]
      mapCounty_sp <- map2SpatialPolygons(mapCounty, IDs = mapCounty$names)
      z <- over(mapCounty_sp, selected)
      r <- mapCounty_sp[(!is.na(z))] 
      
      output$myMap2 <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles("Stamen.TonerLite",
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addPolygons(data=r,
                      fillColor = topo.colors(10, alpha = NULL), 
                      stroke = FALSE)
      })  
    })
  })
