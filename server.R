server <- function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  # 1) It is "reactive" and therefore should
  #     re-execute automatically when inputs change
  # 2) Its output type is a plot
  
  output$map <- leaflet::renderLeaflet({
    
    dir <- "Data/SDM/Projections_rep2_10km/"
    dir2 <- "Data/SDM/Odonata_rep2_10km/"
    
    if (input$maptype == "range change" & input$period != "present day"){
      
      pres <- raster::raster(paste0("../../", dir2, sub(" ", "_", input$species), 
                                             "_rep2_10km/Rasters/Binary.tif"))
      proj <- raster::raster(paste0("../../", dir, sub(" ", "_", input$species), "_rep2_10km/", 
                                             input$period, "_", input$gcm, "_", input$rcp, "_", 
                                             sub(" ", "_", input$species), "_rep2_10km/Rasters/Binary.tif"))

      lay <- 2 * proj - pres
      
      pal <- leaflet::colorFactor(c(Extinction="#A50026",
                                     Absence="#DCDCDC",
                                     Stability="#B5DF73",
                                     Colonization="#006837"),
                                   -1:2, na.color = "transparent")
      
      labels <- c("Extinction", "Absence", "Stability", "Colonization")
      leg <- "Status"
      
    } else {
      
      if (input$period == "present day"){
        
        lay <- raster::raster(paste0("../../", dir2, sub(" ", "_", input$species), 
                                     "_rep2_10km/Rasters/", stringr::str_to_title(input$maptype), ".tif"))
        
      } else {
        
        lay <- raster::raster(paste0("../../", dir, sub(" ", "_", input$species), "_rep2_10km/", 
                                     input$period, "_", input$gcm, "_", input$rcp, "_", 
                                     sub(" ", "_", input$species), "_rep2_10km/Rasters/",
                                     stringr::str_to_title(input$maptype), ".tif"))
      }
      
      if (input$maptype == "binary"){
        
        
        pal <- leaflet::colorFactor(c(Absence="#DCDCDC",
                                       Presence="black"),
                                     0:1, na.color = "transparent")
        labels <- c("Absence", "Presence")
        leg <- "Status"
        
      } else {
        
        pal <- leaflet::colorNumeric(grDevices::heat.colors(n=100, rev=TRUE),
                                     0:1, na.color = "transparent")
        labels <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
        leg <- "Suitability"
      }
      
    }
    
    l <- leaflet::leaflet() %>% 
      leaflet::addTiles() %>%
      leaflet::addRasterImage(x = lay, colors = pal, opacity = 0.7,
                              project=TRUE, method="ngb") %>% 
      leaflet::addLegend(position = "topright",
                         pal = pal, values = raster::values(lay),
                         labFormat = function(type, cuts, p) paste0(labels),
                         title=leg
                         )
    
    
    if(input$occurrence){
      
      occ_flt <- occ2 %>% 
        dplyr::filter(Species == input$species &
                        YEAR >= input$slideryear[1] &
                        YEAR <= input$slideryear[2]) %>% 
        dplyr::mutate(occ = "Hide/Show records")
      
      l <- l %>%
        leaflet::addCircleMarkers(data = occ_flt,
                                  lng = ~Longitude, lat = ~Latitude, radius=3, stroke=FALSE, fillOpacity=0.5,
                                  popup = paste0("<a href=", "https://www.artportalen.se/Sighting/", occ_flt$Obsid, "> #",
                                                 occ_flt$Obsid," </a>"),
                                  group = ~occ) %>% 
        leaflet::addLayersControl(options = leaflet::layersControlOptions(collapsed = FALSE),
                                  overlayGroups = unique(occ_flt$occ))
    }
    l
    
  })
  
}