library(shiny)
# library(shinydashboard)
library(leaflet)
library(tidyverse)


# Input data --------------------------------------------------------------

# occ <- readr::read_csv("Data/Occurrence/Odonata_Artportalen_2006.2020_flt.wgs84_thin.csv")
# occ2 <- readr::read_csv("Data/Occurrence/Odonata_Artportalen_2006.2020_flt.wgs84_linux.csv") %>% 
#   dplyr::filter(Noggrannhet<=1000) %>% 
#   dplyr::select(Species, YEAR, Longitude, Latitude, Obsid)

occ2 <- readr::read_csv("Data/Occurrence/Odonata_Artportalen_2006.2020_flt.wgs84_linux(1).csv") %>% # raw file splitted to be less than 50Mo individually
  rbind(readr::read_csv("Data/Occurrence/Odonata_Artportalen_2006.2020_flt.wgs84_linux(2).csv")) %>% 
  dplyr::filter(Noggrannhet<=1000) %>% 
  dplyr::select(Species, YEAR, Longitude, Latitude, Obsid)


occ_thin <- readr::read_csv("Data/Occurrence/thinned_10km.csv")
# spe <- unique(occ_thin$Species)
# spe <- spe[!spe %in% c("Aeshna affinis","Anax ephippiger","Nehalennia speciosa")]

# Limit to a few species for now:
spe <- c("Aeshna viridis", "Leucorrhinia albifrons", "Leucorrhinia caudalis",
         "Leucorrhinia pectoralis", "Ophiogomphus cecilia")



# user interface ----------------------------------------------------------


ui <- shiny::fluidPage(
  # title = "Swedish odonates & range predictions",
  
  shiny::fluidPage(  # Application title
    shiny::h1("Odonate ranges in Sweden with climate change predictions")
    # shiny::h1("Range predictions in Swedish odonates")
  ),
  
  shiny::sidebarLayout(
    
    shiny::sidebarPanel(
      # Selector for species
      shiny::selectInput(inputId = "species",
                         label = "Species",
                         choices = spe),
      
      # Selector for period
      shiny::selectInput(inputId = "period",
                         label = "Time period",
                         choices = c("present day", "future")),
      
      # Selector for map type
      shiny::selectInput(inputId = "maptype",
                         label = "Type of map",
                         choices = c("probability", "binary")),
      
      # Selector for future time period
      shiny::conditionalPanel(
        
        condition = "input.period == 'future'", 
        shiny::selectInput(inputId = "futuretime",
                           label = "Future time period",
                           choices = c("2041-2060"="2041.2060", "2061-2080"="2061.2080")),
        
        # Selector for RCP
        shiny::selectInput(inputId = "rcp",
                           label = "Climate change scenario",
                           choices = c("intermediate (RCP4.5)"="rcp45", "worst case (RCP8.5)"="rcp85")),
        
        # Selector for GCM
        shiny::selectInput(inputId = "gcm",
                           label = "Climate model",
                           # choices = c("ACCESS1.0", "CCSM4", "CMCC.CM", "CNRM.CM5","GFDL.ESM2G", "HadGEM2.CC", "MPI.ESM.MR")),
                           choices = c("ACCESS1-0"="ACCESS1.0","CMCC-CM"="CMCC.CM"))
      ),
      
      shiny::p(shiny::h3("Occurrences")),
      
      # Display occurrence records
      shiny::checkboxInput(inputId = "occurrence",
                           label = "Display occurrence records",
                           value = TRUE),
      
      shiny::conditionalPanel(
        
        condition = "input.occurrence == true", 
        # Copy the line below to make a slider bar 
        shiny::sliderInput(inputId = "slideryear", label = "Slider date range", min = 2006, 
                           max = 2020, value = c(2006, 2020), sep="", width='30%')
      ),
      
      shiny::textOutput("auc"),
      
      h6("Only climate variables were used to build the models.
      The actual (un)availability of suitable habitat has not been taken into account
      and may explain observed discrepancies with some records."),
      
      # shiny::conditionalPanel(condition = "input.maptype == 'probability'",
      #                         p("The closest to 1 the more likely the species can be present. 
      #                           Note that only climate variables were used to built the models, 
      #                           the local (un)availability of habitat is not taken into account.")),
      
      
      
      # Sources
      p(shiny::h3(tags$strong("Sources"))),
      
      p("Models were built thanks to occurrence data recorded in", a(href="https://www.artportalen.se/", "Artportalen")),
      
      p("Pélissié M., Johansson F. & Hyseni C. (2022)", a(href="https://doi.org/10.1093/ee/nvac056", "Pushed Northward by Climate Change: Range Shifts 
                                  With a Chance of Co-occurrence Reshuffling in the Forecast for Northern European Odonates", .noWS="after"),
        ".", tags$i("Environmental Entomology", .noWS="after"),", 51: 910–921"),
      
    ),
  
    # Plot map
    shiny::mainPanel(
      leaflet::leafletOutput(outputId = "map", height = "80vh"),
      
      # Plot probability distribution plot
      shiny::conditionalPanel(
        condition = "input.period == 'present day' && input.maptype == 'probability'",
        shiny::absolutePanel(id = "suit_plot", class = "panel panel-default",
                             bottom = 0, right = 20, width = 375, height = 150,
                             fixed=FALSE, draggable = TRUE,
                             
                             shiny::plotOutput("suit_plot", height="150px", width="100%")
        )
      )
    
    ) # end mainPanel
  ) # end sidebarLayout
  
  # shinydashboard::dashboardBody(
  #   
  #   tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  #   leaflet::leafletOutput("map")
  #   
  # )
  
) # end fluidPage


# server function ---------------------------------------------------------


server <- function(input, output, session) {
  
  # Table with statistics on present-day model
  scores <- reactive({
    
    readr::read_csv(paste0("Data/SDM/", input$species, "/", sub(" ", "_", input$species), 
                                   "_rep2_10km/Tables/esdmeval.csv"))
  })
  
  
  # AUC score of present-day model
  output$auc <- shiny::renderText({
    
    paste("AUC =", round(scores()$AUC, digits=3), "(good model quality if AUC>0.8)")
    
  })
  
  
  # Map layer
  layer <- reactive({
    
    # Load present-day map:
    if (input$period == "present day"){
      
      lay <- terra::rast(paste0("Data/SDM/", input$species, "/", sub(" ", "_", input$species), 
                                "_rep2_10km/Rasters/", stringr::str_to_title(input$maptype), ".tif"))
      return(lay)
      
      # Load future or range change map:
    } else if (input$period == "future"){
      
      # Load future suitability or binary map:
      if (input$maptype != "range change"){
        
        lay <- terra::rast(paste0("Data/SDM/", input$species, "/", 
                                  input$futuretime, "_", input$gcm, "_", input$rcp, "_", 
                                  sub(" ", "_", input$species), "_rep2_10km/Rasters/",
                                  stringr::str_to_title(input$maptype), ".tif"))
        return(lay)
        
        # Load range change map:
      } else {
        
        pres <- terra::rast(paste0("Data/SDM/", input$species, "/", sub(" ", "_", input$species), 
                                   "_rep2_10km/Rasters/Binary.tif"))
        proj <- terra::rast(paste0("Data/SDM/", input$species, "/", 
                                   input$futuretime, "_", input$gcm, "_", input$rcp, "_", 
                                   sub(" ", "_", input$species), "_rep2_10km/Rasters/Binary.tif"))
        
        lay <- 2 * proj - pres
        
        return(lay)
        
      }
    }
  })
  
  
  # Palette and legend according to map type
  pals <- reactive({
    
    if (input$maptype == "range change"){
      
      pal <- leaflet::colorFactor(c(Extinction="#A50026",
                                    Absence="#DCDCDC",
                                    Stability="#B5DF73",
                                    Colonization="#006837"),
                                  -1:2, na.color = "transparent")
      
      labels <- c("Extinction", "Absence", "Still present", "Colonization")
      leg <- "Expected status"
      
      return(list("pal"=pal, "labels"=labels, "leg"=leg))
      
      
    } else if (input$maptype == "binary"){
      
      pal <- leaflet::colorFactor(c(Absence="#DCDCDC",
                                    Presence="black"),
                                  0:1, na.color = "transparent")
      labels <- c("Absence", "Presence")
      leg <- "Status"
      
      return(list("pal"=pal, "labels"=labels, "leg"=leg))
      
    } else if (input$maptype == "probability"){
      
      pal <- leaflet::colorNumeric(grDevices::heat.colors(n=100, rev=TRUE),
                                   0:1, na.color = "transparent")
      
      labels <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
      leg <- "Probability"
      
      return(list("pal"=pal, "labels"=labels, "leg"=leg))
    }
    
  })
  
  
  # Make leaflet map:
  map <- reactive({
    
    l <- leaflet::leaflet() %>% 
      leaflet::addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>% 
      addProviderTiles(providers$Stamen.Terrain, group = "Stamen.Terrain") %>% 
      addLayersControl(baseGroups = c("OpenStreetMap", "OpenTopoMap", "Stamen.Terrain"), 
                       options = layersControlOptions(collapsed = TRUE)) %>% 
      leaflet::addRasterImage(x = layer(), colors = pals()$pal, opacity = 0.7,
                              project=TRUE, method="ngb") %>% 
      leaflet::addLegend(position = "topright",
                         pal = pals()$pal, values = raster::values(layer()),
                         labFormat = function(type, cuts, p) paste0(pals()$labels),
                         title=pals()$leg
                         )
    })
  
  
  # Filter occurrences
  occ_flt <- reactive({
    
    # if(input$occurrence) 
      occ_flt <- occ2 %>%
        dplyr::filter(Species == input$species &
                        YEAR >= input$slideryear[1] &
                        YEAR <= input$slideryear[2]) %>%
        dplyr::mutate(occ = "Hide/Show records")
    
  })
  
  
  # Make plot of suitability distribution
  suit_plot <- reactive({
    
    if(input$maptype == "probability" & input$period == "present day"){
      
      p <- terra::vect(x=occ_flt(), geom=c("Longitude","Latitude"))
      extr <- terra::extract(x=layer(), y=p)
      df <- data.frame(layer()[]) %>%
        tidyr::drop_na() %>%
        dplyr::filter(Probability>0.1)
      y <- density(df$Probability, n = 2^12)
      
      plot <- ggplot(data.frame(x = y$x, y = y$y), aes(x, y))+
        geom_segment(aes(xend = x, yend = 0, colour = x))+ 
        scale_color_gradientn(colours=grDevices::heat.colors(n=100, rev=TRUE))+
        theme_light()+
        # geom_density(fill="grey50", col=NA, adjust=0.2)+
        geom_vline(xintercept=scores()$threshold, color="grey40")+
        expand_limits(x=c(0,1))+
        labs(y=NULL, x="Occurrence probability")+
        # labs(y="Frequency", x="Suitability")+
        
        ggdist::stat_dots(
          data=extr, aes(x=Probability),
          ## orientation to the top:
          side = "top",
          ## move geom verticaly
          justification = 0,
          ## adjust grouping (binning) of observations
          binwidth = .007,
          fill = "#0000FF",
          col = "#0000FF",
          alpha = 0.7,
          inherit.aes = FALSE
        )+
        guides(color="none")
      
      plot
    }
    
    })
  
  output$suit_plot <- shiny::renderPlot({
    suit_plot()
  })
  
  
  final_map <- reactive({
    
    if(input$occurrence){
      
      # Make map with occurrence points:
      l <- map() %>%
        leaflet::addCircleMarkers(data = occ_flt(),
                                  lng = ~Longitude, lat = ~Latitude, radius=3, stroke=FALSE, fillOpacity=0.5, color = "#0000FF",
                                  popup = paste0("<a href=", "https://www.artportalen.se/Sighting/", occ_flt()$Obsid, "> #",
                                                 occ_flt()$Obsid," </a>"),
                                  group = ~occ) %>% 
        leaflet::addLayersControl(options = leaflet::layersControlOptions(collapsed = FALSE),
                                  overlayGroups = unique(occ_flt()$occ))
      return(l)
    }
    
    map()

    })
  
  
  # Display map:
  output$map <- leaflet::renderLeaflet({
    
    final_map()
    
  })
  
  
  # output$maptype_choices <- reactive({
  #   
  #   if(input$period=="present day") choices <- c("suitability"="probability", "binary")
  #   if(input$period=="future") choices <- c("suitability"="probability", "binary", "range change")
  #   
  #   choices
  #   
  # })
  
  
  # Update selection remove range change for present day:
  shiny::observeEvent(input$period, {
    
    if(input$period=="present day") y <- c("probability", "binary")
    if(input$period=="future") y <- c("probability", "binary", "range change")
    
    shiny::updateSelectInput(session, "maptype",
                             label = "Type of map",
                             choices = y)
    
  })
  
  
  # shiny::observe({
  #   
  #   if(input$period=="present day") y <- c("suitability"="probability", "binary")
  #   if(input$period=="future") y <- c("suitability"="probability", "binary", "range change")
  #   
  #   shiny::updateSelectInput(session, "maptype",
  #                            label = "Type of map",
  #                            choices = y)
  #   
  # })
  
}


# Launch app --------------------------------------------------------------

shiny::shinyApp(ui=ui, server=server)
