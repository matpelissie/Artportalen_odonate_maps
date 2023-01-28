ui <- shiny::fluidPage(
  
  # Application title
  shiny::titlePanel("Odonate range predictions in Sweden"),
  
  # Sidebar with a slider input for the number of bins
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      # Selector for species
      shiny::selectInput(inputId = "species",
                         label = "Choose a species",
                         choices = spe),
      
      # Selector for map type
      shiny::selectInput(inputId = "maptype",
                         label = "Choose a type of map",
                         choices = c("probability", "binary", "range change")),
      
      # Selector for period
      shiny::selectInput(inputId = "period",
                         label = "Choose a period",
                         choices = c("present day", "2041.2060", "2061.2080")),
      
      # Selector for RCP
      shiny::selectInput(inputId = "rcp",
                         label = "Choose a scenario",
                         choices = c("rcp45", "rcp85")),
      
      # Selector for GCM
      shiny::selectInput(inputId = "gcm",
                         label = "Choose a climate model",
                         choices = c("ACCESS1.0", "CCSM4", "CMCC.CM", "CNRM.CM5","GFDL.ESM2G", "HadGEM2.CC", "MPI.ESM.MR")),
      
      # Display occurrence records
      shiny::checkboxInput(inputId = "occurrence",
                         label = "Display occurrence records",
                         value = FALSE),
      
    ),
    
    # Show a plot of the generated distribution
    shiny::mainPanel(
      # shiny::plotOutput("occmap")
      # ,
      
      # Leaflet map
      leaflet::leafletOutput("map"),
      width=8
    )
  )
)