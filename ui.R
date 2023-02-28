ui <- shinydashboard::dashboardPage(title = "Swedish odonates & range predictions",
  
  shinydashboard::dashboardHeader(  # Application title
    title = "Range predictions in Swedish odonates",
    titleWidth = 450),
  
  shinydashboard::dashboardSidebar(
    # Selector for species
    shiny::selectInput(inputId = "species",
                       label = "Choose a species",
                       choices = spe),
    
    # Selector for period
    shiny::selectInput(inputId = "period",
                       label = "Choose a period",
                       choices = c("present day", "2041.2060", "2061.2080")),
    
    # Selector for map type
    shiny::selectInput(inputId = "maptype",
                       label = "Choose a type of map",
                       choices = c("probability", "binary", "range change")),
    
    # Selector for RCP
    shiny::selectInput(inputId = "rcp",
                       label = "Choose a scenario",
                       choices = c("rcp45", "rcp85")),
    
    # Selector for GCM
    shiny::selectInput(inputId = "gcm",
                       label = "Choose a climate model",
                       # choices = c("ACCESS1.0", "CCSM4", "CMCC.CM", "CNRM.CM5","GFDL.ESM2G", "HadGEM2.CC", "MPI.ESM.MR")),
                       choices = c("ACCESS1.0","CMCC.CM")),
    
    # Display occurrence records
    shiny::checkboxInput(inputId = "occurrence",
                         label = "Display occurrence records",
                         value = TRUE),
    
    # Copy the line below to make a slider bar 
    shiny::sliderInput("slideryear", label = shiny::h3("Slider Range"), min = 2006, 
                max = 2020, value = c(2006, 2020), sep="")
    
  ),
  
  shinydashboard::dashboardBody(
    
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    leaflet::leafletOutput("map")
    
  )
)