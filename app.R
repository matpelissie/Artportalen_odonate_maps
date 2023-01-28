library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)

# Occurrence --------------------------------------------------------------

# occ <- readr::read_csv("Code/Artportalen_odonate_maps/Odonata_Artportalen_2006.2020_flt.wgs84_thin.csv")
occ2 <- readr::read_csv("Code/Artportalen_odonate_maps/Odonata_Artportalen_2006.2020_flt.wgs84_linux.csv") %>% 
  dplyr::filter(Noggrannhet<=1000) %>% 
  dplyr::select(Species, YEAR, Longitude, Latitude, Obsid)

occ_thin <- readr::read_csv("Code/Artportalen_odonate_maps/thinned_10km.csv")
spe <- unique(occ_thin$Species)
spe <- spe[!spe %in% c("Aeshna affinis","Anax ephippiger","Nehalennia speciosa")]

WGS84 <- "+proj=longlat +datum=WGS84 +no_defs"
# occ_spe <- sf::st_as_sf(occ2, coords = c("Longitude", "Latitude"), crs = sp::CRS(WGS84), agr = "Species")


# Projections -------------------------------------------------------------

# dir <- "Data/SDM/Projections_rep2_10km/"
# dir2 <- "Data/SDM/Odonata_rep2_10km/"
# scen <- gsub("_Aeshna_caerulea_rep2_10km","",grep(".rds",list.files(paste0("../../",dir,"/Aeshna_caerulea_rep2_10km")), value=T, invert = T))
# pres <- raster::raster(paste0("../../", dir2, "Aeshna_grandis", "_rep2_10km/Rasters/Binary.tif"))
# proj <- raster::raster(paste0("../../", dir, "Aeshna_grandis", "_rep2_10km/", 
#                              "2061.2080", "_", "ACCESS1.0", "_", "rcp85", "_", 
#                              "Aeshna_grandis", "_rep2_10km/Rasters/Binary.tif"))
# lay <- 2*proj-pres

shiny::runApp("Code/Artportalen_odonate_maps")
