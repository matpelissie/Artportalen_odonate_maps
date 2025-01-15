input <- list()
input$species <- "Aeshna viridis"
input$period <- "future"
input$futuretime <- "2041.2060"
input$rcp <- "rcp45"
input$gcm <- "ACCESS1.0"
input$maptype <- "probability"
input$occurrence <- FALSE

input <- list()
input$species <- "Ophiogomphus cecilia"
input$period <- "future"
input$futuretime <- "2041.2060"
input$rcp <- "rcp45"
input$gcm <- "CMCC.CM"
input$maptype <- "probability"
input$occurrence <- FALSE
input$slideryear <- c(2006,2020)


# Suit_plot tests

ggplot(data, aes(x=output, y=name, fill=..x..))+
  geom_density_ridges_gradient()+
  scale_fill_gradient(low="orange", high="navy")

plot <- ggplot(data.frame(lay[]) %>%
                 tidyr::drop_na() %>%
                 dplyr::filter(Probability>0.09),
               aes(x=Probability, fill=stat(x)))+
  # scale_fill_viridis_c(name = "", option = "C")+
  # geom_density_ridges_gradient(aes(y=name))+
  scale_fill_gradient(low="orange", high="navy")+
  theme_light()+
  geom_density(fill="grey50", col=NA, adjust=0.2)+
  geom_vline(xintercept=scores$threshold, color="red")+
  expand_limits(x=c(0,1))+
  labs(y="Frequency", x="Suitability")+
  
  ggdist::stat_dots(
    data=extr, aes(x=Probability),
    ## orientation to the top:
    side = "top",
    ## move geom verticaly
    justification = 0,
    ## adjust grouping (binning) of observations
    binwidth = .007,
    fill = "orange",
    col = "orange",
    alpha = 0.7
  )
plot


p <- terra::vect(x=occ_flt, geom=c("Longitude","Latitude"))
extr <- terra::extract(x=lay, y=p)
df <- data.frame(lay[]) %>%
  tidyr::drop_na() %>%
  dplyr::filter(Probability>0.1)
y <- density(df$Probability, n = 2^12)

plot <- ggplot(data.frame(x = y$x, y = y$y), aes(x, y))+
  geom_segment(aes(xend = x, yend = 0, colour = x))+ 
  scale_color_gradientn(colours=grDevices::heat.colors(n=100, rev=TRUE))+
  theme_light()+
  # geom_density(fill="grey50", col=NA, adjust=0.2)+
  geom_vline(xintercept=scores$threshold, color="grey40")+
  expand_limits(x=c(0,1))+
  labs(y="Frequency", x="Suitability")+
  
  ggdist::stat_dots(
    data=extr, aes(x=Probability),
    ## orientation to the top:
    side = "top",
    ## move geom verticaly
    justification = 0,
    ## adjust grouping (binning) of observations
    binwidth = .007,
    fill = "black",
    col = "black",
    alpha = 0.7,
    inherit.aes = FALSE
  )+
  guides(color="none")
plot
