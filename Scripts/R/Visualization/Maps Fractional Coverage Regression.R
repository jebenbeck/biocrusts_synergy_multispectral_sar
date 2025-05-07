#############################################################################################
#######################  Random forest regression probability maps  ####################### 
#############################################################################################



#' load required packages
#' -------------------------------------------------------------------------------------------

Packages <- c("raster", "sf", "rgdal", "dplyr", "ggplot2", "xlsx", "tidyr", "RColorBrewer", "terra", "ggpubr", "gridExtra", "cowplot")

lapply(Packages, library, character.only = TRUE)

#' change parameters:
region = "Cautivo"


#' load in datasets
#' -------------------------------------------------------------------------------------------


##' the prediction parameters:

probability_raster <- raster(paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, "/Regression/Regression_", region, ".tif"))
plot(probability_raster)
names(probability_raster) <- "Probability"

#' Convert the rasters to data frame:
probability_df <- as.data.frame(probability_raster, xy = T, na.rm = T) 
  
head(probability_df)

#' outline of raster:
polygon_outline <- as.polygons(rast(probability_raster)  > -Inf)
outline <- sf::st_as_sf(polygon_outline)
plot(outline)


#' Plot function:

myPalette <- brewer.pal(9, name = "Blues")
  
plot_prob <- ggplot() +
    geom_raster(data = probability_df, aes(x = x, y = y, fill = Probability)) + 
    geom_sf(data = outline, fill = NA, color = "black", size = 0.25) +
    coord_sf(expand = T) +
    scale_fill_gradientn(colours = myPalette, 
                         limits=c(0, 1), 
                         breaks = seq(0, 1, 0.2),
                         labels = scales::label_percent()) +
    guides(fill = guide_colourbar(barwidth = 7,
                                  title = "Fractional biocrust cover",
                                  barheight = 0.3, 
                                  title.position = "top",
                                  frame.colour=c("black"),
                                  frame.linewidth = .75, 
                                  ticks.colour="black", 
                                  direction="horizontal")) +
    #ggtitle(paste0(Title)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size=9, color = "black"),
      legend.title.align = 0.5,
      legend.text = element_text(size=7, color = "black")#,
      #plot.title = element_text(hjust = 0.5, size = 10, colour = "black")
    )

plot_prob

#' export_plot

ggsave(filename = paste0("Regression_probability_", region, ".png"), plot = plot_prob, device = "png", bg = "white",
       path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Regression Maps"), 
       width = 80, height = 100, units = "mm", dpi = 300)

