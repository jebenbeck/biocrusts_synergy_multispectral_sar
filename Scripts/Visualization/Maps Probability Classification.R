#############################################################################################
#######################  Random forest classification and probability maps  ####################### 
#############################################################################################



#' load required packages
#' -------------------------------------------------------------------------------------------

Packages <- c("raster", "sf", "rgdal", "dplyr", "ggplot2", "xlsx", "tidyr", "RColorBrewer", "terra", "ggpubr", "gridExtra", "cowplot")

lapply(Packages, library, character.only = TRUE)

#' change parameters:
region = "Negev"
names <- c("Classification", "Bare", "Biocrusts", "Vegetation")


#' load in datasets
#' -------------------------------------------------------------------------------------------


##' the classification rasters:

load_rasters <- function() {
  list_files <- list.files(paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                                  "/Classification"),
                           pattern = paste0(".*.tif$"), 
                           recursive = T, full.names = T)
  list_stacks <- list()
  for (i in 1:length(list_files)) {
    list_stacks[[i]] <- stack(list_files[[i]])
  }
  stack_full <- stack(list_stacks)
}

stack_rasters <- load_rasters()

#' Convert the rasters to data frame:
stack_df <- as.data.frame(stack_rasters, xy = T, na.rm = T) %>%
  rename(Classification = 3, Bare = 4, Biocrusts = 5, Vegetation = 6) %>%
  mutate(Classification = as.numeric(Classification)) %>%
  pivot_longer(c(-x, -y), names_to = "Class", values_to = "Probability") %>%
  mutate(Class = factor(Class, levels = names)) %>%
  group_by(Class) %>%
  group_split()
  
head(stack_df[[1]])
names(stack_df) <- names
names(stack_df)

#' outline of raster:
polygon_outline <- as.polygons(rast(stack_rasters[[1]])  > -Inf)
outline <- sf::st_as_sf(polygon_outline)
plot(outline)


#' Plot function:

plot_probability <- function(list, Title){
  
  myPalette <- brewer.pal(9, name = "Blues")
  
  plot <- ggplot() +
    geom_raster(data = list, aes(x = x, y = y, fill = Probability)) + 
    geom_sf(data = outline, fill = NA, color = "black", size = 0.25) +
    coord_sf(expand = T) +
    scale_fill_gradientn(colours = myPalette, 
                         limits=c(0, 1), 
                         breaks = seq(0, 1, 0.5),
                         labels = scales::label_percent()) +
    guides(fill = guide_colourbar(barwidth = 3, 
                                  barheight = 0.3, 
                                  title.position = "top",
                                  frame.colour=c("black"),
                                  frame.linewidth = .75, 
                                  ticks.colour="black", 
                                  direction="horizontal")) +
    ggtitle(paste0(Title)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size=9, color = "black"),
      legend.title.align = 0.5,
      legend.text = element_text(size=7, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 10, colour = "black")
      )
  
  return(plot)
}

plot_classification <- function(list, Title){
  
  myPalette <- brewer.pal(9, name = "Blues")
  
  plot <- ggplot() +
    geom_raster(data = list, aes(x = x, y = y, fill = as.factor(Probability))) + 
    geom_sf(data = outline, fill = NA, color = "black", size = 0.25) +
    coord_sf(expand = T) +
    scale_fill_manual(values = c("#E3C922", "#215495", "#1BB325"),
                      labels = c("Bare", "Biocrusts", "Vegetation")) +
    guides(fill = guide_legend(title = "Class", 
                               title.position = "left",
                               keywidth = 0.7, keyheight = 0.2,
                               label.position = "right",
                               direction="vertical")) +
    ggtitle(paste0(Title)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size=9, color = "black"),
      legend.title.align = 0.5,
      legend.text = element_text(size=7, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 10, colour = "black"),
      legend.key = element_rect(color="black")
      )
  
  return(plot)
}

#' Calculate changes by applying the LULC change function to each of the FAO LULC tables:
plot_bar <- plot_probability(stack_df$Bare, "Bare")
plot_bsc <- plot_probability(stack_df$Biocrusts, "Biocrusts")
plot_veg <- plot_probability(stack_df$Vegetation, "Vegetation")
plot_cla <- plot_classification(stack_df$Classification, "Classification")

#' arrange plots:

layout_plot <- 
  rbind(c(1,2,3,4), 
        c(1,2,3,4),
        c(1,2,3,4),
        c(1,2,3,4),
        c(1,2,3,4),
        c(NA,5,NA,6)
  )

layout_plot <- 
  rbind(c(1,2), 
        c(1,2),
        c(1,2),
        c(1,2),
        c(3,4),
        c(3,4),
        c(3,4),
        c(3,4),
        c(5,6)
  )

legend_cla <- get_legend(plot_cla)
  
legend_pro <- get_legend(plot_bar)
  
plot_grid <- grid.arrange(plot_bar + theme(legend.position = "none"), plot_bsc + theme(legend.position = "none"),
                       plot_veg + theme(legend.position = "none"), plot_cla + theme(legend.position = "none"),
                       legend_pro, legend_cla, 
                       layout_matrix = layout_plot)

#' export_plot

ggsave(filename = paste0("Classification_", region, ".png"), plot = plot_grid, device = "png", bg = "white",
       path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Classification Maps"), 
       width = 160, height = 190, units = "mm", dpi = 300)

