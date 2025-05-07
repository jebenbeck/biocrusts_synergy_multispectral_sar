#' load required packages
#' -------------------------------------------------------------------------------------------


#' the required packages:
Packages <- c("raster", "ellipse", "ggcorrplot", "dplyr", "ggpubr")

lapply(Packages, library, character.only = TRUE)

region <- "Soebatsfontein"



#' load in the data
#' -------------------------------------------------------------------------------------------



####' Topographic Attributes:
#' ---------------------------


#' path to the Attributes:
TAs_path <- paste0("C:/GEODATA/Master Thesis/Terrain/", region, "/Attributes")

#' list of all images:
list_TAs <- list.files(TAs_path, recursive = F, full.names = T, 
                          pattern = "*.tif$")

#' import all images to list:
stack_TAs <- stack(list_TAs)
names(stack_TAs)
stack_TAs <- subset(stack_TAs, c(3,8,1,9,7,5,6,4,2,10,11))
names(stack_TAs)



####' Classification Probability:
#' --------------------------------


#' path to the classification rasters:
classification_path <- paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, "/Classification/")

#' list of all classification files:
list_classification <- list.files(classification_path, recursive = F, full.names = T, 
                                  pattern = "Probability.*.tif$")

#' import all images to list:
stack_classification <- stack(list_classification)
names(stack_classification) <- c("Bare", "Biocrusts", "Vegetation")



#' Process and Plot:
#' -------------------------------------------------------------------------------------------


stack_combined <- stack(stack_TAs, stack_classification)
names(stack_combined)

stack_df <- as.data.frame(stack_combined, na.rm = T) 
names(stack_df)

#' the plot:
corellation_plot <- function(dataframe, cor_method, title) {

    #' compute and process the corellation matrix:
  cormat <- cor(dataframe, method = cor_method) 
  cormat_filter <- cormat %>%  
    as.data.frame() %>%
    filter(!row.names(cormat) %in% c("Vegetation", "Biocrusts", "Bare")) %>%
    select(c(Vegetation, Biocrusts, Bare))

  row.names(cormat_filter) <- c("Elevation", "Slope", "Aspect", "TRI", "Profile Curvature", "Plan Curvature",  
                                "Positive Openness",  "Negative Openness", "Direct Insolation", "TWI", "WEI")
  
  #' Corellation matrix Plot:
  corplot <- ggcorrplot(cormat_filter, lab = TRUE, lab_size = 7/.pt) +
    scale_y_discrete(expand = c(0,0),
                     name = "Class") +
    scale_x_discrete(expand = c(0,0),
                     name = "Topographic Attribute",
                     position = "top") +
    theme_bw() +
    guides(fill = guide_colourbar(barwidth = 7, 
                                  barheight = 0.3,
                                  title = paste(title),
                                  title.position = "top",
                                  frame.colour=c("black"),
                                  frame.linewidth = .75, 
                                  ticks.colour="black", 
                                  direction="horizontal")) +
    theme(
      axis.text.y = element_text(size=7, color = "black"), 
      axis.text.x.top = element_text(size=7, color = "black", angle = 45, hjust=0, vjust = 0), 
      axis.title = element_blank(), 
      panel.grid.major = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
      legend.position = "bottom",
      legend.title = element_text(size=9, color = "black"),
      legend.title.align = 0.5,
      legend.text = element_text(size=7, color = "black"),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-2,-10,5,-10)
    )
  
  return(corplot)
  
}

cor_plot_spearman <- corellation_plot(stack_df, "spearman", "Spearman Correlation")
cor_plot_pearson <- corellation_plot(stack_df, "pearson", "Pearson Correlation")

cor_plot_combined <- ggarrange(cor_plot_pearson, cor_plot_spearman, ncol=1, nrow=2)
cor_plot_combined



#' Export Plot:
#' -------------------------------------------------------------------------------------------

ggsave(filename = paste0("Correlation_TA_Class_", region, ".png"), plot = cor_plot_combined, device = "png", 
         path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Corellation Matrices"), 
         width = 180, height = 140, units = "mm", dpi = 300)

