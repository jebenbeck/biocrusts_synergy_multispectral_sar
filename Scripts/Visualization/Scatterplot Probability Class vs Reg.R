Packages <- c("Metrics", "xlsx", "ggplot2", "ggpubr", "raster")
lapply(Packages, library, character.only = TRUE)

region <- "Cautivo"



#' load in the classification and regression probability rasters:
prob_raster_class <- raster(paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                                  "/Classification/Probability_classification_", region, "_Biocrusts.tif"))
prob_raster_reg <- raster(paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                                  "/Regression/Regression_", region, ".tif"))

#' stack both together:
prob_stack <- stack(prob_raster_class, prob_raster_reg)
names(prob_stack) <- c("Classification", "Regression")

#' convert to data frame:
prob_df <- as.data.frame(prob_stack, xy = T, na.rm = T)
prob_df_sub <- prob_df[sample(nrow(prob_df), 5000), ]
nrow(prob_df_sub) 

#' calculate Accurracy metrics:

#' calculate R²:
rsq <- function (x, y) cor(x, y) ^ 2
RSQ <- rsq(prob_df_sub$Classification, prob_df_sub$Regression)
RSQ <- sprintf("%.3f", RSQ)
RSQ

#'calculate RMSE:
RMSE <- rmse(prob_df_sub$Classification, prob_df_sub$Regression)
RMSE <- sprintf("%.3f", RMSE)
RMSE

#' calculate MAE:
MAE <- mae(prob_df_sub$Classification, prob_df_sub$Regression)
MAE <- sprintf("%.3f", MAE)
MAE

#' scatterplot with regression line and metrics:
s <- ggplot(prob_df_sub, aes(x = Classification, y = Regression)) +
  geom_point(size = 0.5, shape = 16, color = "#215495") +
  geom_smooth(method='lm',formula=y~x, color = "black", linetype = "solid", size = 1) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + 
  stat_regline_equation(aes(label =  paste0("Equation: ", ..eq.label..)), label.x = 0.975, label.y = 0.05, hjust="right", size = 9/.pt) +
  annotate("text", x = 0.975, y = 0.2, label = paste0("R²: ", RSQ), hjust = "right", size = 9/.pt) +
  annotate("text", x = 0.975, y = 0.15, label = paste0("MAE: ", MAE), hjust = "right", size = 9/.pt) +
  annotate("text", x = 0.975, y = 0.1, label = paste0("RMSE: ", RMSE), hjust = "right", size = 9/.pt) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     name = "Probability", 
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     name = "Fractional cover", 
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text = element_text(size=7, color = "black"), 
        axis.title =  element_text(size=9, color = "black"), 
        panel.grid.major = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
        panel.grid.minor = element_blank()
  )

plot(s)


#' Export scatterplot to disk:
ggsave(filename = paste0("Scatterplot_Prob_Reg_Class_", region, ".png"), plot = s, device = "png", 
       path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Scatterplots Probability Reg Class"), 
       width = 80, height = 80, units = "mm", dpi = 300)

