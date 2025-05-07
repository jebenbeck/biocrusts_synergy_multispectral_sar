Packages <- c("Metrics", "xlsx", "ggplot2", "ggpubr")
lapply(Packages, library, character.only = TRUE)

region <- "Cautivo"

#' read in the datasets:
accuracy_data <- read.xlsx(file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, "/Regression/Accuracy_assessment_", region, ".xlsx"),
  sheetIndex = 2, header = TRUE)
accuracy_metrics <- read.xlsx(paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, "/Regression/Accuracy_assessment_", region, ".xlsx"),
                               sheetIndex = 1, header = TRUE)

#' get the accurracy metrics:
RSQ <- sprintf("%.3f", accuracy_metrics$Value[1])
RMSE <- sprintf("%.3f", accuracy_metrics$Value[2])
MAE <- sprintf("%.3f", accuracy_metrics$Value[3])

#' scatterplot with regression line and metrics:
s <- ggplot(accuracy_data, aes(x = Reference, y = Prediction)) +
  geom_point(size = 0.5, shape = 16, color = "#215495") +
  geom_smooth(method='lm',formula=y~x, color = "black", linetype = "solid", size = 1) + 
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + 
  stat_regline_equation(aes(label =  paste0("Equation: ", ..eq.label..)), label.x = 0.975, label.y = 0.05, hjust="right", size = 9/.pt) +
  annotate("text", x = 0.975, y = 0.2, label = paste0("R²: ", RSQ), hjust = "right", size = 9/.pt) +
  annotate("text", x = 0.975, y = 0.15, label = paste0("MAE: ", MAE), hjust = "right", size = 9/.pt) +
  annotate("text", x = 0.975, y = 0.1, label = paste0("RMSE: ", RMSE), hjust = "right", size = 9/.pt) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), 
                     name = "Reference", 
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1),
                     name = "Prediction", 
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
ggsave(filename = paste0("Regression_Scatterplot_", region, ".png"), plot = s, device = "png", 
       path = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Regression Scatterplots"), 
       width = 80, height = 80, units = "mm", dpi = 300)
