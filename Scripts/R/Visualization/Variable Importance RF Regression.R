#############################################################################################
#######################  Variable Importance Regression  ####################### 
#############################################################################################



#' load required packages
#' -------------------------------------------------------------------------------------------


Packages <- c("xlsx", "ggplot2", "ggpubr", "dplyr")
lapply(Packages, library, character.only = TRUE)



#' load in data:
#' -------------------------------------------------------------------------------------------


#' path to the data:
files_path <- paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results")

#' list of all variable importance tables:
list_files <- list.files(files_path, recursive = T, full.names = T, 
                         pattern = "Variable_Importance_regression.*.xlsx")

#' import all tables to list:
list_dataframes <- setNames(lapply(list_files, read.xlsx, sheetIndex = 1, header = TRUE), 
                            substring(tools::file_path_sans_ext(basename(list_files)), 
                                      first = 32))

#' read in the metrics recode table:
recode_lookup <- read.xlsx(file = "C:/Users/Rieser/Desktop/spatiotemporal_metrics_conversion.xlsx", sheetIndex = 1, header = TRUE)
head(recode_lookup)



#' data processing:
#' -------------------------------------------------------------------------------------------


#' rename first column:
list_dataframes_2 <- sapply(X = names(list_dataframes),
                            FUN = function(x) rename(list_dataframes[[x]], Metric = NA.),
                            simplify = F, USE.NAMES = T)

#' the data recoding function, dataframe is the original table, recode_lookup contains the conversion matrix
recode_metrics <- function(dataframe, recode_lookup){
  
  metrics_new <- recode_lookup[[2]]                        #' new metrics
  names(metrics_new) <- recode_lookup[[1]]                 #' named with old Types
  
  types <- recode_lookup[[3]]                              #' types
  names(types) <- recode_lookup[[1]]                       #' named with old Types
  
  data_recoded <- dataframe %>%
    mutate(Metric_new = recode(Metric, !!!metrics_new,), #' convert metrics names
           Type = recode(Metric, !!!types)) %>%          #' insert types
    as.data.frame()                                      #' convert to data frame
  
  return(data_recoded)
}

#' apply recode function:
list_VarImp_data_recoded <- sapply(X = names(list_dataframes_2),
                                   FUN = function(x) recode_metrics(list_dataframes_2[[x]], recode_lookup),
                                   simplify = F, USE.NAMES = T)


head(list_VarImp_data_recoded$Amoladeras)



#' make plots:
#' -------------------------------------------------------------------------------------------


#' Plots:
plot_accuracy <- function(dataframe, max, spacing){
  
  #' Increase in MSE:
  IncMSE_plot <- ggplot(data = dataframe, mapping = aes(x = X.IncMSE, y = reorder(Metric_new, X.IncMSE), fill = Type)) + 
    geom_point(shape = 21) +
    scale_fill_manual(values = c("#215495", "#1BB325")) +
    scale_x_continuous(breaks = seq(0, 60, 5),
                       name = "IMSE (%)",
                       limits = c(0, 60),
                       expand = c(0, 0))+
    labs(fill = "Parameter types:") +
    theme_bw()+
    theme(axis.text = element_text(size=7, color = "black"), 
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=9, color = "black"), 
          panel.grid.major = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          panel.grid.minor = element_blank(),
          
          legend.title = element_blank(),
          legend.position = c(0.75, 0.1),
          legend.box.background = element_rect(color = "black", size = 0.5),
          legend.direction = "vertical",
          legend.margin = margin(-1, 2, 2, 1),
          legend.key = element_blank(),
          legend.spacing.y = unit(0.1, "cm"),
          legend.key.height = unit(0.1, "cm"),
          legend.key.width = unit(0.3, "cm"),
          legend.text = element_text(size=9, color = "black")
    )
  
  #' Increase in node purity:
  INP_plot <- ggplot(data = dataframe, mapping = aes(x = IncNodePurity, y = reorder(Metric_new, IncNodePurity), fill = Type)) + 
    geom_point(shape = 21) +
    scale_fill_manual(values = c("#215495", "#1BB325")) +
    scale_x_continuous(breaks = seq(0, 550, spacing),
                       name = "INP",
                       limits = c(0, max),
                       expand = c(0,0))+
    theme_bw()+
    theme(axis.text = element_text(size=7, color = "black"), 
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=9, color = "black"), 
          panel.grid.major = element_line(colour="lightgray", size = 0.1, linetype = "solid"),
          panel.grid.minor = element_blank(),
          
          legend.title = element_blank(),
          legend.position = c(0.75, 0.1),
          legend.box.background = element_rect(color = "black", size = 0.5),
          legend.direction = "vertical",
          legend.margin = margin(-1, 2, 2, 1),
          legend.key = element_blank(),
          legend.spacing.y = unit(0.1, "cm"),
          legend.key.height = unit(0.1, "cm"),
          legend.key.width = unit(0.3, "cm"),
          legend.text = element_text(size=9, color = "black")
    )
  
  Var_Imp_plot <- ggarrange(IncMSE_plot, INP_plot, ncol=2, nrow=1)
  return(Var_Imp_plot)
}  



#' define plot parameters:
#' -------------------------------------------------------------------------------------------

list_spacing <- c(5, 10, 5) 
list_max <- c(
  max(list_VarImp_data_recoded$Amoladeras$IncNodePurity)+5,
  max(list_VarImp_data_recoded$Cautivo$IncNodePurity)+5,
  max(list_VarImp_data_recoded$Soebatsfontein$IncNodePurity)+5
)


#' plot and export:
#' -------------------------------------------------------------------------------------------


#' make plots:
list_VarImp_plots <- list()
for(i in 1:length(list_VarImp_data_recoded)){
  list_VarImp_plots[[i]] <- plot_accuracy(list_VarImp_data_recoded[[i]], spacing = list_spacing[[i]], max = list_max[[i]])
  names(list_VarImp_plots)[[i]] <- names(list_VarImp_data_recoded)[[i]]
}

#' export plots:
for (i in 1:length(list_VarImp_plots)){
  ggsave(filename = paste0("VarImp_Regression_", names(list_VarImp_data_recoded)[[i]], ".png"),
         plot = list_VarImp_plots[[i]], device = "png",
         path = "C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Graphics/Variable importance",
         width = 160, height = 120, units = "mm", dpi = 300)
}

