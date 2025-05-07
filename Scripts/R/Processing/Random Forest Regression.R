#############################################################################################
#######################  Random forest regression  ####################### 
#############################################################################################



#' load required packages
#' -------------------------------------------------------------------------------------------


Packages <- c("raster", "rgdal", "sp", "randomForest", "dplyr", "dismo", "Metrics", "ggplot2", "xlsx")
lapply(Packages, library, character.only = TRUE)

#' change parameters:
region = "Soebatsfontein"
predictors_type_optical <- "Sentinel-2"



#' load in datasets
#' -------------------------------------------------------------------------------------------


##' the prediction parameters:

load_predictors <- function(Type) {
  list_files <- list.files(paste0("C:/GEODATA/Master Thesis/Satellite data/Metrics/", region),
                           pattern = paste0(Type, ".*.tif$"), 
                           recursive = T, full.names = T)
  list_stacks <- list()
  for (i in 1:length(list_files)) {
    list_stacks[[i]] <- stack(list_files[[i]])
  }
  stack_full <- stack(list_stacks)
}

stack_predictors_optical <- load_predictors(predictors_type_optical)
stack_predictors_sar <- load_predictors("Sentinel-1")

#' subset parameters:
stack_predictors_optical_indices <- stack(
  raster::subset(stack_predictors_optical, grep(c("CI"), names(stack_predictors_optical), value = T)),
  raster::subset(stack_predictors_optical, grep("NDVI", names(stack_predictors_optical), value = T))
)

#' combine all parameters:
stack_predictors <- stack(stack_predictors_optical_indices, stack_predictors_sar)
names(stack_predictors)


##' The reference data raster: 
reference_raster <- raster(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Regression/Reference_biocrust_probability_", region, ".tif"))
plot(reference_raster)
reference_raster


#' prepare the sample data
#' -------------------------------------------------------------------------------------------


#' to use the sample data it first has to be converted from raster to point datasets
#' So we need to generate one point for each sentinel raster cell 

#' generate point samples for each raster cell:
reference_points <- rasterToPoints(reference_raster, spatial = T)
names(reference_points) <- "Probability_BSC"
head(reference_points)

#' extract the predictor values to the training data frame:
#' We use the x-y coordinates to extract the values for the respective  locations
reference_df <- raster::extract(stack_predictors, reference_points@coords)

#' combine class information with extracted values
sampdata <- data.frame(Probability_BSC = reference_points$Probability_BSC, reference_df)

#' subset:
sampdata <- slice_sample(sampdata, n = 5000)



#' Model training and Validation
#' -------------------------------------------------------------------------------------------


#' to train and evaluate the model k-fold cross-validation is used. 
#' In this technique the data used to fit the model is split into k = 10 groups. 

#' In turn, one of the groups will be used for model testing, while the rest of the data is 
#' used for model training (fitting).


set.seed(100)
j <- kfold(sampdata, k = 10)
table(j)

##' the regression model:
x <- list()

for (k in 1:10) {
  traindata <- sampdata[j!= k, ]
  valdata <- sampdata[j == k, ]
  regmodel <- randomForest(Probability_BSC ~., data=traindata, type="regression", importance = T,
                           na.action = na.omit)
  regval <- predict(regmodel, valdata, type='response')
  #' create a dataframe using the validation and prediction
  x[[k]] <- cbind(valdata$Probability_BSC, regval)
}

#' evaluate the model
regmodel
plot(regmodel)


#' Model Prediction
#' -------------------------------------------------------------------------------------------


#' apply the model on the raster stack to get a regression map:
regression_map <- predict(stack_predictors, regmodel, type = "response")
plot(regression_map)

#' export the regression map to disk as raster:
writeRaster(regression_map, filename=paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", 
                                                region, "/Regression/Regression_", region), 
            format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))




#' Variable importance:
#' -------------------------------------------------------------------------------------------


#' calculate the variable importances:
rfImp <- importance(regmodel)
rfImp

#' plot:
varImpPlot(regmodel)

#' Export variable importance to excel file:
write.xlsx(x = rfImp, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Regression/Variable_Importance_regression_", region, ".xlsx"), 
           col.names = TRUE, row.names = TRUE)



#' Accurracy Assessment:
#' -------------------------------------------------------------------------------------------


#' based on the combined predicted and reference data from the k-folding approach:
y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('Reference', 'Prediction')
head(y)

#' calculate R²:
rsq <- function (a, b) cor(a, b) ^ 2
R2 <- rsq(y$Reference, y$Prediction)
R2

#'calculate RMSE:
RMSE <- rmse(y$Reference, y$Prediction)
RMSE

#' calculate MAE:
MAE <- mae(y$Reference, y$Prediction)
MAE

metrics_df <- as.data.frame(rbind(R2, RMSE, MAE))
colnames(metrics_df) <- "Value"
metrics_df$Metric <- rownames(metrics_df)
metrics_df

#' Export Accuracy tables to excel file:
write.xlsx(x = metrics_df, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Regression/Accuracy_assessment_", region, ".xlsx"), 
           sheetName = "Accurracy Metrics", col.names = TRUE, row.names = FALSE)

write.xlsx(x = y, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Regression/Accuracy_assessment_", region, ".xlsx"), 
           sheetName = "Data", append = TRUE, col.names = TRUE, row.names = FALSE)









