#############################################################################################
#######################  Random forest classification and regression  ####################### 
#############################################################################################



#' load required packages
#' -------------------------------------------------------------------------------------------


Packages <- c("raster", "rgdal", "sp", "randomForest", "dplyr", "dismo", "Metrics", "xlsx")

lapply(Packages, library, character.only = TRUE)

#' change parameters:
region = "Soebatsfontein"
predictors_type_optical <- "Sentinel-2"

classes <- c("Bare", "Biocrusts", "Vegetation")



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
reference_raster <- raster(paste0("C:/GEODATA/Master Thesis/Reference data/", region, "/Classification/Reference_data_raster_classes_", region, ".tif"))
plot(reference_raster)
unique(reference_raster)
reference_raster


#' prepare the sample data
#' -------------------------------------------------------------------------------------------


#' to use the sample data it first has to be converted from raster to point datasets
#' So we need to generate one point for each sentinel raster cell 

#' generate point samples for each raster cell:
reference_points <- rasterToPoints(reference_raster, spatial = T)
names(reference_points) <- "Class"

head(reference_points)

#' extract the predictor values to the training data frame:
#' We use the x-y coordinates to extract the values for the respective  locations
reference_df <- raster::extract(stack_predictors, reference_points@coords)

#' combine class information with extracted values
sampdata <- data.frame(Class = reference_points$Class, reference_df)

#' optional subset:
#sampdata <- sample_n(sampdata, 2000)



#' Model training and Validation
#' -------------------------------------------------------------------------------------------


#' to train and evaluate the model k-fold cross-validation is used. 
#' In this technique the data used to fit the model is split into k = 10 groups. 

#' In turn, one of the groups will be used for model testing, while the rest of the data is 
#' used for model training (fitting).

set.seed(100)
j <- kfold(sampdata, k = 10, by=sampdata$Class)
table(j)

#' Now the model is trained and tested 10 times, each time computing a confusion matrix that 
#' is stored in a list.

x <- list()
for (k in 1:10) {
  traindata <- sampdata[j!= k, ]
  valdata <- sampdata[j == k, ]
  classmodel <- randomForest(as.factor(Class) ~., data=traindata, type="classification",
                             importance=T, na.action = na.omit)
  classval <- predict(classmodel, valdata, type='class')
  #' create a dataframe using the validation and prediction
  x[[k]] <- cbind(valdata$Class, as.integer(classval))
}

#' evaluate the model
classmodel
plot(classmodel)


#' Model Predictions
#' -------------------------------------------------------------------------------------------


#' apply the model on the raster stack to get a classification map:
classification_map <- predict(stack_predictors, classmodel, type = "class")
plot(classification_map)

#' apply the model on the raster stack to get probability maps:
probability_maps <- predict(stack_predictors, classmodel, type = "prob", index = 1:length(classes))
names(probability_maps) <- classes
plot(probability_maps)

#' export the probability maps to disk as raster:
writeRaster(probability_maps, 
            filename=paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", 
                            region, "/Classification/Probability_classification_", region, "_", names(probability_maps)), 
            format="GTiff", overwrite=TRUE, bylayer=T, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

#' export the classification map to disk as raster:
writeRaster(classification_map, filename=paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", 
                                           region, "/Classification/Classification_", region), 
            format="GTiff", overwrite=TRUE, options=c("INTERLEAVE=BAND","COMPRESS=LZW"))



#' Variable importance:
#' -------------------------------------------------------------------------------------------


#' calculate the variable importances_
rfImp <- importance(classmodel)
colnames(rfImp)[1:3] <- classes
colnames(rfImp)

#' plot:
varImpPlot(classmodel, cex=1)

#' Export variable importance to excel file:
write.xlsx(x = rfImp, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Classification/Variable_Importance_classification_", region, ".xlsx"), 
           col.names = TRUE, row.names = TRUE)



#' Calculate class area percentage in AOI:
#' -------------------------------------------------------------------------------------------


#' calculate the area and percentage of AOI covered by each class:
area_m2 <- tapply(area(classification_map), classification_map[], sum) #' area in m²
area_ha <- area_m2/10000                                               #' area in ha
sum_area <- sum(area_ha)                                               #' total area
area_percent <- (area_ha/sum_area)*100                                 #' proportions in %
coverage <- as.data.frame(rbind(area_m2, area_ha, area_percent))       #' combine to df
names(coverage) <- classes
coverage$Total <- rowSums(coverage[1:3])                               #' add total area
coverage

#' Export class coverage to excel file:
write.xlsx(x = coverage, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Classification/Coverage_classes_area_", region, ".xlsx"), 
           col.names = TRUE, row.names = TRUE)



#' Accurracy Assessment:
#' -------------------------------------------------------------------------------------------


#' based on the combined confusion matrices from the k-folding approach:
y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('Reference', 'Classification')
conmat <- table(y)

#' change the name of the classes
colnames(conmat) <- classes
rownames(conmat) <- colnames(conmat)


##' calculate all necessary accurracy metrics:


##' number of cases
n <- sum(conmat)

##' number of correctly classified cases per class
diag <- diag(conmat)

##' Overall Accuracy
OA <- sum(diag) / n

##' Kappa coefficient:

#' observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n

# predicted cases per class
colsums <- apply(conmat, 2, sum)
colsums
q <- colsums / n

expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)

##' Producer's accuracy
PA <- diag / colsums

## User's accuracy
UA <- diag / rowsums


###' make an complete error matrix:

Errmat <- cbind(conmat, rowsums, UA)

colsums_new <- c(colsums, sum(diag), NA)
PA_new <- c(PA, NA, NA)

Errmat <- rbind(Errmat, colsums_new, PA_new)

colnames(Errmat) <- c(classes, "Classification total", "UA (%)")
rownames(Errmat) <- c(classes, "Reference total", "PA (%)")

Errmat_df <- as.data.frame(Errmat)
Errmat_df


###' make an Accuracy table:

Accuracy_df <- data.frame(PA, UA)
Accuracy_df$Kappa <- c(kappa, NA, NA)
Accuracy_df$OA <- c(OA, NA, NA)
colnames(Accuracy_df) <- c("PA (%)", "UA (%)", "Kappa", "OA (%)")
Accuracy_df


#' Export Accuracy tables to excel file:
write.xlsx(x = Errmat_df, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Classification/Accuracy_assessment_", region, ".xlsx"), 
           sheetName = "Error Matrix", col.names = TRUE, row.names = TRUE)

write.xlsx(x = Accuracy_df, 
           file = paste0("C:/Users/Rieser/OneDrive/EAGLE M.Sc/Term 5 (Winter 2021 - 2022)/Master Thesis/Results/", region, 
                         "/Classification/Accuracy_assessment_", region, ".xlsx"), 
           sheetName = "Accuracy Metrics", append = TRUE, col.names = TRUE, row.names = TRUE)


