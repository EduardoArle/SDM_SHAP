#load packages
library(randomForest)

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'

#read temperature results table

setwd(wd_tables)
res_temp <- read.csv('Temperature_Rel_Polar_all_points_order.csv')

#calculate species' latitudinal amplitude
res_temp$latAmplitude <- res_temp$maxLat - res_temp$minLat

#calculate 1/SE * slope
res_temp$one_SE_x_slope_minT_RP <- (1/res_temp$minTSE_RP) * res_temp$minTslope_RP 

#run RF
model <- randomForest(minTslope_RP ~ n_records + 
                                     rangeSize + 
                                     latAmplitude +
                                     maxLat +
                                     minLat,
                          data = res_temp, ntree = 1000,
                          keep.forest = FALSE,
                          importance = TRUE)

model2 <- randomForest(minTslope_RP ~ n_records + 
                                      rangeSize + 
                                      latAmplitude  +
                                      maxLat +
                                      minLat +
                                      order,
                      data = res_temp, ntree = 1000,
                      keep.forest = FALSE,
                      importance = TRUE)


### eliminate rows that did not have values for the response variable
res_temp2 <- res_temp[complete.cases(res_temp$one_SE_x_slope_minT_RP),]

model3 <- randomForest(one_SE_x_slope_minT_RP ~ n_records + 
                                                rangeSize + 
                                                latAmplitude  +
                                                maxLat +
                                                minLat +
                                                order,
                       data = res_temp2, ntree = 1000,
                       keep.forest = FALSE,
                       importance = TRUE)


varImpPlot(model2)

model$importanceSD
model$importance

model2$importanceSD
model2$importance

model3$importanceSD
model3$importance






