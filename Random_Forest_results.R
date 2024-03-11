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

# %IncMSE is the most robust and informative measure. It is the increase in mse of predictions(estimated with out-of-bag-CV) as a result of variable j being permuted(values randomly shuffled).

# IncNodePurity relates to the loss function which by best splits are chosen. The loss function is mse for regression and gini-impurity for classification. More useful variables achieve higher increases in node purities, that is to find a split which has a high inter node 'variance' and a small intra node 'variance'. IncNodePurity is biased and should only be used if the extra computation time of calculating %IncMSE is unacceptable. 

varImpPlot(model2)

model$importanceSD
model$importance

model2$importanceSD
model2$importance

model3$importanceSD
model3$importance


plot(res_temp$minTslope_RP, res_temp$n_records, pch = 19)
plot(res_temp$minTslope_RP, res_temp$latAmplitude, pch = 19)
plot(res_temp$minTslope_RP, res_temp$minLat, pch = 19)
plot(res_temp$minTslope_RP, res_temp$maxLat, pch = 19)


############################
######### MIN TEMP ########
############################


###### check relationships by latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[which
                (res_temp_large_ranges$latAmplitude >= 0),]

pos_latAmp_all <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                          nrow(res_temp_large_ranges2) * 100
neg_latAmp_all <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                          nrow(res_temp_large_ranges2) * 100

#### more than 5 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[which
                 (res_temp_large_ranges$latAmplitude >= 5),]

pos_latAmp_5 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                        nrow(res_temp_large_ranges2) * 100
neg_latAmp_5 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                        nrow(res_temp_large_ranges2) * 100

#### more than 10 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[which
                  (res_temp_large_ranges$latAmplitude >= 10),]

pos_latAmp_10 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_latAmp_10 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 20 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[which
                   (res_temp_large_ranges$latAmplitude >= 20),]

pos_latAmp_20 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_latAmp_20 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100


dt_latAmp <- data.frame(all = c(pos_latAmp_all, neg_latAmp_all),
                        min_5 = c(pos_latAmp_5, neg_latAmp_5),
                        min_10 = c(pos_latAmp_10, neg_latAmp_10),
                        min_20 = c(pos_latAmp_20, neg_latAmp_20))

row.names(dt_latAmp) <- c('Positive', 'Negative')

dt_latAmp <-as.matrix(dt_latAmp)    

  
barplot(dt_latAmp,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                             nrow(res_temp_large_ranges2) * 100
neg_rangeSize_all <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                             nrow(res_temp_large_ranges2) * 100

#### more than 100000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 100000),]

pos_rangeSize_100000 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                                nrow(res_temp_large_ranges2) * 100
neg_rangeSize_100000 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                                nrow(res_temp_large_ranges2) * 100

#### more than 500000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 500000),]

pos_rangeSize_500000 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                                nrow(res_temp_large_ranges2) * 100
neg_rangeSize_500000 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                                nrow(res_temp_large_ranges2) * 100

#### more than 1000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 1000000),]

pos_rangeSize_1000000 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                                 nrow(res_temp_large_ranges2) * 100
neg_rangeSize_1000000 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                                 nrow(res_temp_large_ranges2) * 100

#### more than 2000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 2000000),]

pos_rangeSize_2000000 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                                 nrow(res_temp_large_ranges2) * 100
neg_rangeSize_2000000 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                                 nrow(res_temp_large_ranges2) * 100

#### more than 3000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 3000000),]

pos_rangeSize_3000000 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                                 nrow(res_temp_large_ranges2) * 100
neg_rangeSize_3000000 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                                 nrow(res_temp_large_ranges2) * 100


dt_rangeSize <- data.frame(all = c(pos_rangeSize_all, neg_rangeSize_all),
                min_100K = c(pos_rangeSize_100000, neg_rangeSize_100000),
                min_500K = c(pos_rangeSize_500000, neg_rangeSize_500000),
                min_1000K = c(pos_rangeSize_1000000, neg_rangeSize_1000000),
                min_2000K = c(pos_rangeSize_2000000, neg_rangeSize_2000000),
                min_3000K = c(pos_rangeSize_3000000, neg_rangeSize_3000000))

row.names(dt_rangeSize) <- c('Positive', 'Negative')

dt_rangeSize <-as.matrix(dt_rangeSize)    


barplot(dt_rangeSize,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by min lat

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_RP != 0),]

#### all min lats
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 0),]

pos_minLat_all <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                          nrow(res_temp_large_ranges2) * 100
neg_minLat_all <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                          nrow(res_temp_large_ranges2) * 100

#### more than 5 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 5),]

pos_minLat_5 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                        nrow(res_temp_large_ranges2) * 100
neg_minLat_5 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                        nrow(res_temp_large_ranges2) * 100

#### more than 10 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 10),]

pos_minLat_10 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_minLat_10 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 20 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 20),]

pos_minLat_20 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_minLat_20 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 30 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 30),]

pos_minLat_30 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_minLat_30 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 50 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 50),]

pos_minLat_50 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_minLat_50 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100


dt_minLat <- data.frame(all = c(pos_minLat_all, neg_minLat_all),
                           min_5 = c(pos_minLat_5, neg_minLat_5),
                           min_10 = c(pos_minLat_10, neg_minLat_10),
                           min_20 = c(pos_minLat_20, neg_minLat_20),
                           min_30 = c(pos_minLat_30, neg_minLat_30),
                           min_50 = c(pos_minLat_50, neg_minLat_50))

row.names(dt_minLat) <- c('Positive', 'Negative')

dt_minLat <-as.matrix(dt_minLat)    


barplot(dt_minLat,
        col = c("blue", "red"),
        legend = NA)


###### check relationships by max lat

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_RP != 0),]

#### all min lats
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 0),]

pos_maxLat_all <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                          nrow(res_temp_large_ranges2) * 100
neg_maxLat_all <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                          nrow(res_temp_large_ranges2) * 100

#### more than 5 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 5),]

pos_maxLat_5 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                        nrow(res_temp_large_ranges2) * 100
neg_maxLat_5 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                        nrow(res_temp_large_ranges2) * 100

#### more than 10 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 10),]

pos_maxLat_10 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_maxLat_10 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 20 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 20),]

pos_maxLat_20 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_maxLat_20 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 30 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 30),]

pos_maxLat_30 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_maxLat_30 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100

#### more than 50 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 50),]

pos_maxLat_50 <- sum(res_temp_large_ranges2$minTslope_RP > 0) /
                         nrow(res_temp_large_ranges2) * 100
neg_maxLat_50 <- sum(res_temp_large_ranges2$minTslope_RP < 0) /
                         nrow(res_temp_large_ranges2) * 100


dt_maxLat <- data.frame(all = c(pos_maxLat_all, neg_maxLat_all),
                        max_5 = c(pos_maxLat_5, neg_maxLat_5),
                        max_10 = c(pos_maxLat_10, neg_maxLat_10),
                        max_20 = c(pos_maxLat_20, neg_maxLat_20),
                        max_30 = c(pos_maxLat_30, neg_maxLat_30),
                        max_50 = c(pos_maxLat_50, neg_maxLat_50))

row.names(dt_maxLat) <- c('Positive', 'Negative')

dt_maxLat <-as.matrix(dt_maxLat)    


barplot(dt_maxLat,
        col = c("blue", "red"),
        legend = NA)





############################
######### MEAN TEMP ########
############################



###### check relationships by latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 0),]

pos_latAmp_all <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_all <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 5 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 5),]

pos_latAmp_5 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_5 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 10 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 10),]

pos_latAmp_10 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_10 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 20 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 20),]

pos_latAmp_20 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_20 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_latAmp <- data.frame(all = c(pos_latAmp_all, neg_latAmp_all),
                        min_5 = c(pos_latAmp_5, neg_latAmp_5),
                        min_10 = c(pos_latAmp_10, neg_latAmp_10),
                        min_20 = c(pos_latAmp_20, neg_latAmp_20))

row.names(dt_latAmp) <- c('Positive', 'Negative')

dt_latAmp <-as.matrix(dt_latAmp)    


barplot(dt_latAmp,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_all <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 100000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 100000),]

pos_rangeSize_100000 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_100000 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 500000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 500000),]

pos_rangeSize_500000 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_500000 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 1000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 1000000),]

pos_rangeSize_1000000 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_1000000 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 2000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 2000000),]

pos_rangeSize_2000000 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_2000000 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 3000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 3000000),]

pos_rangeSize_3000000 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_3000000 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_rangeSize <- data.frame(all = c(pos_rangeSize_all, neg_rangeSize_all),
                  min_100K = c(pos_rangeSize_100000, neg_rangeSize_100000),
                  min_500K = c(pos_rangeSize_500000, neg_rangeSize_500000),
                  min_1000K = c(pos_rangeSize_1000000, neg_rangeSize_1000000),
                  min_2000K = c(pos_rangeSize_2000000, neg_rangeSize_2000000),
                  min_3000K = c(pos_rangeSize_3000000, neg_rangeSize_3000000))

row.names(dt_rangeSize) <- c('Positive', 'Negative')

dt_rangeSize <-as.matrix(dt_rangeSize)    


barplot(dt_rangeSize,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by min lat

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_RP != 0),]

#### all min lats
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 0),]

pos_minLat_all <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_all <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 5 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 5),]

pos_minLat_5 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_5 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 10 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 10),]

pos_minLat_10 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_10 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 20 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 20),]

pos_minLat_20 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_20 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 30 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 30),]

pos_minLat_30 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_30 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 50 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 50),]

pos_minLat_50 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_50 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_minLat <- data.frame(all = c(pos_minLat_all, neg_minLat_all),
                        min_5 = c(pos_minLat_5, neg_minLat_5),
                        min_10 = c(pos_minLat_10, neg_minLat_10),
                        min_20 = c(pos_minLat_20, neg_minLat_20),
                        min_30 = c(pos_minLat_30, neg_minLat_30),
                        min_50 = c(pos_minLat_50, neg_minLat_50))

row.names(dt_minLat) <- c('Positive', 'Negative')

dt_minLat <-as.matrix(dt_minLat)    


barplot(dt_minLat,
        col = c("blue", "red"),
        legend = NA)


###### check relationships by max lat

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_RP != 0),]

#### all min lats
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 0),]

pos_maxLat_all <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_all <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 5 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 5),]

pos_maxLat_5 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_5 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 10 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 10),]

pos_maxLat_10 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_10 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 20 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 20),]

pos_maxLat_20 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_20 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 30 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 30),]

pos_maxLat_30 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_30 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 50 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 50),]

pos_maxLat_50 <- sum(res_temp_large_ranges2$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_50 <- sum(res_temp_large_ranges2$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_maxLat <- data.frame(all = c(pos_maxLat_all, neg_maxLat_all),
                        max_5 = c(pos_maxLat_5, neg_maxLat_5),
                        max_10 = c(pos_maxLat_10, neg_maxLat_10),
                        max_20 = c(pos_maxLat_20, neg_maxLat_20),
                        max_30 = c(pos_maxLat_30, neg_maxLat_30),
                        max_50 = c(pos_maxLat_50, neg_maxLat_50))

row.names(dt_maxLat) <- c('Positive', 'Negative')

dt_maxLat <-as.matrix(dt_maxLat)    


barplot(dt_maxLat,
        col = c("blue", "red"),
        legend = NA)






############################
######### MAX TEMP #########
############################



###### check relationships by latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 0),]

pos_latAmp_all <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_all <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 5 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 5),]

pos_latAmp_5 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_5 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 10 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 10),]

pos_latAmp_10 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_10 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 20 degrees
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 20),]

pos_latAmp_20 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_latAmp_20 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_latAmp <- data.frame(all = c(pos_latAmp_all, neg_latAmp_all),
                        min_5 = c(pos_latAmp_5, neg_latAmp_5),
                        min_10 = c(pos_latAmp_10, neg_latAmp_10),
                        min_20 = c(pos_latAmp_20, neg_latAmp_20))

row.names(dt_latAmp) <- c('Positive', 'Negative')

dt_latAmp <-as.matrix(dt_latAmp)    


barplot(dt_latAmp,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_all <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 100000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 100000),]

pos_rangeSize_100000 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_100000 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 500000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 500000),]

pos_rangeSize_500000 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_500000 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 1000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 1000000),]

pos_rangeSize_1000000 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_1000000 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 2000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 2000000),]

pos_rangeSize_2000000 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_2000000 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 3000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 3000000),]

pos_rangeSize_3000000 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_3000000 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_rangeSize <- data.frame(all = c(pos_rangeSize_all, neg_rangeSize_all),
                min_100K = c(pos_rangeSize_100000, neg_rangeSize_100000),
                min_500K = c(pos_rangeSize_500000, neg_rangeSize_500000),
                min_1000K = c(pos_rangeSize_1000000, neg_rangeSize_1000000),
                min_2000K = c(pos_rangeSize_2000000, neg_rangeSize_2000000),
                min_3000K = c(pos_rangeSize_3000000, neg_rangeSize_3000000))

row.names(dt_rangeSize) <- c('Positive', 'Negative')

dt_rangeSize <-as.matrix(dt_rangeSize)    


barplot(dt_rangeSize,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by min lat

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_RP != 0),]

#### all min lats
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 0),]

pos_minLat_all <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_all <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 5 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 5),]

pos_minLat_5 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_5 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 10 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 10),]

pos_minLat_10 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_10 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 20 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 20),]

pos_minLat_20 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_20 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 30 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 30),]

pos_minLat_30 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_30 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 50 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$minLat >= 50),]

pos_minLat_50 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_minLat_50 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_minLat <- data.frame(all = c(pos_minLat_all, neg_minLat_all),
                        min_5 = c(pos_minLat_5, neg_minLat_5),
                        min_10 = c(pos_minLat_10, neg_minLat_10),
                        min_20 = c(pos_minLat_20, neg_minLat_20),
                        min_30 = c(pos_minLat_30, neg_minLat_30),
                        min_50 = c(pos_minLat_50, neg_minLat_50))

row.names(dt_minLat) <- c('Positive', 'Negative')

dt_minLat <-as.matrix(dt_minLat)    


barplot(dt_minLat,
        col = c("blue", "red"),
        legend = NA)


###### check relationships by max lat

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_RP != 0),]

#### all min lats
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 0),]

pos_maxLat_all <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_all <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 5 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 5),]

pos_maxLat_5 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_5 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 10 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 10),]

pos_maxLat_10 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_10 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 20 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 20),]

pos_maxLat_20 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_20 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 30 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 30),]

pos_maxLat_30 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_30 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 50 degress
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$maxLat >= 50),]

pos_maxLat_50 <- sum(res_temp_large_ranges2$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_maxLat_50 <- sum(res_temp_large_ranges2$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_maxLat <- data.frame(all = c(pos_maxLat_all, neg_maxLat_all),
                        max_5 = c(pos_maxLat_5, neg_maxLat_5),
                        max_10 = c(pos_maxLat_10, neg_maxLat_10),
                        max_20 = c(pos_maxLat_20, neg_maxLat_20),
                        max_30 = c(pos_maxLat_30, neg_maxLat_30),
                        max_50 = c(pos_maxLat_50, neg_maxLat_50))

row.names(dt_maxLat) <- c('Positive', 'Negative')

dt_maxLat <-as.matrix(dt_maxLat)    


barplot(dt_maxLat,
        col = c("blue", "red"),
        legend = NA)


7h59