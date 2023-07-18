#load packages
library(caret);library(raster);library(xgboost);library(rgdal);library(rgeos)
library(rworldmap);library(rworldxtra)

#list wds
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/GBIF'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/Bio-oracle'
wd_results <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Lessepsian_fish/Results'

#load all Lessepsian fishes' occurrence data from GBIF
setwd(wd_occ)
occ_all <- read.csv('Lessepsian_fish_GBIF_occurrences.csv')

#split the data per species
species_occ_0 <- split(occ_all, f = occ_all$species)

#select species with at least 25 presence points
species_occ <- species_occ_0[sapply(species_occ_0, function(x) { 
                      nrow(x[which(x$occurrenceStatus == 'PRESENT'),]) > 25 })]
  

###### COUNT PRESENCES AND ABSENCES PER SPECIES #####


# Check species presence and absence numbers

#create empty numeric vectors to allocate presences and absences numbers
n_pr <- numeric()
n_abs <- numeric()

for(i in 1:length(species_occ))
{
  #select the species of the loop
  sps <- species_occ[[i]]
  
  #eliminate entries without coordinates
  sps2 <- sps[complete.cases(sps$decimalLongitude, sps$decimalLatitude),]
  
  #split the data into presence and absence
  sps_pr <- sps2[sps2$occurrenceStatus == 'PRESENT',]
  sps_abs <- sps2[sps2$occurrenceStatus == 'ABSENT',]
  
  #populate the vectors with number of presences and absences
  n_pr[i] <- nrow(sps_pr)
  n_abs[i] <- nrow(sps_abs)
}

#make a table with numbers of presences and absences per species
n_pr_abs <- data.frame(Species = names(species_occ), Presences = n_pr, Absences = n_abs)

#create a sub directory in wd_results to save tables
dir.create(paste0(wd_results, '/Tables'))
setwd(paste0(wd_results, '/Tables'))
write.csv(n_pr_abs, 'Species_occurrence_count.csv', row.names = F)



###### RUN SCRIPT TO PRODUCE SHAP MAPS FOR EACH SPECIES MAKING INDIVIDUAL DECISIONS #####


for(i in 1:length(species_occ))
{
  #select the species of the loop
  sps <- species_occ[[i]]
  
  #eliminate entries without coordinates
  sps2 <- sps[complete.cases(sps$decimalLongitude, sps$decimalLatitude),]
  
  #make a spatial object
  coordinates(sps2) <- ~ decimalLongitude + decimalLatitude
  sps2@data <- cbind(sps2@data, coordinates(sps2))
  
  #split the data into presence and absence
  sps_pr <- sps2[sps2$occurrenceStatus == 'PRESENT',]
  sps_abs <- sps2[sps2$occurrenceStatus == 'ABSENT',]
  
  #get rate of abs / pr
  rate <- nrow(sps_abs) / nrow(sps_pr)
  
  #create NA obj for pa_coords and abs_coords
  pa_coords <- NA
  abs_coords <- NA
  
  #if the rate of abs / pr < 0.5, create pseudo abs to get to this value
  if(rate < 0.5){
    
    #CREATE PSEUDO ABSENCES TO COMPLETE 50% OF THE NUMBER OF PRESENCES
    
    #make a big buffer around the points to delimit the area where I want pseudo abs
    big_buffer <- gBuffer(sps_pr, width = 5) # 1 degree
    
    #make a small buffer around the points to delimit the area where I don't want pseudo abs
    small_buffer <- gBuffer(sps_pr, width = 1) # 1 degree
    
    #make a who in the big buffer by the small buffer
    pa_area <- gDifference(big_buffer, small_buffer)
    
    #cut the shapefile for ocean only
    land <- getMap(resolution = 'high') #get shp of land
    pa_area2 <- gDifference(pa_area, land)
    
    #define number of pseudo abs to be created (abs + pseudo abs = presence / 2)
    n_pa <-  round(nrow(sps_pr) / 2 - nrow(sps_abs))
    
    #generate pseudo absences
    pa <- spsample(pa_area2, n = n_pa, type = "random")
    
    #get coords of pa
    pa_coords <- as.data.frame(coordinates(pa))
    names(pa_coords) <- c('decimalLongitude', 'decimalLatitude')
    pa_coords$Occurrence <- 0 #include column informing occ status
    pa_coords$Type <- 'Pseudo-absence'
  }
  
  #if the rate of abs / pr > 0.5, thin the abs to get to this value
  if(rate > 0.5){
    
    #select random rows to get down to the desired number of abs
    rows_thin <-  sample(c(1:nrow(sps_abs)), round(0.5 * nrow(sps_pr)))
    
    #substitute the obj sps_abs for the thined version
    sps_abs <- sps_abs[rows_thin,]
    
    abs_coords <- as.data.frame(coordinates(sps_abs))
    names(abs_coords) <- c('decimalLongitude', 'decimalLatitude')
    abs_coords$Occurrence <- 0 #include column informing occ status
    abs_coords$Type <- 'Absence'
  }
  
  #if the rate of abs / pr < 0.5 and > 0, get all abs
  if(rate > 0 & rate < 0.5){
    
    abs_coords <- as.data.frame(coordinates(sps_abs))
    names(abs_coords) <- c('decimalLongitude', 'decimalLatitude')
    abs_coords$Occurrence <- 0 #include column informing occ status
    abs_coords$Type <- 'Absence'
  }
  
  #get coords of pr
  pr_coords <- as.data.frame(coordinates(sps_pr))
  names(pr_coords) <- c('decimalLongitude', 'decimalLatitude')
  pr_coords$Occurrence <- 1 #include column informing occ status
  pr_coords$Type <- 'Presence'
  
  #create obj for species occ with pr_coords
  species <- pr_coords
  
  #include pseudo absences (if available)
  if(class(pa_coords) == 'data.frame'){
    species <- rbind(species, pa_coords)
  }
  
  #include absences (if available)
  if(class(abs_coords) == 'data.frame'){
    species <- rbind(species, abs_coords)
  }

  
}


#check abs pr rates

abs <- sapply(species_occ, function(x){length(which(x$occurrenceStatus == 'ABSENT')) / 
                                       length(which(x$occurrenceStatus == 'PRESENT'))})

table(abs)

#make spatial obj again
coordinates(species) <- ~ decimalLongitude + decimalLatitude
species@data <- cbind(species@data, coordinates(species))

#visualise
plot(species[species$Occurrence == 1,], col='blue', pch=16, add =T) 
points(species[species$Occurrence == 0,], col='red', pch=16)

#load predictor variables (raster datasets)
setwd(wd_variables)

sst_mean <- raster('Present.Surface.Temperature.Mean.asc')
sst_max <- raster('Present.Surface.Temperature.Max.asc')
sst_min <- raster('Present.Surface.Temperature.Min.asc')

# stack all variables
preds <- stack(sst_mean, sst_max, sst_min) 
preds # see the specification of the raster layers (e.g., cell size, extent, etc.)

######## NOW RUN RANDOM FOREST ##########

#extract values from each location from all variables and make a table
vals_pts <- extract(preds, species)
tab_occ_vars <- cbind(species@data, vals_pts)

#split data into train and test
rows_train <- sample(c(1:nrow(tab_occ_vars)), round(0.7 * nrow(tab_occ_vars)))

train <- tab_occ_vars[rows_train,]
test <- tab_occ_vars[-rows_train,]

#summarise to check for NAs
summary(train) # NAs
summary(test) # NAs

#eliminate points having NAs
train <- train[complete.cases(train$Present.Surface.Temperature.Mean),]
test <- test[complete.cases(test$Present.Surface.Temperature.Mean),]

#set a random seed
set.seed(51)

#use 5 folds for cross-validation
model <- train(Occurrence ~ Present.Surface.Temperature.Mean + 
                 Present.Surface.Temperature.Min + 
                 Present.Surface.Temperature.Max, 
               data = train, 
               method = 'rf',
               trControl = trainControl(method = 'cv'), 
               number = 10) 

#test the model
test$prediction <- predict(model, newdata = test)

#calculate the AUC of the model
AUC <- auc(test$Occurrence, test$prediction)

#calculate the RMSE of the model
RMSE <- sqrt((sum(test$prediction - test$Occurrence)^2)/nrow(test))

##### project predictions in the map #####

#create an ID raster
ID_raster <-  preds[[1]]
ID_raster[which(!is.na(ID_raster[]))] <- which(!is.na(ID_raster[]))

#create one spatial point per non NA cell in ID raster
ID_pts <- as.data.frame(rasterToPoints(ID_raster))
names(ID_pts)[3] <- 'cellID'
coordinates(ID_pts) <- ~ x + y

#extract values for each cell of the study area
vars_all <- as.data.frame(extract(preds, ID_pts))
vars_all$ID_cell <- ID_pts$cellID

#eliminate NAs
vars_all <- vars_all[complete.cases(vars_all$Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1),]

#make prediction for all cells
vars_all$pred <- predict(model, newdata = vars_all)

#project
RF_proj <- ID_raster
RF_proj[] <- NA
RF_proj[vars_all$ID_cell] <- vars_all$pred

plot(RF_proj)

###### SHAP #####

# Fit XGBoost model
pred_vars <- c('Present.Surface.Temperature.Mean', 
               'Present.Surface.Temperature.Min', 
               'Present.Surface.Temperature.Max')
shap_train <- xgb.DMatrix(data.matrix(train[pred_vars]), label = train$Occurrence)

fit <- xgb.train(
  params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
  data = shap_train,
  nrounds = 65L
)


# We also pass feature data X with originally encoded values
shp <- shapviz(fit, X_pred = data.matrix(train[pred_vars]), X = train)

# get the highest absolute SHAP value for each prediction
shap <- numeric()
shap_var <- character()

for(i in 1:nrow(train))
{
  obj <- sv_waterfall(shp, row_id = i) +
    theme(axis.text = element_text(size = 11))
  
  a <- which.max(abs(obj$data$S)) #get the highest absolute value
  
  shap[i] <- obj$data$S[a]
  shap_var[i] <- row.names(obj$data)[a]
}

train$shap_var <- shap_var
train$shap <- shap

##### plot occurrence points by contributing variables
coordinates(train) <- ~ decimalLongitude + decimalLatitude
train$colour <- 'xx'

train$colour[train$shap_var == "Present.Surface.Temperature.Mean"] <- 'blue'
train$colour[train$shap_var == "Present.Surface.Temperature.Min"] <- 'red'
train$colour[train$shap_var == "Present.Surface.Temperature.Max"] <- 'orange'
        
      world_simple <- getMap()
      par(mar = c(0,0,0,0))
      
      #plot all 
      
      plot(world_simple, col = 'white', bg = 'grey80', border = NA)
      plot(train, add = T, pch = 19, cex = 0.7, col = train$colour)
      
      points(-175, -25, pch = 19, cex = 2, col = 'blue')
      text(-140, -25, 'Present.Surface.Temperature.Mean', cex = 2)
      
      points(-175, -35, pch = 19, cex = 2, col = 'red')
      text(-128, -35, 'Present.Surface.Temperature.Min', cex = 2)
      
      points(-175, -55, pch = 19, cex = 2, col = 'orange')
      text(-132, -55, 'Present.Surface.Temperature.Max', cex = 2)
      
      #plot only presence
      
      plot(world_simple, col = 'white', bg = 'grey80', border = NA)
      plot(train[which(train$Occurrence == 1),], add = T, pch = 19, cex = 0.7, col = train$colour[which(train$Occurrence == 1)])
      
      points(-175, -25, pch = 19, cex = 2, col = 'blue')
      text(-157, -25, 'Present.Surface.Temperature.Mean', pos = 4, cex = 2)
      
      points(-175, -35, pch = 19, cex = 2, col = 'red')
      text(-157, -35, 'Present.Surface.Temperature.Min', pos = 4, cex = 2)
      
      points(-175, -45, pch = 19, cex = 2, col = 'orange')
      text(-157, -45, 'Present.Surface.Temperature.Max', pos = 4, cex = 2)
      
      #plot only absence
      
      plot(world_simple, col = 'white', bg = 'grey80', border = NA)
      plot(train[which(train$Occurrence == 0),], add = T, pch = 19, cex = 0.7, col = train$colour[which(train$Occurrence == 0)])
      
      points(-175, -25, pch = 19, cex = 2, col = 'blue')
      text(-140, -25, 'Surface Salinity Mean', cex = 2)
      
      points(-175, -35, pch = 19, cex = 2, col = 'red')
      text(-128, -35, 'Surface Dissolved oxygen Min', cex = 2)
      
      points(-175, -45, pch = 19, cex = 2, col = 'green')
      text(-128, -45, 'Benthic Current Velocity Mean', cex = 2)
      
      points(-175, -55, pch = 19, cex = 2, col = 'orange')
      text(-132, -55, 'Surface Temperature Mean', cex = 2)
      
      
      #plot only SHAP positive
      
      plot(world_simple, col = 'white', bg = 'grey80', border = NA)
      plot(train[which(train$shap > 0),], add = T, pch = 19, cex = 0.7, col = train$colour[which(train$shap > 0)])
      
      points(-175, -25, pch = 19, cex = 2, col = 'blue')
      text(-140, -25, 'Surface Salinity Mean', cex = 2)
      
      points(-175, -35, pch = 19, cex = 2, col = 'red')
      text(-128, -35, 'Surface Dissolved oxygen Min', cex = 2)
      
      points(-175, -45, pch = 19, cex = 2, col = 'green')
      text(-128, -45, 'Benthic Current Velocity Mean', cex = 2)
      
      points(-175, -55, pch = 19, cex = 2, col = 'orange')
      text(-132, -55, 'Surface Temperature Mean', cex = 2)
      
      
      #plot only SHAP negative
      
      plot(world_simple, col = 'white', bg = 'grey80', border = NA)
      plot(train[which(train$shap < 0),], add = T, pch = 19, cex = 0.7, col = train$colour[which(train$shap < 0)])
      
      points(-175, -25, pch = 19, cex = 2, col = 'blue')
      text(-140, -25, 'Surface Salinity Mean', cex = 2)
      
      points(-175, -35, pch = 19, cex = 2, col = 'red')
      text(-128, -35, 'Surface Dissolved oxygen Min', cex = 2)
      
      points(-175, -45, pch = 19, cex = 2, col = 'green')
      text(-128, -45, 'Benthic Current Velocity Mean', cex = 2)
      
      points(-175, -55, pch = 19, cex = 2, col = 'orange')
      text(-132, -55, 'Surface Temperature Mean', cex = 2)
      
      ##### plot positive and negative shap separately indicating whether each point is pr or abs
      plot(preds[[1]], col = 'grey80', legend = F, box = F, axes = F)
      plot(train[which(train$shap > 0),], add = T, pch = 19, col = train$colour[which(train$shap > 0)])
      plot(train[which(train$shap < 0),], add = T, pch = 15, col = train$colour[which(train$shap < 0)])
      
      #####################
      
      # We also pass feature data X with originally encoded values
      shp <- shapviz(model, X_pred = test[,c(2:5)], X = test)
      
      shp <- shapviz(fit, X_pred = data.matrix(dia_small[x]), X = dia_small)
      
      
      
############################################################################################
      
#SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP  #SCRAP
      
############################################################################################
      
## include a modified part of the script (in the species that have real absences), a sensitivity analysis for the rate of real absences per pseudo-absences. Always respecting half the number of presence points
      