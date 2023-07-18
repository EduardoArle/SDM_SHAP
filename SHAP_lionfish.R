#load packages
library(caret);library(raster);library(xgboost);library(rgdal);library(rgeos)
library(rworldmap);library(rworldxtra);library(shapviz)

#list wds
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/GBIF'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/Bio-oracle'

#load lionfish occurrence data
setwd(wd_occ)
occ_all <- read.csv('Lessepsian_fish_GBIF_occurrences.csv')
occ_lionfish <- occ_all[which(occ_all$species == 'Pterois miles'),]
occ_lionfish <- occ_lionfish[complete.cases(occ_lionfish$decimalLongitude),]

#make a spatial object
coordinates(occ_lionfish) <- ~ decimalLongitude + decimalLatitude
occ_lionfish@data <- cbind(occ_lionfish@data, coordinates(occ_lionfish))

#create pseudo absences

#make a convex hull around points to determine the area of study
hull <- chull(coordinates(occ_lionfish))  #get points that make the chull
coords <- occ_lionfish[c(hull, hull[1]), ]  # closed polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coordinates(coords))), ID=1)))

#make a buffer around the area
st_area1 <- gBuffer(sp_poly, width = 1) # 1 degree

#cut the shapefile for ocean only
land <- getMap(resolution = 'high') #get shp of land
st_area2 <- gDifference(st_area1, land)

#make a buffer around occurrence points to set pseudo absences out of those limits
bf_pts <- gBuffer(occ_lionfish, width = 1)

#exclude these areas from the species buffered convex hull on the ocean
pa_area <- gDifference(st_area2, bf_pts)

#generate pseudo absences
pa <- spsample(pa_area, n = nrow(occ_lionfish), type = "random")

#make a table with presences and pseudo absences

#get coords of pa
pa_coords <- as.data.frame(coordinates(pa))
names(pa_coords) <- c('decimalLongitude', 'decimalLatitude')
pa_coords$Occurrence <- 0 #include column informing occ status

#get coords of pr
pr_coords <- as.data.frame(coordinates(occ_lionfish))
names(pr_coords) <- c('decimalLongitude', 'decimalLatitude')
pr_coords$Occurrence <- 1 #include column informing occ status

#concatenate
species <-rbind(pa_coords, pr_coords)

#make spatial obj again
coordinates(species) <- ~ decimalLongitude + decimalLatitude
species@data <- cbind(species@data, coordinates(species))

#visualise
plot(species[species$Occurrence == 1,], col='blue', pch=16) 
points(species[species$Occurrence == 0,], col='red', pch=16)

#load predictor variables (raster datasets)
setwd(wd_variables)

sst_mean <- raster('Present.Surface.Temperature.Mean.asc')
sss_mean <- raster('Present.Surface.Salinity.Mean.asc')
sso_min <- raster('Present.Surface.Dissolved.oxygen.Min.asc')
dcv_mean <- raster('Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1.asc')

# stack all variables
preds <- stack(sst_mean, sss_mean, sso_min, dcv_mean) 
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
                            Present.Surface.Salinity.Mean + 
                            Present.Surface.Dissolved.oxygen.Min + 
                            Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1, 
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
                'Present.Surface.Salinity.Mean', 
                'Present.Surface.Dissolved.oxygen.Min', 
                'Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1')
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

train$colour[train$shap_var == "Present.Surface.Salinity.Mean"] <- 'blue'
train$colour[train$shap_var == "Present.Surface.Dissolved.oxygen.Min"] <- 'red'
train$colour[train$shap_var == "Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1"] <- 'green'
train$colour[train$shap_var == "Present.Surface.Temperature.Mean"] <- 'orange'

world_simple <- getMap()
par(mar = c(0,0,0,0))

#plot all 

plot(world_simple, col = 'white', bg = 'grey80', border = NA)
plot(train, add = T, pch = 19, cex = 0.7, col = train$colour)

points(-175, -25, pch = 19, cex = 2, col = 'blue')
text(-140, -25, 'Surface Salinity Mean', cex = 2)

points(-175, -35, pch = 19, cex = 2, col = 'red')
text(-128, -35, 'Surface Dissolved oxygen Min', cex = 2)

points(-175, -45, pch = 19, cex = 2, col = 'green')
text(-128, -45, 'Benthic Current Velocity Mean', cex = 2)

points(-175, -55, pch = 19, cex = 2, col = 'orange')
text(-132, -55, 'Surface Temperature Mean', cex = 2)

#plot only presence

plot(world_simple, col = 'white', bg = 'grey80', border = NA)
plot(train[which(train$Occurrence == 1),], add = T, pch = 19, cex = 0.7, col = train$colour[which(train$Occurrence == 1)])

points(-175, -25, pch = 19, cex = 2, col = 'blue')
text(-140, -25, 'Surface Salinity Mean', cex = 2)

points(-175, -35, pch = 19, cex = 2, col = 'red')
text(-128, -35, 'Surface Dissolved oxygen Min', cex = 2)

points(-175, -45, pch = 19, cex = 2, col = 'green')
text(-128, -45, 'Benthic Current Velocity Mean', cex = 2)

points(-175, -55, pch = 19, cex = 2, col = 'orange')
text(-132, -55, 'Surface Temperature Mean', cex = 2)

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
