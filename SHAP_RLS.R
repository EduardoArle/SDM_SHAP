#load packages
library(caret);library(raster);library(xgboost);library(rgdal);library(rgeos)
library(rworldmap);library(rworldxtra);library(shapviz);library(kernelshap)
library(data.table);library(maptools)

#list wds
wd_occ <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Reef_Life_Survey'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/CNA marine/Data/Bio-oracle'
wd_results <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Reef_Life_Survey/Results'
wd_res_species <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Reef_Life_Survey/Results/Species'
wd_sps_ranges <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Reef_Life_Survey/Results/Species_ranges'
wd_sps_ranges_land <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Reef_Life_Survey/Results/Species_ranges_land'
wd_results_analysis <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Reef_Life_Survey/Results_variable_importance'

#load predictor variables (raster datasets)
setwd(wd_variables)

sst_mean <- raster('Present.Surface.Temperature.Mean.asc')
sst_max <- raster('Present.Surface.Temperature.Max.asc')
sst_min <- raster('Present.Surface.Temperature.Min.asc')
sss_mean <- raster('Present.Surface.Salinity.Mean.asc')
sso_min <- raster('Present.Surface.Dissolved.oxygen.Min.asc')
dcv_mean <- raster('Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1.asc')

# stack all variables
preds <- stack(sst_mean, sst_max, sst_min, sss_mean, sso_min, dcv_mean) 

#make ID raster to thin points by variable resolution
ID_raster <- sst_mean
ID_raster[] <- c(1:length(ID_raster))

#load all Lessepsian fishes' occurrence data from GBIF
setwd(wd_occ)
occ_all <- read.csv('RLSreeffishdataset.csv')

#split the data per species
species_occ_0 <- split(occ_all, f = occ_all$Taxon)

#count entries per species
sps_occ_n <- sapply(species_occ_0, nrow)

#empty vector to include only the number of entries after thinned 
sps_occ_thin <- numeric()

#empty list to include only the data on species with more than 25 thinned entries
species_occ <- list()

#thin points and select species with at least 25 presence points
for(i in 1:length(species_occ_0))
{
  #create a spatial object of each species occ
  a <- species_occ_0[[i]]
  coordinates(a) <- ~ SiteLong + SiteLat
  
  #get cellID value
  b <- extract(ID_raster, a)
  
  #include cell value and coordinates into species data
  a@data <- cbind(a@data, coordinates(a))
  a$cellID <- b
  
  #keep only one entry per cell
  c <- unique(as.data.table(a@data), by = 'cellID')
  
  #include count of unique entries in the vector
  sps_occ_thin[i] <- nrow(c)
  
  #if the species has more than 25 entries, include it in the list
  if(nrow(c) >= 25){
    species_occ[[length(species_occ) + 1]] <- c
     
    #name the item in the list
    names(species_occ)[length(species_occ)] <- names(species_occ_0)[i]
  }
  
  print(i)
}
  
#make table with entries per species before and after thinning
occ_count <- data.frame(Species = names(species_occ_0),
                        Records = sps_occ_n,
                        Thinned_records = sps_occ_thin)

#save table occ_count
setwd(wd_results)
write.csv(occ_count, 'Occurrence_all_thin.csv', row.names = F)

#count species
sps_n <- length(species_occ_0) #all = 2367
sps_sel <- length(species_occ) #selected = 421

#get world map
land <- getMap(resolution = 'high') #get shp of land

#make one feature for all land
land <- gUnaryUnion(land)

# calculate the variable contribution for each point of each species

for(i in 1:length(species_occ))
{
  #select the species of the loop
  sps <- species_occ[[i]]
  
  #make a spatial object
  coordinates(sps) <- ~ SiteLong + SiteLat
  sps@data <- cbind(sps@data, coordinates(sps))
    
  #CREATE PSEUDO ABSENCES 50% OF THE NUMBER OF PRESENCES
    
  #make a big buffer around the points to delimit the area where I want pseudo abs
  big_buffer <- gBuffer(sps, width = 5) # 1 degree
    
  #make a small buffer around the points to delimit the area where I don't want pseudo abs
  small_buffer <- gBuffer(sps, width = 1) # 1 degree
    
  #make a who in the big buffer by the small buffer
  pa_area <- gDifference(big_buffer, small_buffer)
    
  #cut the shapefile for ocean only
  pa_area2 <- gDifference(pa_area, land)
    
  #define number of pseudo abs to be created (abs + pseudo abs = presence / 2)
  n_pa <-  round(nrow(sps) / 2)
    
  #generate pseudo absences
  pa <- spsample(pa_area2, n = n_pa, type = "random")
    
  #get coords of pa
  pa_coords <- as.data.frame(coordinates(pa))
  names(pa_coords) <- c('decimalLongitude', 'decimalLatitude')
  pa_coords$Occurrence <- 0 #include column informing occ status
  pa_coords$Type <- 'Pseudo-absence'

  #get coords of pr
  pr_coords <- as.data.frame(coordinates(sps))
  names(pr_coords) <- c('decimalLongitude', 'decimalLatitude')
  pr_coords$Occurrence <- 1 #include column informing occ status
  pr_coords$Type <- 'Presence'
  
  #combine pseudo absences and presences
  species <- rbind(pr_coords, pa_coords)

  #make spatial obj again
  coordinates(species) <- ~ decimalLongitude + decimalLatitude
  species@data <- cbind(species@data, coordinates(species))
  
  ###### SHAP #####
  
  #extract values from each location from all variables and make a table
  vals_pts <- extract(preds, species)
  tab_occ_vars <- cbind(species@data, vals_pts)

  # Fit XGBoost model
  pred_vars <- c('Present.Surface.Temperature.Mean', 
                 'Present.Surface.Temperature.Min', 
                 'Present.Surface.Temperature.Max',
                 'Present.Surface.Salinity.Mean', 
                 'Present.Surface.Dissolved.oxygen.Min', 
                 'Present.Benthic.Max.Depth.Current.Velocity.Mean.asc.BOv2_1')
  
  shap <- xgb.DMatrix(data.matrix(tab_occ_vars[pred_vars]), label = tab_occ_vars$Occurrence)
  
  fit <- xgb.train(
    params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
    data = shap,
    nrounds = 65L
  )
  
  # We also pass feature data X with originally encoded values
  shp <- shapviz(fit, X_pred = data.matrix(tab_occ_vars[pred_vars]), X = tab_occ_vars)
  
  # get the highest absolute SHAP value for each prediction
  shap_var <- character()
  
  # get the temperature variable contributing the most
  shap_var_temp <- character()
  
  for(j in 1:nrow(tab_occ_vars))
  {
    obj <- sv_waterfall(shp, row_id = j) +
      theme(axis.text = element_text(size = 11))
    
    a <- which.max(abs(obj$data$S)) #get the highest absolute value
    
    shap_var[j] <- row.names(obj$data)[a]
    
    #select only temperature variables
    res_temp <- obj$data[grep('Temperature', row.names(obj$data)),]
    
    b <- which.max(abs(res_temp$S)) #get the highest absolute value for temp
    shap_var_temp[j] <- row.names(res_temp)[b]
  }
  
  tab_occ_vars$shap_var <- shap_var
  tab_occ_vars$shap_var_temp <- shap_var_temp
  
  #save results per species
  setwd(wd_res_species)
  write.csv(tab_occ_vars, paste0(names(species_occ)[i],'.csv'), row.names = F)
}

#Visualise

# plot(world, border = NA, col = 'grey80')
# plot(big_buffer, add = T)
# plot(sps, add = T, col = 'orange', pch = 19)
# plot(species, add = T, col = 'red', pch = 19)
# plot(species[which(species$Occurrence == 1),], add = T, col = 'green', pch = 19)

#loop through the species results and look for patterns

setwd(wd_res_species)
species_list <- list.files()

for(i in 236:length(species_list))
{
  setwd(wd_res_species)
  sps <- read.csv(species_list[i])
  
  #define the species range or ranges
  sps_sp <- sps
  coordinates(sps_sp) <- ~ decimalLongitude + decimalLatitude
  sps_sp@data <- cbind(sps_sp@data, coordinates(sps_sp))
  
  #select only presences
  sps_sp_pr <- sps_sp[sps_sp$Occurrence == 1,]
  
  #group points by ranges considerind contiguous with max dist of c. 1110 km (10 degrees)
  
  #make a 5 degree buffer around only presences, keeping individual features
  buf_pr <- gBuffer(sps_sp_pr, byid = T, width = 10)
  
  #create an ID for each buffer around the points
  buf_pr$ID <- c(1:nrow(buf_pr))
  
  #seed stratified points in all the regions
  pts <- spsample(buf_pr, type = "stratified")
  
  #extract ID values in each point
  ID_values <- over(pts, buf_pr, returnList = T)
  
  #select only the points showing region overlap in the buf_pr
  pts_overlap <- ID_values[which(sapply(ID_values, nrow) > 1)]
  
  #loop through each of the buf_pr objects to see with which others they contiguous
  contig <- list()
  for(j in 1:nrow(buf_pr))
  {
    a <-  buf_pr[j,]$ID #get ID of the feature
    
    #select pts overlapping each ID
    b <-  pts_overlap[sapply(pts_overlap, function(x){a %in% x$ID})] 
    
    #bind all
    c <-  rbindlist(b)
    
    #get all IDs connected to the point
    contig[[j]] <- unique(c$ID)
    print(j)
  }
  
  #make a while loop to get each individual range
  
  #duplicate list contig to save the ranges in it
  ranges <- contig
  
  #value to check if another loop is necessary
  v_start <- length(ranges)
  
  #value to check when to stop the look
  v_stop <- 0
  
  #vector to input values in each loop
  vals_loops <- length(ranges)
  
  while(v_start > v_stop)
  {
    #loop through each of the contiguos items to get contiguos ranges
    contig_2 <- list()
    for(j in 1:length(ranges))
    {
      a <- ranges[[j]]
      
      #get all positions with at least one overlap with each item
      b <-  sapply(contig, function(x){TRUE %in% (a %in% x)}) 
      
      #concatenate all unique values in b
      contig_2[[j]] <- sort(unique(unlist(contig[b])))
    }
    
    ranges <- unique(contig_2) #get only unique combinations
    
    #input nrows in each loop
    vals_loops <- c(vals_loops, length(ranges))
    
    #set v_start to the penultime value in vals_loops
    v_start <- vals_loops[length(vals_loops) - 1]
   
    #set v_stop to the last value in vals_loops
    v_stop <- vals_loops[length(vals_loops)] 
  }
  
  #select only ranges with at least 5 points (represented by features in buf_pr)
  ranges_2 <- ranges[sapply(ranges, length) >= 5]
  
  #create convex hulls for each range of the species, and land ranges
  for(j in 1:length(ranges_2))
  {
    #identify the points of each range
    pts_range <- sps_sp_pr[ranges_2[[j]],]
    
    #make the convex hull
    hull <- chull(coordinates(pts_range))  #get points that make the chull
    coords <- pts_range[c(hull, hull[1]), ]  # closed polygon
    poly <- SpatialPolygons(list(Polygons(list(Polygon(coordinates(coords))), ID=j))) 
    
    #cut land parts out
    poly_land <- gDifference(poly, land)
    
    #indicate features IDs
    poly <- spChFIDs(poly, paste(j))
    poly_land <- spChFIDs(poly_land, paste(j))
    
    #include attribute informing which range it is
    poly$range <- j
    poly_land$range <- j
    
    if(j == 1){
      sps_ranges <- poly
      sps_ranges_land <- poly_land
    }else{
      sps_ranges <- spRbind(sps_ranges, poly)
      sps_ranges_land <- spRbind(sps_ranges_land, poly_land)
    }
  }
  
  #save objects with ranges for the species
  writeOGR(sps_ranges, dsn = wd_sps_ranges, 
           layer = gsub('.csv', '', species_list[i]),
           driver = 'ESRI Shapefile')
  
  writeOGR(sps_ranges_land, dsn = wd_sps_ranges_land, 
           layer = gsub('.csv', '', species_list[i]),
           driver = 'ESRI Shapefile')
  
  #calculate distance from each point to the closest land
  distanceLand <-  gDistance(sps_sp, land, byid = T)
  
  #make an IDpts
  sps_sp$IDpts <- c(1:nrow(sps_sp))
  
  #include distance to land info in the results
  sps_sp$distanceLand <- distanceLand[1,]
  
  #for to include range info per point
  sps_sp$Range <- NA
  
  #for to calculate the distance from each point to the edges of its total and land ranges
  sps_sp$distRange <- NA
  
  for(j in 1:nrow(sps_ranges))
  {
    #make a buffer around each range
    range_buff <- gBuffer(sps_ranges[j,], width = 0.5)
    
    #make a hole in the buffer using the total range
    range_diff <- gDifference(range_buff, sps_ranges[j,])
    
    #make a hole in the buffer using the land range
    range_diff <- gDifference(range_buff, sps_ranges_land[j,])
    
    #select points inside of the range
    pts_in_range <- over(sps_sp, sps_ranges[j,])
    pts_in_range2 <- sps_sp[!is.na(pts_in_range$range),]
    
    #get the distance
    dist_range <-  gDistance(pts_in_range2, range_diff, byid = T)
    
    #input dist range into the table
    sps_sp$distRange[as.numeric(names(dist_range[1,]))] <- dist_range[1,]
    
    #input range ID
    sps_sp$Range[as.numeric(names(dist_range[1,]))] <- j
  }
  
  #create column indicating if whether the most contributing var is temperature
  sps_sp$shap_climatic <- ifelse(grepl('Temperature',sps_sp$shap_var),
                                 'Climatic','Non-climatic')
  
  setwd(wd_results_analysis)
  write.csv(sps_sp@data, species_list[i])
}


# Plot results

setwd(wd_results_analysis)
res <- lapply(list.files(), read.csv)

# all results in one table

all_res <- rbindlist(res)

boxplot(distRange ~ shap_climatic, data = res[[i]])
boxplot(distanceLand ~ shap_climatic, data = res[[i]])

#

#box plot results
boxplot(distRange ~ shap_climatic, data = all_res)
boxplot(distanceLand ~ shap_climatic, data = all_res)


plot(world, border = NA,  col = 'grey80')
plot(sps_ranges, add = T)
plot(sps_sp_pr, add = T, pch = 19, cex = 0.8, col = 'orange')


##### CLIM vs NON CLIM

sps_sp_pr$col <- 'orange'
sps_sp_pr$col[grep('Temperature', sps_sp_pr$shap_var)] <- 'blue'
plot(world, border = NA,  col = 'grey80')
plot(sps_sp_pr, add = T, col = sps_sp_pr$col, pch = 19)
plot(sps_ranges, add = T)

points(-175, -55, pch = 19, cex = 2, col = 'blue')
text(-170, -55, 'Climatic', pos = 4, cex = 2)
points(-175, -65, pch = 19, cex = 2, col = 'orange')
text(-170, -65, 'Non climatic', pos = 4, cex = 2)

boxplot(distRange ~ shap_climatic, data = res[[i]])
boxplot(distanceLand ~ shap_climatic, data = res[[i]])


##### ONLY CLIM

sps_sp_pr$col <- NA
sps_sp_pr$col[which(sps_sp_pr$shap_var_temp == "Present.Surface.Temperature.Min")] <- 'blue'
sps_sp_pr$col[which(sps_sp_pr$shap_var_temp == "Present.Surface.Temperature.Mean")] <- 'orange'
sps_sp_pr$col[which(sps_sp_pr$shap_var_temp == "Present.Surface.Temperature.Max")] <- 'red'
    
plot(world, border = NA,  col = 'grey80')
plot(sps_sp_pr, add = T, col = sps_sp_pr$col, pch = 19)
plot(sps_ranges, add = T)
    
points(-175, -45, pch = 19, cex = 2, col = 'blue')
text(-170, -45, 'SST min', pos = 4, cex = 2)
points(-175, -55, pch = 19, cex = 2, col = 'orange')
text(-170, -55, 'SST mean', pos = 4, cex = 2)
points(-175, -65, pch = 19, cex = 2, col = 'red')
text(-170, -65, 'SST max', pos = 4, cex = 2)

res[[i]]$shap_var_temp <- gsub("Present.Surface.Temperature", "SST", x = res[[i]]$shap_var_temp)


boxplot(distRange ~ shap_var_temp, data = res[[i]])
boxplot(distanceLand ~ shap_var_temp, data = res[[i]])



##############################################################################
### plot variables per distance to the range
temp_var <- sps_sp@data[grep('Temperature',sps_sp$shap_var),]


## boxplot on a formula:
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
# *add* notches (somewhat funny here <--> warning "notches .. outside hinges"):

sps_sp_pr$col <- 'orange'
sps_sp_pr$col[grep('Temperature', sps_sp_pr$shap_var)] <- 'blue'
  
plot(world, border = NA,  col = 'grey80')
plot(sps_sp_pr, add = T, col = sps_sp_pr$col, pch = 19)
plot(c_hull[[7]], add = T)
  
plot(sps_sp[1,], add = T, pch = 19, col = 'orange')

sps_sp_pr$col[train$shap_var == "Present.Surface.Temperature.Mean"] <- 'blue'
sps_sp_pr$col[train$shap_var == "Present.Surface.Temperature.Min"] <- 'red'
sps_sp_pr$col[train$shap_var == "Present.Surface.Temperature.Max"] <- 'orange'
      
    
plot(sps_sp_pr)
    
      


plot(buf_pr, add = F)

i = 1

plot(sps_sp_pr[ranges_2[[j]],], add = T, col = 'orange', pch = 19)
plot(c_hull[[j]], add = T)


#create ranges for each species (method from Lerner et al. 2023)

#make convex hull for now (discuss concave approach)

#create pseudo absence

#visualise
plot(species[species$Occurrence == 1,], col='green', pch=16, add =F) 
points(species[species$Occurrence == 0,], col='red', pch=16)



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
      