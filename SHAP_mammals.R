#load packages 
library(raster);library(rgdal);library(data.table);library(rworldmap)
library(dplyr);library(sf);library(rgeos);library(shapviz);library(kernelshap)
library(xgboost);library(caret)

#list wds
wd_tree_poly <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Plants/tree species polygon'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5'
wd_res_species <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Plants/Results'
wd_thinned_occ <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Thinned_occurrences'

#list species 
setwd(wd_thinned_occ)
sps_list <- gsub('.csv', '', list.files())

#load polygons object
setwd(wd_tree_poly)
load('GBIFpolygon_groups_kden.RData')

#load selected variables (chosen according to Lerner et al.)
setwd(wd_variables)
mean_d_range <- raster('bio2.bil')
temp_seas <- raster('bio4.bil')
temp_an_range <- raster('bio7.bil')
prec_seas <- raster('bio15.bil')

# stack all variables
preds <- stack(mean_d_range, temp_seas, temp_an_range, prec_seas)

######## Run SHAP for all species ######

for(i in 280:length(sps_list))
{
  #select species
  sps <- sps_list[i]
  
  #loas species occurrences
  setwd(wd_thinned_occ)
  sps_occ <- read.csv(paste0(sps,'.csv'))
  
  #select only presence occurrences
  pr_sps <- sps_occ[which(sps_occ$occurrenceStatus == 'PRESENT'),]
  
  #create pseudo absences (half the number of presences)
  
  #select ranges for the species
  sps_range <- which(kden.p_groups_trim$species == sps)
  sps_range2 <- kden.p_groups_trim[[3]][sps_range]
  
  #unify all features
  sps_range2 <- st_union(sps_range2)
  
  #make spatial object
  pr_sps_sp <- st_as_sf(pr_sps, coords = c('decimalLongitude', 'decimalLatitude'),
                        crs = crs(sps_range2))
  
  #make a small buffer around the points to delimit the area where I don't want pseudo abs
  small_buffer <- st_buffer(pr_sps_sp, 1) # 1 degree
  
  #make one object
  no_pa_area <- st_union(small_buffer)
  
  #make a holes in the species range by the small buffer around points
  pa_area <- st_difference(sps_range2, no_pa_area)
  
  #define number of pseudo abs to be created (abs + pseudo abs = presence / 2)
  n_pa <-  round(nrow(pr_sps) / 2)
  
  #generate pseudo absences
  pa <- st_sample(pa_area, size = n_pa, type = "random")
  
  #get coords of pa
  pa_coords <- as.data.frame(st_coordinates(pa))
  names(pa_coords) <- c('decimalLongitude', 'decimalLatitude')
  pa_coords$Occurrence <- 0 #include column informing occ status
  pa_coords$Type <- 'Pseudo-absence'
  
  #get coords of pr
  pr_coords <- as.data.frame(st_coordinates(pr_sps_sp))
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
  pred_vars <- c('bio2','bio4','bio7','bio15')

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
  
  for(j in 1:nrow(tab_occ_vars))
  {
    obj <- sv_waterfall(shp, row_id = j) +
      theme(axis.text = element_text(size = 11))
    
    a <- which.max(abs(obj$data$S)) #get the highest absolute value
    
    shap_var[j] <- row.names(obj$data)[a]
    
    print(j)
  }
  
  tab_occ_vars$shap_var <- shap_var

  #save results per species
  setwd(wd_res_species)
  write.csv(tab_occ_vars, paste0(sps,'.csv'), row.names = F)
}



#################


plot(sps_range2)


#make a spatial object
occ_sps_sp <- occ_sps
coordinates(occ_sps_sp) <- ~ decimalLongitude + decimalLatitude

plot(world, border = NA, col = 'gray80')
plot(occ_sps_sp, pch = 19, col = 'red', cex = 0.4, add = T)




plot(t, add = T)

class(t)
length(t)

#ger world map
world <- getMap()

class(kden.p_groups_trim)
head(kden.p_groups_trim)

class(kden.p_groups_trim[[1]])
length(kden.p_groups_trim[[1]])
head(kden.p_groups_trim[[1]])

class(kden.p_groups_trim[[2]])
length(kden.p_groups_trim[[2]])
head(kden.p_groups_trim[[2]])

class(kden.p_groups_trim[[3]])
length(kden.p_groups_trim[[3]])

class(kden.p_groups_trim[[3]][[i]])
length(kden.p_groups_trim[[3]][[i]])
head(kden.p_groups_trim[[3]][[i]])

class(kden.p_groups_trim[[3]][[i]][[1]])
length(kden.p_groups_trim[[3]][[i]][[1]])

class(kden.p_groups_trim[[3]][[i]][[2]])
length(kden.p_groups_trim[[3]][[i]][[2]])

class(kden.p_groups_trim[[3]][[i]][[1]][[1]])
nrow(kden.p_groups_trim[[3]][[i]][[1]][[1]])

class(kden.p_groups_trim[[3]][[i]][[2]][[1]])
nrow(kden.p_groups_trim[[3]][[i]][[2]][[1]])

t <- as.data.frame(kden.p_groups_trim[[3]][[i]][[1]][[1]])
t2 <- as.data.frame(kden.p_groups_trim[[3]][[i]][[2]][[1]])
t3 <- as.data.frame(kden.p_groups_trim[[3]][[i]][[3]][[1]])
t3 <- as.data.frame(kden.p_groups_trim[[3]][[i]][[93]][[1]])


plot(world)

head(t)
coordinates(t) <- ~ V1 + V2
t

plot(t, add = T)

head(t2)
coordinates(t2) <- ~ V1 + V2
t2

plot(world)
plot(t2, add = F)

head(t3)
coordinates(t3) <- ~ V1 + V2
t3

plot(world)
plot(t3, add = F)

setwd('/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP')
load('sf_world2.RData')


