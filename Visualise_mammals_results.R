#load libraries
library(sf); library(rworldmap); library(raster)

#list wds
wd_ranges <- "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_res_species <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Species'
wd_maps <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Maps'


#make a list of species for which I have results
setwd(wd_res_species)
sps_list <- gsub("_m\\w+", "", list.files())
sps_list <- unique(gsub(".csv", "", sps_list))

#get a world map from package rworldmap
world <- getMap(resolution = 'low')
world <- st_as_sf(world)
original_crs <- crs(world)
world <-  st_transform(world, crs = 3857) #to cope with the edges issue

#### LOOP TROUGH RESULTS ####

#load each result
for(i in 1:length(sps_list))
{
  ### Load and prepare the necessary data for each species ###
  
  #SHAP results for the species
  setwd(wd_res_species)
  sps <- lapply(list.files(pattern = sps_list[i]), read.csv)
  
  names(sps) <- gsub('.csv', '', 
                  gsub('_', '', 
                    gsub(sps_list[i], '', list.files(pattern = sps_list[i]))))
  
  #species range map
  setwd(wd_ranges)
  range <- st_read(dsn = wd_ranges, layer = sps_list[i])
  
  #select the range representing only the native range of the species
  range <- range[range$legend == 'Extant (resident)',]
  
  #unify all features
  range <- st_union(range)
  
  #get min and max coord values of the range
  ext <- st_bbox(range) 
  
  #get the values for the margin around the box
  x_mar <- abs(ext[3] - ext[1]) / 3
  y_mar <- abs(ext[4] - ext[2]) / 3
  
  #make a dataframe with coordinates to create a margin around range extent
  box_df <- data.frame(x = ext[c(1,3,3,1,1)] + x_mar * c(-1,1,1,-1,-1), 
                       y = ext[c(4,4,2,2,4)] + y_mar * c(1,1,-1,-1,1))

  #make the box
  box1 <- st_as_sfc(
            st_bbox(
              st_as_sf(box_df, coords = c('x', 'y'), crs = crs(range))))

  #project the box to the same crs as the world object
  box <- st_transform(box1, crs = 3857)

  #crop world map to show the region where the range
  region <- st_crop(world, box)
  
  #reconvert the cropped region to WGS84
  region <- st_transform(region, crs = crs(range))
  
  #create a colour ramp palhette
  colramp <- colorRampPalette(c("red", "blue"))
  
  #MIN values
  sps_sp_min <- st_as_sf(sps$mins, crs = crs(range),
                     coords = c('decimalLongitude', 'decimalLatitude'))
  
  ### calculate variable contribution for each prediction ###
  
  #sum absolute values of both variables
  total_contr <- abs(sps_sp_min$MIN_temp_SHAP) + abs(sps_sp_min$MIN_prec_SHAP)
  
  #get percentage value of contribution
  sps_sp_min$MIN_temp_Perc <- abs(sps_sp_min$MIN_temp_SHAP) / total_contr * 100
  sps_sp_min$MIN_prec_Perc <- abs(sps_sp_min$MIN_prec_SHAP) / total_contr * 100
  
  #create a column with the colours representing variable contribution
  sps_sp_min$colour <- rgb(sps_sp_min$MIN_temp_Perc, 0, sps_sp_min$MIN_prec_Perc, 
           maxColorValue = 100)
  
  #select only presences
  sps_sp_min_pr <- sps_sp_min[which(sps_sp_min$Occurrence == 1),]
  
  
  #MEAN values
  sps_sp_mean <- st_as_sf(sps$means, crs = crs(range),
                     coords = c('decimalLongitude', 'decimalLatitude'))
  
  ### create a measure of contribution of variable ###
  
  #sum absolute values of both variables
  total_contr <- abs(sps_sp_mean$MEAN_temp_SHAP) + abs(sps_sp_mean$MEAN_prec_SHAP)
  
  #get percentage value of contribution
  sps_sp_mean$MEAN_temp_Perc <- abs(sps_sp_mean$MEAN_temp_SHAP) / total_contr * 100
  sps_sp_mean$MEAN_prec_Perc <- abs(sps_sp_mean$MEAN_prec_SHAP) / total_contr * 100
  
  #create a column with the colours representing variable contribution
  sps_sp_mean$colour <- rgb(sps_sp_mean$MEAN_temp_Perc,
                            0,
                            sps_sp_mean$MEAN_prec_Perc, 
                            maxColorValue = 100)
  
  #select only presences
  sps_sp_mean_pr <- sps_sp_mean[which(sps_sp_mean$Occurrence == 1),]
  
  
  #MAX values
  sps_sp_max <- st_as_sf(sps$maxs, crs = crs(range),
                          coords = c('decimalLongitude', 'decimalLatitude'))
  
  ### create a measure of contribution of variable ###
  
  #sum absolute values of both variables
  total_contr <- abs(sps_sp_max$MAX_temp_SHAP) + abs(sps_sp_max$MAX_prec_SHAP)
  
  #get percentage value of contribution
  sps_sp_max$MAX_temp_Perc <- abs(sps_sp_max$MAX_temp_SHAP) / total_contr * 100
  sps_sp_max$MAX_prec_Perc <- abs(sps_sp_max$MAX_prec_SHAP) / total_contr * 100
  
  #create a column with the colours representing variable contribution
  sps_sp_max$colour <- rgb(sps_sp_max$MAX_temp_Perc,
                            0,
                            sps_sp_max$MAX_prec_Perc, 
                            maxColorValue = 100)
  
  #select only presences
  sps_sp_max_pr <- sps_sp_max[which(sps_sp_max$Occurrence == 1),]
  
  #install function "myGradientLegend" in script modified_function_gradientLegend
  
  ### save image
  setwd(wd_maps)
  
  pdf(file=paste0(sps_list[i],"_var_contribution.pdf"))
  
  par(mfrow = c(1,3))
  
  #plot layers
  plot(st_geometry(region), col = 'khaki', main = 'Min')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_min_pr), add = T, col = sps_sp_min$colour, pch = 19)
  
  #make legend
  myGradientLegend(valRange = c(0, 100),
                   pos=c(0.3,0,0.7,.015),
                   color = colramp(100),
                   side = 1,
                   n.seg = 0,
                   values = c("Temp","Prec"),
                   cex = 1)
  
  
  #plot layers
  plot(st_geometry(region), col = 'khaki', main = 'Mean')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_mean_pr), add = T, col = sps_sp_mean$colour, pch = 19)
  
  #make legend
  myGradientLegend(valRange = c(0, 100),
                   pos=c(0.3,0,0.7,.015),
                   color = colramp(100),
                   side = 1,
                   n.seg = 0,
                   values = c("Temp","Prec"),
                   cex = 1)
  
  
  #plot layers
  plot(st_geometry(region), col = 'khaki', main = 'Max')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_max_pr), add = T, col = sps_sp_max$colour, pch = 19)
  
  #make legend
  myGradientLegend(valRange = c(0, 100),
                   pos=c(0.3,0,0.7,.015),
                   color = colramp(100),
                   side = 1,
                   n.seg = 0,
                   values = c("Temp","Prec"),
                   cex = 1)
  
  dev.off()
  
}
  
  
  ###################

  
  #create a column with the colours representing each variable that contributed the most
  sps_sp$colour <-'xx'
  sps_sp$colour[which(sps_sp$shap_var == 'bio2')] <- 'red'
    sps_sp$colour[which(sps_sp$shap_var == 'bio4')] <- 'yellow'
      sps_sp$colour[which(sps_sp$shap_var == 'bio7')] <- 'orange'
        sps_sp$colour[which(sps_sp$shap_var == 'bio15')] <- 'blue'
          
        #select only presences
        sps_sp_pr <- sps_sp[which(sps_sp$Occurrence == 1),]
        
        #get the values to find the position to plot the legends
        leg_pos <- st_bbox(region)
        inter_x <- abs(leg_pos[3] - leg_pos[1]) / 20
        inter_y <- abs(leg_pos[4] - leg_pos[2]) / 20
        
  
  ### create a mesure of centralness whithin the range ###
  
  ########

  
 
  #save map
  setwd(wd_maps)
  
  #jpeg(file=paste0(sps_list[i],"_var_contribution.jpeg"))
  
  #plot part of the world as background
  plot(world, border = NA, col = 'gray90')
  
  #plot part of the world as background
  plot(region, border = NA, col = 'gray90')
  
  #plot points with shap result 
  plot(sps_sp_pr, add = T, col = sps_sp_pr$colour, pch =19)
  
  #make legend
  points(leg_pos[1] + inter_x, 
         leg_pos[2] + inter_y, pch = 19, cex = 2, col = 'blue')
  text(leg_pos[1] + (inter_x * 1.5), 
         leg_pos[2] + inter_y, 'Precipitation Seasonality', 
         cex = 1.3, pos = 4)
  
  points(leg_pos[1] + inter_x, 
         leg_pos[2] + (2 * inter_y), pch = 19, cex = 2, col = 'orange')
  text(leg_pos[1] + (inter_x * 1.5), 
       leg_pos[2] + (2 * inter_y), 'Temperature Annual Range', 
       cex = 1.3, pos = 4)
  
  points(leg_pos[1] + inter_x, 
         leg_pos[2] + (3 * inter_y), pch = 19, cex = 2, col = 'red')
  text(leg_pos[1] + (inter_x * 1.5), 
       leg_pos[2] + (3 * inter_y), 'Mean Diurnal Range', 
       cex = 1.3, pos = 4)
  
  points(leg_pos[1] + inter_x, 
         leg_pos[2] + (4 * inter_y), pch = 19, cex = 2, col = 'yellow')
  text(leg_pos[1] + (inter_x * 1.5), 
       leg_pos[2] + (4 * inter_y), 'Temperature Seasonality', 
       cex = 1.3, pos = 4)
  
  #plot range on top of the world map
  plot(sps_range2, add = T, border = 'darkgreen', lwd = 2)
  
  #plot species name
  text((leg_pos[1] + leg_pos[3]) / 2, 
        leg_pos[4] - (2 * inter_y), 
        paste0(sps_list[i]),
        font = 3,
        cex = 1.3)
  
  #dev.off()
  
  #check total range size for each species
  
  #check for the size of parts of the range ?
  
}

# bio 2 : Mean Diurnal Range
# bio 4 : Temperature Seasonality
# bio 7 : Temperature Annual Range
# bio 15 : Precipitation Seasonality

