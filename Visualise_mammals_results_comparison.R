#load libraries
library(sf); library(rworldmap); library(raster)

#list wds
wd_ranges <- "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_res_species <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_maps <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Maps_comparison'


#make a list of species for which I have results
setwd(wd_res_species)
sps_list <- gsub("_m\\w+", "", list.files()) #regex to delete '_meanPPT' etc
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
  
  #all SHAP results for the species
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
  
  ### calculate variable contribution for each prediction ###
  
  ## mean PPT to compare T variables importances
  
  
  ##############################################
  ############## mean PPT + min T ##############
  ##############################################
  
  
  #spatial object with the occurrences minT 
  sps_sp_minT <- st_as_sf(sps$minT, crs = crs(range),
                         coords = c('decimalLongitude', 'decimalLatitude'))
  
  #sum absolute values of both variables
  con_total_minT <- abs(sps_sp_minT$Min_T_SHAP) + abs(sps_sp_minT$Mean_PPT_SHAP)
  
  #get proportion of value of contribution (with signal)
  sps_sp_minT$MinT_perc <- sps_sp_minT$Min_T_SHAP / con_total_minT * 100
  sps_sp_minT$MeanPPT_perc <- abs(sps_sp_minT$Mean_PPT_SHAP) / con_total_minT* 100
  
  #select only presences
  sps_sp_minT_pr <- sps_sp_minT[which(sps_sp_minT$Occurrence == 1),]
  
  
  ###############################################
  ############## mean PPT + mean T ##############
  ###############################################
  
  
  #spatial object with the occurrences minT 
  sps_sp_meanT <- st_as_sf(sps$meanT, crs = crs(range),
                          coords = c('decimalLongitude', 'decimalLatitude'))

  #sum absolute values of both variables
  con_total_meanT <- abs(sps_sp_meanT$Mean_T_SHAP) + 
                     abs(sps_sp_meanT$Mean_PPT_SHAP)
  
  #get percentage value of contribution
  sps_sp_meanT$MeanT_perc <- abs(sps_sp_meanT$Mean_T_SHAP) / 
                                 con_total_meanT * 100
  sps_sp_meanT$MeanPPT_perc <- abs(sps_sp_meanT$Mean_PPT_SHAP) / 
                                   con_total_meanT* 100
  
  #select only presences
  sps_sp_meanT_pr <- sps_sp_meanT[which(sps_sp_meanT$Occurrence == 1),]
  
  
  ##############################################
  ############## mean PPT + max T ##############
  ##############################################
  
  
  #spatial object with the occurrences minT 
  sps_sp_maxT <- st_as_sf(sps$maxT, crs = crs(range),
                          coords = c('decimalLongitude', 'decimalLatitude'))
  
  #sum absolute values of both variables
  con_total_maxT <- abs(sps_sp_maxT$Max_T_SHAP) + abs(sps_sp_maxT$Mean_PPT_SHAP)
  
  #get percentage value of contribution
  sps_sp_maxT$MaxT_perc <- abs(sps_sp_maxT$Max_T_SHAP) / con_total_maxT * 100
  sps_sp_maxT$MeanPPT_perc <- abs(sps_sp_maxT$Mean_PPT_SHAP) / con_total_maxT* 100
  
  #select only presences
  sps_sp_maxT_pr <- sps_sp_maxT[which(sps_sp_maxT$Occurrence == 1),]
  
  
  ## mean T to compare PPT variables importance
  
  ##############################################
  ############## mean T + min PPT ##############
  ##############################################
  
  
  #spatial object with the occurrences minT 
  sps_sp_minPPT <- st_as_sf(sps$minPPT, crs = crs(range),
                          coords = c('decimalLongitude', 'decimalLatitude'))
  
  #sum absolute values of both variables
  con_total_minPPT <- abs(sps_sp_minPPT$Min_PPT_SHAP) + 
                      abs(sps_sp_minPPT$Mean_T_SHAP)
  
  #get percentage value of contribution
  sps_sp_minPPT$MinPPT_perc <- abs(sps_sp_minPPT$Min_PPT_SHAP) / 
                                                      con_total_minPPT * 100
  sps_sp_minPPT$MeanT_perc <- abs(sps_sp_minPPT$Mean_T_SHAP) / 
                                                      con_total_minPPT* 100
  
  #select only presences
  sps_sp_minPPT_pr <- sps_sp_minPPT[which(sps_sp_minPPT$Occurrence == 1),]
  
  
  ###############################################
  ############## mean T + mean PPT ##############
  ###############################################
  
  
  #spatial object with the occurrences minT 
  sps_sp_meanPPT <- st_as_sf(sps$meanPPT, crs = crs(range),
                           coords = c('decimalLongitude', 'decimalLatitude'))
  
  #sum absolute values of both variables
  con_total_meanPPT <- abs(sps_sp_meanPPT$Mean_PPT_SHAP) + 
                       abs(sps_sp_meanPPT$Mean_T_SHAP)
  
  #get percentage value of contribution
  sps_sp_meanPPT$MeanPPT_perc <- abs(sps_sp_meanPPT$Mean_PPT_SHAP) / 
                                                  con_total_meanPPT * 100
  sps_sp_meanPPT$MeanT_perc <- abs(sps_sp_meanPPT$Mean_T_SHAP) / 
                                                  con_total_meanPPT* 100
  
  #select only presences
  sps_sp_meanPPT_pr <- sps_sp_meanPPT[which(sps_sp_meanPPT$Occurrence == 1),]
  
  
  ##############################################
  ############## mean T + max PPT ##############
  ##############################################
  
  
  #spatial object with the occurrences minT 
  sps_sp_maxPPT <- st_as_sf(sps$maxPPT, crs = crs(range),
                          coords = c('decimalLongitude', 'decimalLatitude'))
  
  #sum absolute values of both variables
  con_total_maxPPT <- abs(sps_sp_maxPPT$Max_PPT_SHAP) + 
                      abs(sps_sp_maxPPT$Mean_T_SHAP)
  
  #get percentage value of contribution
  sps_sp_maxPPT$MaxPPT_perc <- abs(sps_sp_maxPPT$Max_PPT_SHAP) / 
                                                        con_total_maxPPT * 100
  sps_sp_maxPPT$MeanT_perc <- abs(sps_sp_maxPPT$Mean_T_SHAP) / 
                                                        con_total_maxPPT* 100
  
  #select only presences
  sps_sp_maxPPT_pr <- sps_sp_maxPPT[which(sps_sp_maxPPT$Occurrence == 1),]

  
  #save images of the maps
  setwd(wd_maps)
  
  pdf(file=paste0(sps_list[i],"_var_contribution.pdf"))
  
  par(mfrow = c(1,1), mar = c(0,0,1,0))
  
  #plot MIN T
  # plot(st_geometry(region), col = 'khaki', main = 'Min T')
  plot(st_geometry(range), add = F, col = '#0000FF20')
  plot(st_geometry(sps_sp_minT_pr), add = T, pch = 21, 
       col = 'black', bg = 'red', cex = (sps_sp_minT_pr$MinT_perc / 50) + 0.1)
  
  #plot MEAN T
  plot(st_geometry(region), col = 'khaki', main = 'Mean T')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_meanT_pr), add = T, pch = 21, 
       col = 'black', bg = 'red', cex = (sps_sp_meanT_pr$MeanT_perc / 50) + 0.1)
  
  #plot MAX T
  plot(st_geometry(region), col = 'khaki', main = 'Max T')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_maxT_pr), add = T, pch = 21, 
       col = 'black', bg = 'red', cex = (sps_sp_maxT_pr$MaxT_perc / 50) + 0.1)
  
  
  #plot MIN PPT
  plot(st_geometry(region), col = 'khaki', main = 'Min PPT')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_minPPT_pr), add = T, pch = 21, 
       col = 'black', bg = 'blue', cex = (sps_sp_minPPT_pr$MinPPT_perc / 50) + 0.1)
  
  #plot MEAN PPT
  plot(st_geometry(region), col = 'khaki', main = 'Mean PPT')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_meanPPT_pr), add = T, pch = 21, 
       col = 'black', bg = 'blue', 
       cex = (sps_sp_meanPPT_pr$MeanPPT_perc / 50) + 0.1)
  
  #plot MAX PPT
  plot(st_geometry(region), col = 'khaki', main = 'Max PPT')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_maxPPT_pr), add = T, pch = 21, 
       col = 'black', bg = 'blue', cex = (sps_sp_maxPPT_pr$MaxPPT_perc / 50) + 0.1)

  
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

