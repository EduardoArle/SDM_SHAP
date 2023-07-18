#load libraries
library(sf); library(rworldmap); library(rgeos)

#list wds
wd_tree_poly <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Plants/tree species polygon'
wd_res_species <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Plants/Results'
wd_maps <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Plants/Maps'

#load polygons object
setwd(wd_tree_poly)
load('GBIFpolygon_groups_kden.RData')

#make a list of species for which I have results
setwd(wd_res_species)
sps_list <- gsub('.csv', '', list.files())

#get a world map from package rworldmap
world <- getMap(resolution = 'low')
world <- gUnaryUnion(world)

#### LOOP TROUGH RESULTS ####

#load each result
for(i in 1:length(sps_list))
{
  #load the SHAP results for the species
  setwd(wd_res_species)
  sps <- read.csv(paste0(sps_list[i], '.csv'))
  
  #select ranges for the species
  sps_range <- which(kden.p_groups_trim$species == sps_list[i])
  sps_range2 <- kden.p_groups_trim[[3]][sps_range]
  
  #unify all features of the range
  sps_range2 <- st_union(sps_range2)
  
  #get min and max coord values of the range
  ext <- st_bbox(sps_range2) 
  
  #get the values for the margin around the box
  x_mar <- abs(ext[3] - ext[1]) / 3
  y_mar <- abs(ext[4] - ext[2]) / 3
  
  #make a box with margin around the range extent
  box <-  st_polygon(list(cbind(ext[c(1,3,3,1,1)],  
                                ext[c(4,4,2,2,4)])))
  
  #make a box with margin around the range extent
  box <-  st_polygon(list(cbind(ext[c(1,3,3,1,1)] + x_mar * c(-1,1,1,-1,-1),  
                                ext[c(4,4,2,2,4)] + y_mar * c(1,1,-1,-1,1))))
  
  #crop world map to show the region where the range
  region <- st_crop(st_as_sf(world), box)
  
  #make spatial object
  sps_sp <- st_as_sf(sps, coords = c('decimalLongitude', 'decimalLatitude'))
  
  #create a column with the colours representing each variable that contributed the most
  sps_sp$colour <-'xx'
  sps_sp$colour[which(sps_sp$shap_var == 'bio2')] <- 'red'
  sps_sp$colour[which(sps_sp$shap_var == 'bio4')] <- 'yellow'
  sps_sp$colour[which(sps_sp$shap_var == 'bio7')] <- 'orange'
  sps_sp$colour[which(sps_sp$shap_var == 'bio15')] <- 'blue'
    
  #select only presences
  sps_sp_pr <- sps_sp[which(sps_sp$Occurrence == 1),]
  
  #get the values to find the position to plot the legends
  leg_pos <- st_bbox(world)
  inter_x <- abs(leg_pos[3] - leg_pos[1]) / 20
  inter_y <- abs(leg_pos[4] - leg_pos[2]) / 20
  
  #get the values to find the position to plot the legends
  leg_pos <- st_bbox(region)
  inter_x <- abs(leg_pos[3] - leg_pos[1]) / 20
  inter_y <- abs(leg_pos[4] - leg_pos[2]) / 20

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

