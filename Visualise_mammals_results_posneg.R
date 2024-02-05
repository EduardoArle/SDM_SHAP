#load libraries
library(sf); library(rworldmap); library(raster)

#list wds
wd_ranges <- "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Range_maps"
wd_res_species <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_maps <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Maps_all_variables'

#select species
species <- 'Akodon lutescens'

#manually select the files for the species I need
setwd(wd_res_species)
sps_res <- lapply(list.files(pattern = species), read.csv)
names(sps_res) <- gsub(paste0(species, '_'), '',
                       gsub("_.csv", "", list.files(pattern = species)))

#get a world map from package rworldmap
world <- getMap(resolution = 'low')
world <- st_as_sf(world)
original_crs <- crs(world)
world <-  st_transform(world, crs = 3857) #to cope with the edges issue

#species range map
setwd(wd_ranges)
range <- st_read(dsn = wd_ranges, layer = species)

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

#create a spatial object with the occurrences
sps_res_sp <- lapply(sps_res, function(x) {
  st_as_sf(x, crs = crs(range),coords = c('decimalLongitude', 'decimalLatitude'))})
  
#select only presences in the results I want to show
sps_sp_pr <- lapply(sps_res_sp, function(x) {
  x[which(x$Occurrence == 1),]})


#### PLOT
par(mfrow = c(2,3), mar = c(1,1,2,1))

#identify the limits of the contributions
lim_cont <- range(c(sps_sp_pr$minT$Min_T_SHAP,
                    sps_sp_pr$meanT$Mean_T_SHAP,
                    sps_sp_pr$maxT$Max_T_SHAP))

#make a colourRamp
colramp <- colorRampPalette(c("#FF0000", "#FFFFFF", "#0000FF"))

#populate the tables with the colours to be plotted 
sps_sp_pr$minT$colours <- colramp(100)[cut(c(-0.5,0.5,sps_sp_pr$minT$Min_T_SHAP), 
                               breaks = 100)][-c(1,2)]

sps_sp_pr$meanT$colours <- colramp(100)[cut(c(-0.5,0.5,sps_sp_pr$meanT$Mean_T_SHAP), 
                                           breaks = 100)][-c(1,2)]

sps_sp_pr$maxT$colours <- colramp(100)[cut(c(-0.5,0.5,sps_sp_pr$maxT$Max_T_SHAP), 
                                            breaks = 100)][-c(1,2)]

#plot MIN T
plot(st_geometry(region), col = 'khaki', main = 'Min T', cex.main = 2)
plot(st_geometry(range), add = T, col = '#238b4590')
plot(st_geometry(sps_sp_pr$minT), add = T, pch = 21, 
     col = 'black', bg = sps_sp_pr$minT$colours, cex = 1.5)

#plot MEAN T
plot(st_geometry(region), col = 'khaki', main = 'Mean T', cex.main = 2)
plot(st_geometry(range), add = T, col = '#238b4590')
plot(st_geometry(sps_sp_pr$meanT), add = T, pch = 21, 
     col = 'black', bg = sps_sp_pr$meanT$colours, cex = 1.5)

#plot MAX T
plot(st_geometry(region), col = 'khaki', main = 'Max T', cex.main = 2)
plot(st_geometry(range), add = T, col = '#238b4590')
plot(st_geometry(sps_sp_pr$maxT), add = T, pch = 21, 
     col = 'black', bg = sps_sp_pr$maxT$colours, cex = 1.5)

par(mfrow = c(1,1), mar = c(2,2,2,2))

#plot legend
plot(box, border = NA)

#plot map legend
myGradientLegend(valRange = c(-0.5, 0.5), 
                 pos=c(0.28,0,0.73,.017),
                 color = colramp(100), 
                 side = 1,
                 n.seg = 1,
                 cex = 2)

##########################################################


#make a list of species for which I have results
setwd(wd_res_species)
sps_list <- gsub("_.csv", "", list.files())

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
  sps <- read.csv(paste0(sps_list[i], '_.csv'))
  
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
  
  #create a spatial object with the occurrences
  sps_sp <- st_as_sf(sps, crs = crs(range),
                     coords = c('decimalLongitude', 'decimalLatitude'))
  
  ### create a measure of contribution of variables for T and PPT ###
  
  #sum absolute SHAP values of MIN, MEAN, and MAX
  total_contr_T <- abs(sps_sp$MIN_temp_SHAP) + 
                   abs(sps_sp$MEAN_temp_SHAP) +
                   abs(sps_sp$MAX_temp_SHAP)
  
  total_contr_PPT <- abs(sps_sp$MIN_prec_SHAP) + 
                     abs(sps_sp$MEAN_prec_SHAP) +
                     abs(sps_sp$MAX_prec_SHAP)
  
  #get percentage value of contribution
  sps_sp$MIN_T_Perc <- abs(sps_sp$MIN_temp_SHAP) / total_contr_T * 100
  sps_sp$MEAN_T_Perc <- abs(sps_sp$MEAN_temp_SHAP) / total_contr_T * 100
  sps_sp$MAX_T_Perc <- abs(sps_sp$MAX_temp_SHAP) / total_contr_T * 100
  
  sps_sp$MIN_PPT_Perc <- abs(sps_sp$MIN_prec_SHAP) / total_contr_PPT * 100
  sps_sp$MEAN_PPT_Perc <- abs(sps_sp$MEAN_prec_SHAP) / total_contr_PPT * 100
  sps_sp$MAX_PPT_Perc <- abs(sps_sp$MAX_prec_SHAP) / total_contr_PPT * 100
  
  #select only presences
  sps_sp_pr <- sps_sp[which(sps_sp$Occurrence == 1),]
  
  #save images of the maps
  setwd(wd_maps)
  
  pdf(file=paste0(sps_list[i],"_var_contribution.pdf"))
  
  par(mfrow = c(2,3), mar = c(0,0,1,0))
  
  #plot MIN T
  plot(st_geometry(region), col = 'khaki', main = 'Min T')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_pr), add = T, pch = 21, 
       col = 'black', bg = 'red', cex = (sps_sp_pr$MIN_T_Perc / 50) + 0.1)
  
  #plot MEAN T
  plot(st_geometry(region), col = 'khaki', main = 'Mean T')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_pr), add = T, pch = 21, 
       col = 'black', bg = 'red', cex = (sps_sp_pr$MEAN_T_Perc / 50) + 0.1)
  
  #plot MAX T
  plot(st_geometry(region), col = 'khaki', main = 'Max T')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_pr), add = T, pch = 21, 
       col = 'black', bg = 'red', cex = (sps_sp_pr$MAX_T_Perc / 50) + 0.1)
  
  
  #plot MIN PPT
  plot(st_geometry(region), col = 'khaki', main = 'Min PPT')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_pr), add = T, pch = 21, 
       col = 'black', bg = 'blue', cex = (sps_sp_pr$MIN_PPT_Perc / 50) + 0.1)
  
  #plot MEAN PPT
  plot(st_geometry(region), col = 'khaki', main = 'Mean PPT')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_pr), add = T, pch = 21, 
       col = 'black', bg = 'blue', cex = (sps_sp_pr$MEAN_PPT_Perc / 50) + 0.1)
  
  #plot MAX PPT
  plot(st_geometry(region), col = 'khaki', main = 'Max PPT')
  plot(st_geometry(range), add = T, col = '#238b4590')
  plot(st_geometry(sps_sp_pr), add = T, pch = 21, 
       col = 'black', bg = 'blue', cex = (sps_sp_pr$MAX_PPT_Perc / 50) + 0.1)

  
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

