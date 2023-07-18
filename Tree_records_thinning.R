#load packages 
library(raster);library(rgdal);library(data.table)

#list wds
wd_tree_points <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Plants'
wd_variables <- '/Users/carloseduardoaribeiro/Documents/CNA/Data/Variables/wc2-5'
wd_thinned_occ <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Thinned_occurrences'

#load point records
setwd(wd_tree_points)
load('GBIFtree_all.RData')

#select cols that I need
occurrence <- as.data.frame(GBIFtree_all[,c(10,19,22,23)])

#list species 
sps_list <- sort(unique(occurrence$species))
sps_list <- sps_list[-1]

#load one variable to thin the records by the same resolution
setwd(wd_variables)
mean_d_range <- raster('bio2.bil')

#make ID raster to thin points by variable resolution
ID_raster <- mean_d_range
ID_raster[] <- c(1:length(ID_raster))

#thin points and save 
for(i in 1:length(sps_list))
{
  sps <- sps_list[i]
  
  #select points for the species
  occ_sps <- occurrence[which(occurrence$species == sps),]
  
  #create spatial object
  a <- occ_sps
  coordinates(a) <- ~ decimalLongitude + decimalLatitude
  
  #get cellID value
  b <- extract(ID_raster, a)
  
  #include cell value and coordinates into species data
  a@data <- cbind(a@data, coordinates(a))
  a$cellID <- b
  
  #keep only one entry per cell
  c <- unique(as.data.table(a@data), by = 'cellID')
  
  #save results
  setwd(wd_thinned_occ)
  write.csv(c, paste0(sps,'.csv'), row.names = F)
  
  print(i)
}


