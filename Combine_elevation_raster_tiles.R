library(raster)

#list WDs
wd_input <- '/Users/carloseduardoaribeiro/Documents/Post-doc/Variable layes/Elevation_Tozer/Tiles'

#list objects representing tiles of elevation
setwd(wd_input)
tar_objs <- list.files(pattern = '.tar.gz')

#untar files 
#must do one by one and rename otherwise they overwrite the previous one
untar(tar_objs[1]) #rename
untar(tar_objs[2]) #rename
untar(tar_objs[3]) #rename
untar(tar_objs[4]) #rename
untar(tar_objs[5]) #rename

#load in rasters
tiles <- lapply(list.files(pattern = '.tif'), raster)

#extend all rasters
tiles_extended <- lapply(tiles, function(x){
  extend(x, extent(-180, 180, -90, 90), value = 0)
})

#stack all rasters
tiles_stack <- stack(tiles_extended)

#make one raster for the whole world
elevation_world <- sum(tiles_stack)
