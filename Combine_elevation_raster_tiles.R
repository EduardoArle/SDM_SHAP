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
tiles_extended <- list()
for(i in 1:length(tiles))
{
  tiles_extended[[i]] <- extend(tiles[[i]],
                                extent(-180, 180, -90, 90),
                                value = 0)
  
  writeRaster(tiles_extended[[i]],
              filename = paste0('extended_SRTM15Plus_tile',i,'.tif'),
              format = "GTiff")
  
  print(i)
}

#stack all rasters
tiles_stack <- stack(tiles_extended)

#make one raster for the whole world
elevation_world <- sum(tiles_stack)
