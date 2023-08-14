#load packages
library(sf); library(rworldmap)

#list wds
wd_ranges <- "/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals"

#list species
setwd(wd_ranges)
sps_list <- gsub('.shp', '', list.files(pattern = '.shp'))

#load world map
world <- getMap()

#visualise species distributions and choose the ones that are continuous and do not border the oceans (manually)
i = 100

range <- st_read(dsn = wd_ranges, layer = sps_list[i])

plot(st_geometry(range), add = F, col = 'red')
plot(world, border = NA, col = 'darkgreen')
plot(st_geometry(range), add = T, col = 'orange', border = NA)

#list species that may be interesting to look at
interesting_sps <- c(3, 8, 13, 14, 33, 38, 41, 43, 44, 45, 59, 68, 69, 70, 71,
                     72, 83, 85, 87, 89, 91, 92, 93, 98, 100)
