#load packages
library(rnaturalearth)

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Manuscript/Methods/Things for figure'

#set seed
set.seed(69)

##### PLOT GRAPH ######

#create data for plot
varContr <- c(rnorm(10, mean = 0, sd = 0.15),
              rnorm(10, mean = 0.1, sd = 0.22),
              rnorm(10, mean = 0.3, sd = 0.29),
              rnorm(10, mean = 0.4, sd = 0.26),
              rnorm(10, mean = 0.5, sd = 0.15))

relPolewardness <- sort(runif(n = 50, min = 0, max = 1))

#set y and x lims
ylim <- c(-0.2, 0.7)
xlim <- c(0, 1)

#set parametres for plotting
par(mar = c(5,5,5,5), pty="s")

#minT
plot(relPolewardness, varContr, 
     pch = 19, cex = 0.8, col = '#0000FF25',
     ylab = 'Temperature contribution',
     xlab = 'Relative polarwardness',
     ylim = c(ylim[1], ylim[2]))

#fit linear model
lin_mod_minT <- lm(varContr ~ relPolewardness)
abline(lin_mod_minT, col = '#0000FF', lwd = 3)


##### PLOT MAP #####

#load world map
world <- ne_countries(returnclass = "sf")

#create polygon representing species range
poly = st_polygon(
  list(cbind(c(10,08,09,12,15,16,14,12,14,20,25,32,
               34,35,31,29,29,26,23,20,15,12,10),
             c(10,11,13,13,15,15,17,18,23,26,29,30,
               26,23,14,12,10,09,08,05,07,08,10))))

poly_sf <-  st_sfc(poly, crs = crs(world))

#create points representing species occurrences
lat <- (relPolewardness * (st_bbox(poly_sf)$ymax - st_bbox(poly_sf)$ymin)) +
              st_bbox(poly_sf)$ymin

lon <- runif(n = 50, 
             min = st_bbox(poly_sf)$xmin,
             max = st_bbox(poly_sf)$xmax)

#check one by one and put the ones out inside of the range
#I know, not a smart not efficient solution. but it gives me pleasure.
lon[1] <- lon[1] + 2
lon[2] <- lon[2] - 3
lon[3] <- lon[3] + 1
lon[4] <- lon[4] - 3
lon[5] <- lon[5] - 3
lon[6] <- lon[6] + 3
lon[7] <- lon[7] - 8
lon[10] <- lon[10] - 7
lon[12] <- lon[12] - 5
lon[17] <- lon[17] + 3
lon[24] <- lon[24] + 2
lon[32] <- lon[32] + 10
lon[38] <- lon[38] + 3
lon[39] <- lon[39] + 11
lon[41] <- lon[41] + 5
lon[42] <- lon[42] + 14
lon[45] <- lon[45] + 17
lon[47] <- lon[47] - 5

coords <- as.data.frame(cbind(lon, lat))

points_sf <- st_as_sf(coords, coords = c('lon', 'lat'), crs = crs(world))

#plot map
plot(poly_sf, lwd = 3, border = '#707070', col = '#F0F0F0')
plot(points_sf, add = T, pch = 19, cex = 0.8, col = '#0000FF25')

plot(points_sf[1,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[2,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[3,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[4,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[5,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[6,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[7,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[8,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[9,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[10,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[11,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[12,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[13,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[14,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[15,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[16,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[17,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[18,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[19,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[20,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[21,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[22,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[23,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[24,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[25,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[26,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[27,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[28,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[29,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[30,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[31,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[32,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[33,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[34,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[35,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[36,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[37,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[38,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[39,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[40,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[41,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[42,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[43,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[44,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[45,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[46,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[47,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[48,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[49,], add = T, pch = 19, cex = 0.8, col = '#FF0000')
plot(points_sf[50,], add = T, pch = 19, cex = 0.8, col = '#FF0000')


#save new coordinates
setwd(wd_tables)
write.csv(coords, 'Coordinates_points_large_map.csv', row.names = F)
