#load packages
library(rnaturalearth); library(sf); library(units)

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Manuscript/Figures/Fig 1/Things for the figure'

#set seed
set.seed(69)

##### GRAPH ######

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


##### MAP #####

#load world map
world <- ne_countries(returnclass = "sf")

#create polygon representing species range
poly = st_polygon(
  list(cbind(c(10,08,09,12,15,16,14,12,14,20,25,32,
               34,35,31,29,29,26,23,20,15,12,10),
             c(10,11,13,13,15,15,17,18,23,26,29,30,
               26,23,14,12,10,09,08,05,07,08,10))))

poly_sf <-  st_sfc(poly, crs = st_crs(world))

#create points representing species occurrences
lat <- (relPolewardness * (st_bbox(poly_sf)$ymax - st_bbox(poly_sf)$ymin)) +
              st_bbox(poly_sf)$ymin

lon <- runif(n = 50, 
             min = st_bbox(poly_sf)$xmin,
             max = st_bbox(poly_sf)$xmax)

coords <- as.data.frame(cbind(lon, lat))

points_sf <- st_as_sf(coords, coords = c('lon', 'lat'), crs = st_crs(world))

#check one by one and put the ones out inside of the range
#I know, not a smart not efficient solution. but it gives me pleasure.


##############################

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

points_sf <- st_as_sf(coords, coords = c('lon', 'lat'), crs = st_crs(world))


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


########################

#save new coordinates for the map
setwd(wd_tables)
write.csv(coords, 'Coordinates_points_large_map.csv', row.names = F)

#######################################################
######################## LOAD #########################
#######################################################

#load the coordinations when runinf from here
setwd(wd_tables)
coords <- read.csv('Coordinates_points_large_map.csv')

#create a points_sf object with the modified longitutes
points_sf_2 <- st_as_sf(coords, coords = c('lon', 'lat'), crs = st_crs(world))
points_sf_2$edgeDist <- st_distance()

#make boxes for the map

#get min and max coord values of the range
ext <- st_bbox(poly_sf)  

#calculate the size of the small box to plot around the range
side_box <-  max(c(abs(ext[3] - ext[1]), abs(ext[4] - ext[2]))) +
  min(c(abs(ext[3] - ext[1]), abs(ext[4] - ext[2]))) / 4

#calculate central lon and central lat of the range
central_x <- (ext[3] + ext[1]) / 2
names(central_x) <- 'xmean'
central_y <- (ext[4] + ext[2]) / 2
names(central_y) <- 'ymean'

#calculate min and max lon and mean and max lat of the small box
min_x <- central_x - (side_box / 2)
names(min_x) <- 'xmin'
max_x <- central_x + (side_box / 2)
names(max_x) <- 'xmax'

min_y <- central_y - (side_box / 2)
names(min_y) <- 'ymin'
max_y <- central_y + (side_box / 2)
names(max_y) <- 'ymax'
                          
#create big box
box_df <- data.frame(x = c(min_x,max_x,max_x,min_x,min_x), 
                     y = c(max_y,max_y,min_y,min_y,max_y))

box <- st_as_sfc(          #make the box    
  st_bbox(st_as_sf(box_df, coords = c('x', 'y'), crs = st_crs(poly_sf))))

#calculate the size of the box to plot in white in the background and give
#space to other stuff
side_big_box <-  max(c(abs(ext[3] - ext[1]), abs(ext[4] - ext[2]))) +
  min(c(abs(ext[3] - ext[1]), abs(ext[4] - ext[2]))) / 2

#calculate min and max lon and mean and max lat of the big box
min_x_BB <- central_x - (side_big_box / 1.4)
names(min_x_BB) <- 'xmin'
max_x_BB <- central_x + (side_big_box / 2)
names(max_x_BB) <- 'xmax'

min_y_BB <- central_y - (side_big_box / 2)
names(min_y_BB) <- 'ymin'
max_y_BB <- central_y + (side_big_box / 2)
names(max_y_BB) <- 'ymax'

#create big box
big_box_df <- data.frame(x = c(min_x_BB,max_x_BB,max_x_BB,min_x_BB,min_x_BB), 
                     y = c(max_y_BB,max_y_BB,min_y_BB,min_y_BB,max_y_BB))

big_box <- st_as_sfc(          #make the box    
  st_bbox(st_as_sf(big_box_df, coords = c('x', 'y'), crs = st_crs(poly_sf))))



###### PLOT GRAPH SHOWING THE LATITUDINAL GRADIENT ######


#include variable contribution info in the attribute table
points_sf_2$varContr <- varContr

#normalize variable contribution from 0 to 5 (for size)
points_sf_2$varContrNormal <- 
  round(((varContr + abs(min(varContr))) * 2) + 0.7, 2)

#set parametres for plotting
par(mar = c(5,5,5,5), pty="s", mfrow = c(1,1))

#minT
plot(relPolewardness, varContr, 
     pch = 19, cex = 1, col = '#0000FF50',
     ylab = 'Variable contribution',
     xlab = 'Relative polewardness',
     cex.lab = 1.2,
     cex.axis = 1.2,
     ylim = c(ylim[1], ylim[2]))

#fit linear model
lin_mod_minT <- lm(varContr ~ relPolewardness)
abline(lin_mod_minT, col = '#0000FF', lwd = 7)

#save plot (width = 800)

###### PLOT MAP SHOWING THE LATITUDINAL GRADIENT ######

#set parametres for plotting
par(mar = c(0,0,0,0), pty="s", mfrow = c(1,1))

#plot big white box to make room for the things I need to add around
plot(big_box, border = NA)

#plot small box to inform the coordinates
plot(box, add = T)

#plot the polygon
plot(poly_sf, lwd = 3, border = '#707070', col = '#F0F0F0', add = T)

#plot the occurrence points (size is proportional to SHAP value)
plot(st_geometry(points_sf_2),
     add = T, pch = 19, cex = points_sf_2$varContrNormal,
     col = '#0000FF80')

#label axes
text(-1, 18, 'Latitude', srt = 90, cex = 1.2)
text(21, -4.3, 'Longitude', srt = 00, cex = 1.2)

#add ticks to axes
points(st_bbox(box)[1] + (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
       st_bbox(box)[2] - 0.44,
       pch = '|',
       cex = 0.9)
points((st_bbox(box)[1] + st_bbox(box)[3]) / 2,
       st_bbox(box)[2] - 0.44,
       pch = '|',
       cex = 0.9)
points(st_bbox(box)[3] - (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
       st_bbox(box)[2] - 0.44,
       pch = '|',
       cex = 0.9)

points(st_bbox(box)[1] - 0.45,
       st_bbox(box)[2] + (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
       pch = '—',
       cex = 0.9)
points(st_bbox(box)[1] - 0.45,
       (st_bbox(box)[2] + st_bbox(box)[4]) / 2,
       pch = '—',
       cex = 0.9)
points(st_bbox(box)[1] - 0.45,
       st_bbox(box)[4] - (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
       pch = '—',
       cex = 0.9)

#add values to axes
text(st_bbox(box)[1] + (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
       st_bbox(box)[2] - 2.35,
     labels = paste0(round(
         st_bbox(box)[1] + (st_bbox(box)[3] - st_bbox(box)[1]) / 10, 2)),
     cex = 1.2)
text((st_bbox(box)[1] + st_bbox(box)[3]) / 2,
       st_bbox(box)[2] - 2.35,
     labels = paste0(round(
       (st_bbox(box)[1] + st_bbox(box)[3]) / 2, 2)),
     cex = 1.2)
text(st_bbox(box)[3] - (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
       st_bbox(box)[2] - 2.35,
     labels = paste0(round(
         st_bbox(box)[3] - (st_bbox(box)[3] - st_bbox(box)[1]) / 10, 2)),
     cex = 1.2)

text(st_bbox(box)[1] - 2.75,
       st_bbox(box)[2] + (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
     labels = paste0(round(
       st_bbox(box)[2] + (st_bbox(box)[4] - st_bbox(box)[2]) / 10, 2)),
     cex = 1.2, srt = 90)
text(st_bbox(box)[1] - 2.75,
       (st_bbox(box)[2] + st_bbox(box)[4]) / 2,
     labels = paste0(round(
       (st_bbox(box)[2] + st_bbox(box)[4]) / 2, 2)),
     cex = 1.2, srt = 90)
text(st_bbox(box)[1] - 2.75,
       st_bbox(box)[4] - (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
     labels = paste0(round(
       st_bbox(box)[4] - (st_bbox(box)[4] - st_bbox(box)[2]) / 10, 2)),
     cex = 1.2, srt = 90)
 
#save plot (width = 800)


##### Calculate distance from the edges to show the 'centralness'

#cut the range out of the box
range_cut <- st_difference(box, poly_sf)

#calculate the dist from each point to edge in km
points_sf_2$distEdge <- as.numeric(
  set_units(st_distance(points_sf_2, range_cut), km)[,1])

#calculate index (0-1) of distance to edge
points_sf_2$distEdgeNormal <- points_sf_2$distEdge / max(points_sf_2$distEdge)

#create vector for sizes in points
points_sf_2$centralCex <- ((7 - log(points_sf_2$distEdge)) / 1.3) + 
  rnorm(nrow(points_sf_2), mean = 0.25, sd = 0.4)

summary(points_sf_2$centralCex)

#create exemplary values of var contribution showing an increase towards edges
points_sf_2$varContrCentral <- (points_sf_2$centralCex - 1.2) / 5

summary(points_sf_2$varContrCentral)

###### PLOT GRAPH SHOWING THE CENTRE-EDGE GRADIENT ######

#set y and x lims
ylim <- c(-0.2, 0.4)
xlim <- c(0, 1)

#set parametres for plotting
par(mar = c(5,5,5,5), pty="s", mfrow = c(1,1))

#minT
plot(points_sf_2$distEdgeNormal, points_sf_2$varContrCentral, 
     pch = 19, cex = 1, col = '#0000FF50',
     ylab = 'Variable contribution',
     xlab = 'Distance from edge',
     cex.lab = 1.2,
     cex.axis = 1.2,
     ylim = c(ylim[1], ylim[2]))

#fit linear model
lin_mod_minT <- lm(points_sf_2$varContrCentral ~ points_sf_2$distEdgeNormal)
abline(lin_mod_minT, col = '#0000FF', lwd = 7)

#save plot (width = 800)

###### PLOT MAP SHOWING THE LATITUDINAL GRADIENT ######

#set parametres for plotting
par(mar = c(0,0,0,0), pty="s", mfrow = c(1,1))

#plot big white box to make room for the things I need to add around
plot(big_box, border = NA)

#plot small box to inform the coordinates
plot(box, add = T)

#plot the polygon
plot(poly_sf, lwd = 3, border = '#707070', col = '#F0F0F0', add = T)

#plot the occurrence points (size is proportional to SHAP value)
plot(st_geometry(points_sf_2),
     add = T, pch = 19, cex = points_sf_2$centralCex,
     col = '#0000FF80')

#label axes
text(-1, 18, 'Latitude', srt = 90, cex = 1.2)
text(21, -4.3, 'Longitude', srt = 00, cex = 1.2)

#add ticks to axes
points(st_bbox(box)[1] + (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
       st_bbox(box)[2] - 0.44,
       pch = '|',
       cex = 0.9)
points((st_bbox(box)[1] + st_bbox(box)[3]) / 2,
       st_bbox(box)[2] - 0.44,
       pch = '|',
       cex = 0.9)
points(st_bbox(box)[3] - (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
       st_bbox(box)[2] - 0.44,
       pch = '|',
       cex = 0.9)

points(st_bbox(box)[1] - 0.45,
       st_bbox(box)[2] + (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
       pch = '—',
       cex = 0.9)
points(st_bbox(box)[1] - 0.45,
       (st_bbox(box)[2] + st_bbox(box)[4]) / 2,
       pch = '—',
       cex = 0.9)
points(st_bbox(box)[1] - 0.45,
       st_bbox(box)[4] - (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
       pch = '—',
       cex = 0.9)

#add values to axes
text(st_bbox(box)[1] + (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
     st_bbox(box)[2] - 2.35,
     labels = paste0(round(
       st_bbox(box)[1] + (st_bbox(box)[3] - st_bbox(box)[1]) / 10, 2)),
     cex = 1.2)
text((st_bbox(box)[1] + st_bbox(box)[3]) / 2,
     st_bbox(box)[2] - 2.35,
     labels = paste0(round(
       (st_bbox(box)[1] + st_bbox(box)[3]) / 2, 2)),
     cex = 1.2)
text(st_bbox(box)[3] - (st_bbox(box)[3] - st_bbox(box)[1]) / 10,
     st_bbox(box)[2] - 2.35,
     labels = paste0(round(
       st_bbox(box)[3] - (st_bbox(box)[3] - st_bbox(box)[1]) / 10, 2)),
     cex = 1.2)

text(st_bbox(box)[1] - 2.75,
     st_bbox(box)[2] + (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
     labels = paste0(round(
       st_bbox(box)[2] + (st_bbox(box)[4] - st_bbox(box)[2]) / 10, 2)),
     cex = 1.2, srt = 90)
text(st_bbox(box)[1] - 2.75,
     (st_bbox(box)[2] + st_bbox(box)[4]) / 2,
     labels = paste0(round(
       (st_bbox(box)[2] + st_bbox(box)[4]) / 2, 2)),
     cex = 1.2, srt = 90)
text(st_bbox(box)[1] - 2.75,
     st_bbox(box)[4] - (st_bbox(box)[4] - st_bbox(box)[2]) / 10,
     labels = paste0(round(
       st_bbox(box)[4] - (st_bbox(box)[4] - st_bbox(box)[2]) / 10, 2)),
     cex = 1.2, srt = 90)

#save plot (width = 800)
