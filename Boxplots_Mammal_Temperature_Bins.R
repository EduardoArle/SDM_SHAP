#load packages

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'

#read temperature results table

setwd(wd_tables)
res_temp <- read.csv('Temperature_Rel_Polar_all_points_order.csv')

#calculate species' latitudinal amplitude
res_temp$latAmplitude <- res_temp$maxLat - res_temp$minLat


#################################################################
################### RELATIVE POLARWARDNESS ######################
#################################################################


############################
######### MIN TEMP ########
############################


###### check relationships by latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges_all <- res_temp_large_ranges[which
                (res_temp_large_ranges$latAmplitude >= 0),]

pos_latAmp_all <- sum(res_temp_large_ranges_all$minTslope_RP > 0) /
  nrow(res_temp_large_ranges_all) * 100
neg_latAmp_all <- sum(res_temp_large_ranges_all$minTslope_RP < 0) /
  nrow(res_temp_large_ranges_all) * 100

#### 0 to 5 degrees
res_temp_large_ranges_0_5 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 0 &
        res_temp_large_ranges$latAmplitude < 5),]

pos_latAmp_0_5 <- sum(res_temp_large_ranges_0_5$minTslope_RP > 0) /
  nrow(res_temp_large_ranges_0_5) * 100
neg_latAmp_0_5 <- sum(res_temp_large_ranges_0_5$minTslope_RP < 0) /
  nrow(res_temp_large_ranges_0_5) * 100

#### 5 to 10 degrees
res_temp_large_ranges_5_10 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 5 &
        res_temp_large_ranges$latAmplitude < 10),]

pos_latAmp_5_10 <- sum(res_temp_large_ranges_5_10$minTslope_RP > 0) /
  nrow(res_temp_large_ranges_5_10) * 100
neg_latAmp_5_10 <- sum(res_temp_large_ranges_5_10$minTslope_RP < 0) /
  nrow(res_temp_large_ranges_5_10) * 100

#### 10 to 15 degrees
res_temp_large_ranges_10_15 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 10 &
        res_temp_large_ranges$latAmplitude < 15),]

pos_latAmp_10_15 <- sum(res_temp_large_ranges_10_15$minTslope_RP > 0) /
  nrow(res_temp_large_ranges_10_15) * 100
neg_latAmp_10_15 <- sum(res_temp_large_ranges_10_15$minTslope_RP < 0) /
  nrow(res_temp_large_ranges_10_15) * 100

#### 15 to 20 degrees
res_temp_large_ranges_15_20 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 15 &
        res_temp_large_ranges$latAmplitude < 20),]

pos_latAmp_15_20 <- sum(res_temp_large_ranges_15_20$minTslope_RP > 0) /
  nrow(res_temp_large_ranges_15_20) * 100
neg_latAmp_15_20 <- sum(res_temp_large_ranges_15_20$minTslope_RP < 0) /
  nrow(res_temp_large_ranges_15_20) * 100

#### more than 20 degrees
res_temp_large_ranges_20 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 20),]

pos_latAmp_20 <- sum(res_temp_large_ranges_20$minTslope_RP > 0) /
  nrow(res_temp_large_ranges_20) * 100
neg_latAmp_20 <- sum(res_temp_large_ranges_20$minTslope_RP < 0) /
  nrow(res_temp_large_ranges_20) * 100

#create a matrix with the data
dt_latAmp <- matrix(c(c(pos_latAmp_all, neg_latAmp_all),
                      c(pos_latAmp_0_5, neg_latAmp_0_5),
                      c(pos_latAmp_5_10, neg_latAmp_5_10),
                      c(pos_latAmp_10_15, neg_latAmp_10_15),
                      c(pos_latAmp_15_20, neg_latAmp_15_20),
                      c(pos_latAmp_20, neg_latAmp_20)),
                    ncol = 6)

#name the columns
colnames(dt_latAmp) <-
  c('all', 'LA_0_5', 'LA_5_10', 'LA_10_15', 'LA_15_20', 'LA_20')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_xx <- 
  matrix(c(c(pos_latAmp_all, neg_latAmp_all, xx, xx, xx, xx, xx),
           c(xx, pos_latAmp_0_5, neg_latAmp_0_5, xx, xx, xx, xx),
           c(xx, xx, pos_latAmp_5_10, neg_latAmp_5_10, xx, xx, xx),
           c(xx, xx, xx, pos_latAmp_10_15, neg_latAmp_10_15, xx, xx),
           c(xx, xx, xx, xx, pos_latAmp_15_20, neg_latAmp_15_20, xx),
           c(xx, xx, xx, xx, xx, pos_latAmp_20, neg_latAmp_20)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_xx) <-
  c('all', 'LA_0_5', 'LA_5_10', 'LA_10_15', 'LA_15_20', 'LA_20')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_xx,
        col = col,
        cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
        legend = NA, main = 'Min T Lat Amplitude')

text(x= bp[1], y = dt_latAmp[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_all$minTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp[2,1]/2 + dt_latAmp[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_all$minTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_0_5$minTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp[2,2]/2 + dt_latAmp[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_0_5$minTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_5_10$minTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp[2,3]/2 + dt_latAmp[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_5_10$minTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_10_15$minTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp[2,4]/2 + dt_latAmp[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_10_15$minTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_15_20$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp[2,5]/2 + dt_latAmp[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_15_20$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_20$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp[2,6]/2 + dt_latAmp[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_20$minTslope_RP < 0)))

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_rangeSize <- res_temp[which(res_temp$minTslope_RP != 0),]

#### all amplitudes
res_temp_rangeSize_all <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_rangeSize_all$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_all) * 100
neg_rangeSize_all <- sum(res_temp_rangeSize_all$minTslope_RP < 0) /
  nrow(res_temp_rangeSize_all) * 100

#### less than 100000 km2
res_temp_rangeSize_0_100k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize < 100000),]

pos_rangeSize_0_100k <- sum(res_temp_rangeSize_0_100k$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_0_100k) * 100
neg_rangeSize_0_100k <- sum(res_temp_rangeSize_0_100k$minTslope_RP < 0) /
  nrow(res_temp_rangeSize_0_100k) * 100

#### 100000 t0 500000 km2
res_temp_rangeSize_100_500k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 100000 &
        res_temp_rangeSize$rangeSize < 500000),]

pos_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_100_500k ) * 100
neg_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$minTslope_RP < 0) /
  nrow(res_temp_rangeSize_100_500k ) * 100

#### 500000 to 1000000 km2
res_temp_rangeSize_500k_1M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 500000 &
        res_temp_rangeSize$rangeSize < 1000000),]

pos_rangeSize_500k_1M <- 
  sum(res_temp_rangeSize_500k_1M$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100
neg_rangeSize_500k_1M <-
  sum(res_temp_rangeSize_500k_1M $minTslope_RP < 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100

#### 1000000 to 2000000 km2
res_temp_rangeSize_1_2M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 1000000 &
          res_temp_rangeSize$rangeSize < 2000000),]

pos_rangeSize_1_2M <- 
  sum(res_temp_rangeSize_1_2M$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100
neg_rangeSize_1_2M <-
  sum(res_temp_rangeSize_1_2M $minTslope_RP < 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100

#### 2000000 to 3000000 km2
res_temp_rangeSize_2_3M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 2000000 &
          res_temp_rangeSize$rangeSize < 3000000),]

pos_rangeSize_2_3M <- 
  sum(res_temp_rangeSize_2_3M$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100
neg_rangeSize_2_3M <-
  sum(res_temp_rangeSize_2_3M $minTslope_RP < 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100

#### more than 3000000 km2
res_temp_rangeSize_3M  <- res_temp_large_ranges[
  which(res_temp_rangeSize$rangeSize  >= 3000000),]

pos_rangeSize_3M <- 
  sum(res_temp_rangeSize_3M$minTslope_RP > 0) /
  nrow(res_temp_rangeSize_3M) * 100
neg_rangeSize_3M <-
  sum(res_temp_rangeSize_3M $minTslope_RP < 0) /
  nrow(res_temp_rangeSize_3M ) * 100

#create a matrix with the data
dt_rangeSize <-
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all),
           c(pos_rangeSize_0_100k, neg_rangeSize_0_100k),
           c(pos_rangeSize_100_500k, neg_rangeSize_100_500k),
           c(pos_rangeSize_500k_1M, neg_rangeSize_500k_1M),
           c(pos_rangeSize_1_2M, neg_rangeSize_1_2M),
           c(pos_rangeSize_2_3M, neg_rangeSize_2_3M),
           c(pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)

#name the columns
colnames(dt_rangeSize) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_rangeSize_xx <- 
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all,xx,xx,xx,xx,xx,xx),
           c(xx,pos_rangeSize_0_100k, neg_rangeSize_0_100k, xx,xx,xx,xx,xx),
           c(xx,xx,pos_rangeSize_100_500k,neg_rangeSize_100_500k,xx,xx,xx,xx),
           c(xx,xx,xx,pos_rangeSize_500k_1M, neg_rangeSize_500k_1M,xx,xx,xx),
           c(xx,xx,xx,xx,pos_rangeSize_1_2M, neg_rangeSize_1_2M, xx,xx),
           c(xx,xx,xx,xx,xx,pos_rangeSize_2_3M, neg_rangeSize_2_3M,xx),
           c(xx,xx,xx,xx,xx,xx,pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize_xx) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_rangeSize_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Range Size')

text(x= bp[1], y = dt_rangeSize[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$minTslope_RP > 0)))
text(x= bp[1], y = dt_rangeSize[2,1]/2 + dt_rangeSize[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$minTslope_RP < 0)))

text(x= bp[2], y = dt_rangeSize[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$minTslope_RP > 0)))
text(x= bp[2], y = dt_rangeSize[2,2]/2 + dt_rangeSize[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$minTslope_RP < 0)))

text(x= bp[3], y= dt_rangeSize[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$minTslope_RP > 0)))
text(x= bp[3], y= dt_rangeSize[2,3]/2 + dt_rangeSize[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$minTslope_RP < 0)))

text(x= bp[4], y= dt_rangeSize[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$minTslope_RP > 0)))
text(x= bp[4], y= dt_rangeSize[2,4]/2 + dt_rangeSize[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$minTslope_RP < 0)))

text(x= bp[5], y= dt_rangeSize[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$minTslope_RP > 0)))
text(x= bp[5], y= dt_rangeSize[2,5]/2 + dt_rangeSize[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$minTslope_RP < 0)))

text(x= bp[6], y= dt_rangeSize[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$minTslope_RP > 0)))
text(x= bp[6], y= dt_rangeSize[2,6]/2 + dt_rangeSize[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$minTslope_RP < 0)))

text(x= bp[7], y= dt_rangeSize[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$minTslope_RP > 0)))
# text(x= bp[7], y= dt_rangeSize[2,7]/2 + dt_rangeSize[1,7],
#      pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
#      labels = paste(sum(res_temp_rangeSize_3M$minTslope_RP < 0)))

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by mean lat

#eliminate 0s and NAs in the slope
res_temp_meanLat <- res_temp[which(res_temp$minTslope_RP != 0),]

#make a mean lat col
res_temp_meanLat$meanLat <- 
  (res_temp_meanLat$maxLat + res_temp_meanLat$minLat) / 2

#### all lats
res_temp_meanLat_all <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat > 0),]

pos_meanLat_all <- sum(res_temp_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_meanLat_all) * 100
neg_meanLat_all <- sum(res_temp_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_meanLat_0_10 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat < 10),]

pos_meanLat_0_10 <- sum(res_temp_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_meanLat_0_10) * 100
neg_meanLat_0_10 <- sum(res_temp_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_meanLat_10_20 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 10 &
        res_temp_meanLat$meanLat < 20),]

pos_meanLat_10_20 <- sum(res_temp_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_meanLat_10_20) * 100
neg_meanLat_10_20 <- sum(res_temp_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_meanLat_20_30 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 20 &
        res_temp_meanLat$meanLat < 30),]

pos_meanLat_20_30 <- sum(res_temp_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_meanLat_20_30) * 100
neg_meanLat_20_30 <- sum(res_temp_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_meanLat_30_40 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 30 &
        res_temp_meanLat$meanLat < 40),]

pos_meanLat_30_40 <- sum(res_temp_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_meanLat_30_40) * 100
neg_meanLat_30_40 <- sum(res_temp_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_meanLat_40_50 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 40 &
        res_temp_meanLat$meanLat < 50),]

pos_meanLat_40_50 <- sum(res_temp_meanLat_40_50$minTslope_RP > 0) /
  nrow(res_temp_meanLat_40_50) * 100
neg_meanLat_40_50 <- sum(res_temp_meanLat_40_50$minTslope_RP < 0) /
  nrow(res_temp_meanLat_40_50) * 100

#### more than 50 degress
res_temp_meanLat_50 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 50),]

pos_meanLat_50 <- sum(res_temp_meanLat_50$minTslope_RP > 0) /
  nrow(res_temp_meanLat_50) * 100
neg_meanLat_50 <- sum(res_temp_meanLat_50$minTslope_RP < 0) /
  nrow(res_temp_meanLat_50) * 100

#create a matrix with the data
dt_meanLat <-
  matrix(c(c(pos_meanLat_all, neg_meanLat_all),
           c(pos_meanLat_0_10, neg_meanLat_0_10),
           c(pos_meanLat_10_20, neg_meanLat_10_20),
           c(pos_meanLat_20_30, neg_meanLat_20_30),
           c(pos_meanLat_30_40, neg_meanLat_30_40),
           c(pos_meanLat_40_50, neg_meanLat_40_50),
           c(pos_meanLat_50, neg_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_meanLat_xx <-
  matrix(c(c(pos_meanLat_all, neg_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_meanLat_0_10, neg_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_meanLat_10_20, neg_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_meanLat_20_30, neg_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_meanLat_30_40, neg_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_meanLat_40_50, neg_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_meanLat_50, neg_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_meanLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude')

text(x= bp[1], y = dt_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat$minTslope_RP > 0)))
text(x= bp[1], y = dt_minLat[2,1]/2 + dt_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat$minTslope_RP < 0)))

text(x= bp[2], y = dt_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2], y = dt_minLat[2,2]/2 + dt_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3], y= dt_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3], y= dt_minLat[2,3]/2 + dt_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4], y= dt_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4], y= dt_minLat[2,4]/2 + dt_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5], y= dt_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_minLat[2,5]/2 + dt_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_40_50$minTslope_RP > 0)))
text(x= bp[6], y= dt_minLat[2,6]/2 + dt_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_40_50$minTslope_RP < 0)))

text(x= bp[7], y= dt_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_50$minTslope_RP > 0)))
text(x= bp[7], y= dt_minLat[2,7]/2 + dt_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_50$minTslope_RP < 0)))


###### check relationships by mean lat per latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_latAmp_meanLat <- res_temp[which(res_temp$minTslope_RP != 0),]

#make a mean lat col
res_temp_latAmp_meanLat$meanLat <- 
  (res_temp_latAmp_meanLat$maxLat + res_temp_latAmp_meanLat$minLat) / 2

##### latAmp < 5 degrees
res_temp_latAmp_0_5_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude < 5,]

#### all lats
res_temp_latAmp_0_5_meanLat_all <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat > 0),]

pos_latAmp_0_5_meanLat_all <- 
  sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_all) * 100
neg_latAmp_0_5_meanLat_all <- 
  sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_0_5_meanLat_0_10 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat < 10),]

pos_latAmp_0_5_meanLat_0_10 <-
  sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_0_10) * 100
neg_latAmp_0_5_meanLat_0_10 <-
  sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_0_5_meanLat_10_20 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 10 &
        res_temp_latAmp_0_5_meanLat$meanLat < 20),]

pos_latAmp_0_5_meanLat_10_20 <-
  sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_10_20) * 100
neg_latAmp_0_5_meanLat_10_20 <-
  sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_0_5_meanLat_20_30 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 20 &
        res_temp_latAmp_0_5_meanLat$meanLat < 30),]

pos_latAmp_0_5_meanLat_20_30 <-
  sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_20_30) * 100
neg_latAmp_0_5_meanLat_20_30 <-
  sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_0_5_meanLat_30_40 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 30 &
        res_temp_latAmp_0_5_meanLat$meanLat < 40),]

pos_latAmp_0_5_meanLat_30_40 <-
  sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_30_40) * 100
neg_latAmp_0_5_meanLat_30_40 <-
  sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_latAmp_0_5_meanLat_40_50 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 40 &
        res_temp_latAmp_0_5_meanLat$meanLat < 50),]

pos_latAmp_0_5_meanLat_40_50 <-
  sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_40_50) * 100
neg_latAmp_0_5_meanLat_40_50 <-
  sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_40_50) * 100

#### more than 50 degress
res_temp_latAmp_0_5_meanLat_50 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 50),]

pos_latAmp_0_5_meanLat_50 <-
  sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_50) * 100
neg_latAmp_0_5_meanLat_50 <-
  sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_50) * 100

#create a matrix with the data
dt_latAmp_0_5_minLat <-
  matrix(c(c(pos_latAmp_0_5_meanLat_all, neg_latAmp_0_5_meanLat_all),
           c(pos_latAmp_0_5_meanLat_0_10, neg_latAmp_0_5_meanLat_0_10),
           c(pos_latAmp_0_5_meanLat_10_20, neg_latAmp_0_5_meanLat_10_20),
           c(pos_latAmp_0_5_meanLat_20_30, neg_latAmp_0_5_meanLat_20_30),
           c(pos_latAmp_0_5_meanLat_30_40, neg_latAmp_0_5_meanLat_30_40),
           c(pos_latAmp_0_5_meanLat_40_50, neg_latAmp_0_5_meanLat_40_50),
           c(pos_latAmp_0_5_meanLat_50, neg_latAmp_0_5_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_0_5_minLat_xx <-
  matrix(c(c(pos_latAmp_0_5_meanLat_all,
             neg_latAmp_0_5_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_0_5_meanLat_0_10,
             neg_latAmp_0_5_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_0_5_meanLat_10_20,
             neg_latAmp_0_5_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_0_5_meanLat_20_30,
             neg_latAmp_0_5_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_0_5_meanLat_30_40,
             neg_latAmp_0_5_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_0_5_meanLat_40_50,
             neg_latAmp_0_5_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_latAmp_0_5_meanLat_50,
             neg_latAmp_0_5_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_0_5_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_0_5_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude (latAmp 0 - 5)')

text(x= bp[1], y = dt_latAmp_0_5_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp_0_5_minLat[2,1]/2 + dt_latAmp_0_5_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp_0_5_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp_0_5_minLat[2,2]/2 + dt_latAmp_0_5_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp_0_5_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp_0_5_minLat[2,3]/2 + dt_latAmp_0_5_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp_0_5_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp_0_5_minLat[2,4]/2 + dt_latAmp_0_5_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp_0_5_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_0_5_minLat[2,5]/2 + dt_latAmp_0_5_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_0_5_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_0_5_minLat[2,6]/2 + dt_latAmp_0_5_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP < 0)))

text(x= bp[7], y= dt_latAmp_0_5_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP > 0)))
text(x= bp[7], y= dt_latAmp_0_5_minLat[2,7]/2 + dt_latAmp_0_5_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP < 0)))


#####. latAmp 5 to 10 degrees
res_temp_latAmp_5_10_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 5 &
                          res_temp_latAmp_meanLat$latAmplitude < 10,]

#### all lats
res_temp_latAmp_5_10_meanLat_all <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat > 0),]

pos_latAmp_5_10_meanLat_all <- 
  sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_all) * 100
neg_latAmp_5_10_meanLat_all <- 
  sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_5_10_meanLat_0_10 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat < 10),]

pos_latAmp_5_10_meanLat_0_10 <-
  sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_0_10) * 100
neg_latAmp_5_10_meanLat_0_10 <-
  sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_5_10_meanLat_10_20 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 10 &
        res_temp_latAmp_5_10_meanLat$meanLat < 20),]

pos_latAmp_5_10_meanLat_10_20 <-
  sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_10_20) * 100
neg_latAmp_5_10_meanLat_10_20 <-
  sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_5_10_meanLat_20_30 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 20 &
        res_temp_latAmp_5_10_meanLat$meanLat < 30),]

pos_latAmp_5_10_meanLat_20_30 <-
  sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_20_30) * 100
neg_latAmp_5_10_meanLat_20_30 <-
  sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_5_10_meanLat_30_40 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 30 &
          res_temp_latAmp_5_10_meanLat$meanLat < 40),]

pos_latAmp_5_10_meanLat_30_40 <-
  sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_30_40) * 100
neg_latAmp_5_10_meanLat_30_40 <-
  sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_latAmp_5_10_meanLat_40_50 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 40 &
          res_temp_latAmp_5_10_meanLat$meanLat < 50),]

pos_latAmp_5_10_meanLat_40_50 <-
  sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_40_50) * 100
neg_latAmp_5_10_meanLat_40_50 <-
  sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_40_50) * 100

#### more than 50 degress
res_temp_latAmp_5_10_meanLat_50 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 50),]

pos_latAmp_5_10_meanLat_50 <-
  sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_50) * 100
neg_latAmp_5_10_meanLat_50 <-
  sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_50) * 100

#create a matrix with the data
dt_latAmp_5_10_minLat <-
  matrix(c(c(pos_latAmp_5_10_meanLat_all, neg_latAmp_5_10_meanLat_all),
           c(pos_latAmp_5_10_meanLat_0_10, neg_latAmp_5_10_meanLat_0_10),
           c(pos_latAmp_5_10_meanLat_10_20, neg_latAmp_5_10_meanLat_10_20),
           c(pos_latAmp_5_10_meanLat_20_30, neg_latAmp_5_10_meanLat_20_30),
           c(pos_latAmp_5_10_meanLat_30_40, neg_latAmp_5_10_meanLat_30_40),
           c(pos_latAmp_5_10_meanLat_40_50, neg_latAmp_5_10_meanLat_40_50),
           c(pos_latAmp_5_10_meanLat_50, neg_latAmp_5_10_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_5_10_minLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_5_10_minLat_xx <-
  matrix(c(c(pos_latAmp_5_10_meanLat_all,
             neg_latAmp_5_10_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_5_10_meanLat_0_10,
             neg_latAmp_5_10_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_5_10_meanLat_10_20,
             neg_latAmp_5_10_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_5_10_meanLat_20_30,
             neg_latAmp_5_10_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_5_10_meanLat_30_40,
             neg_latAmp_5_10_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_5_10_meanLat_40_50,
             neg_latAmp_5_10_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_latAmp_5_10_meanLat_50,
             neg_latAmp_5_10_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_5_10_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_5_10_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude (latAmp 5 - 10)')


text(x= bp[1], y = dt_latAmp_5_10_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp_5_10_minLat[2,1]/2 + dt_latAmp_5_10_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp_5_10_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp_5_10_minLat[2,2]/2 + dt_latAmp_5_10_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp_5_10_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp_5_10_minLat[2,3]/2 + dt_latAmp_5_10_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp_5_10_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp_5_10_minLat[2,4]/2 + dt_latAmp_5_10_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp_5_10_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_5_10_minLat[2,5]/2 + dt_latAmp_5_10_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_5_10_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_5_10_minLat[2,6]/2 + dt_latAmp_5_10_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP < 0)))

text(x= bp[7], y= dt_latAmp_5_10_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP > 0)))
text(x= bp[7], y= dt_latAmp_5_10_minLat[2,7]/2 + dt_latAmp_5_10_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP < 0)))



#####. latAmp 10 to 15 degrees
res_temp_latAmp_10_15_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 10 &
                            res_temp_latAmp_meanLat$latAmplitude < 15,]

#### all lats
res_temp_latAmp_10_15_meanLat_all <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat > 0),]

pos_latAmp_10_15_meanLat_all <- 
  sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_all) * 100
neg_latAmp_10_15_meanLat_all <- 
  sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_10_15_meanLat_0_10 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat < 10),]

pos_latAmp_10_15_meanLat_0_10 <-
  sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_0_10) * 100
neg_latAmp_10_15_meanLat_0_10 <-
  sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_10_15_meanLat_10_20 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 10 &
          res_temp_latAmp_10_15_meanLat$meanLat < 20),]

pos_latAmp_10_15_meanLat_10_20 <-
  sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_10_20) * 100
neg_latAmp_10_15_meanLat_10_20 <-
  sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_10_15_meanLat_20_30 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 20 &
        res_temp_latAmp_10_15_meanLat$meanLat < 30),]

pos_latAmp_10_15_meanLat_20_30 <-
  sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_20_30) * 100
neg_latAmp_10_15_meanLat_20_30 <-
  sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_10_15_meanLat_30_40 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 30 &
          res_temp_latAmp_10_15_meanLat$meanLat < 40),]

pos_latAmp_10_15_meanLat_30_40 <-
  sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_30_40) * 100
neg_latAmp_10_15_meanLat_30_40 <-
  sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_30_40) * 100

#### more than 40 degress
res_temp_latAmp_10_15_meanLat_40 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 40),]

pos_latAmp_10_15_meanLat_40 <-
  sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_40) * 100
neg_latAmp_10_15_meanLat_40 <-
  sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_40) * 100

#create a matrix with the data
dt_latAmp_10_15_minLat <-
  matrix(c(c(pos_latAmp_10_15_meanLat_all, neg_latAmp_10_15_meanLat_all),
           c(pos_latAmp_10_15_meanLat_0_10, neg_latAmp_10_15_meanLat_0_10),
           c(pos_latAmp_10_15_meanLat_10_20, neg_latAmp_10_15_meanLat_10_20),
           c(pos_latAmp_10_15_meanLat_20_30, neg_latAmp_10_15_meanLat_20_30),
           c(pos_latAmp_10_15_meanLat_30_40, neg_latAmp_10_15_meanLat_30_40),
           c(pos_latAmp_10_15_meanLat_40, neg_latAmp_10_15_meanLat_40)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_10_15_minLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_10_15_minLat_xx <-
  matrix(c(c(pos_latAmp_10_15_meanLat_all,
             neg_latAmp_10_15_meanLat_all, xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_10_15_meanLat_0_10,
             neg_latAmp_10_15_meanLat_0_10, xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_10_15_meanLat_10_20,
             neg_latAmp_10_15_meanLat_10_20, xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_10_15_meanLat_20_30,
             neg_latAmp_10_15_meanLat_20_30, xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_10_15_meanLat_30_40,
             neg_latAmp_10_15_meanLat_30_40, xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_10_15_meanLat_40,
             neg_latAmp_10_15_meanLat_40)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_10_15_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_10_15_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude (latAmp 10 - 15)')


text(x= bp[1], 
     y = dt_latAmp_10_15_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], 
     y = dt_latAmp_10_15_minLat[2,1]/2 + dt_latAmp_10_15_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], 
     y = dt_latAmp_10_15_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2],
     y = dt_latAmp_10_15_minLat[2,2]/2 + dt_latAmp_10_15_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3],
     y= dt_latAmp_10_15_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = 
       paste(sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3],
     y= dt_latAmp_10_15_minLat[2,3]/2 + dt_latAmp_10_15_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4],
     y= dt_latAmp_10_15_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4],
     y= dt_latAmp_10_15_minLat[2,4]/2 + dt_latAmp_10_15_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5],
     y= dt_latAmp_10_15_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_10_15_minLat[2,5]/2 + dt_latAmp_10_15_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_10_15_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_10_15_minLat[2,6]/2 + dt_latAmp_10_15_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP < 0)))



#####. latAmp more than 15 degrees
res_temp_latAmp_15_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 15,]

#### all lats
res_temp_latAmp_15_meanLat_all <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat > 0),]

pos_latAmp_15_meanLat_all <- 
  sum(res_temp_latAmp_15_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_all) * 100
neg_latAmp_15_meanLat_all <- 
  sum(res_temp_latAmp_15_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_all) * 100

#### 0 to 30 degrees
res_temp_latAmp_15_meanLat_0_30 <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat < 30),]

pos_latAmp_15_meanLat_0_30 <-
  sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_0_30) * 100
neg_latAmp_15_meanLat_0_30 <-
  sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_0_30) * 100

#### more than 30 degrees
res_temp_latAmp_15_meanLat_30 <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat >= 30 &
          res_temp_latAmp_15_meanLat$meanLat < 90),]

pos_latAmp_15_meanLat_30 <-
  sum(res_temp_latAmp_15_meanLat_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_30) * 100
neg_latAmp_15_meanLat_30 <-
  sum(res_temp_latAmp_15_meanLat_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_30) * 100

#create a matrix with the data
dt_latAmp_15_minLat <-
  matrix(c(c(pos_latAmp_15_meanLat_all, neg_latAmp_15_meanLat_all),
           c(pos_latAmp_15_meanLat_0_30, neg_latAmp_15_meanLat_0_30),
           c(pos_latAmp_15_meanLat_30, neg_latAmp_15_meanLat_30)),
         ncol = 3)

#name the columns
colnames(dt_latAmp_15_minLat) <-
  c('all', 'Lat_0_30', 'Lat_30')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_15_minLat_xx <-
  matrix(c(c(pos_latAmp_15_meanLat_all,
             neg_latAmp_15_meanLat_all, xx,xx),
           c(xx, pos_latAmp_15_meanLat_0_30,
             neg_latAmp_15_meanLat_0_30, xx),
           c(xx,xx, pos_latAmp_15_meanLat_30,
             neg_latAmp_15_meanLat_30)),
         ncol = 3)

#name the columns
colnames(dt_latAmp_15_minLat_xx) <- c('all', 'Lat_0_30', 'Lat_30')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_15_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude (latAmp > 15)')


text(x= bp[1], 
     y = dt_latAmp_15_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], 
     y = dt_latAmp_15_minLat[2,1]/2 + dt_latAmp_15_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], 
     y = dt_latAmp_15_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP > 0)))
text(x= bp[2],
     y = dt_latAmp_15_minLat[2,2]/2 + dt_latAmp_15_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP < 0)))

text(x= bp[3],
     y= dt_latAmp_15_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_30$minTslope_RP > 0)))
text(x= bp[3],
     y= dt_latAmp_15_minLat[2,3]/2 + dt_latAmp_15_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_30$minTslope_RP < 0)))











############################
######### MEAN TEMP ########
############################


###### check relationships by latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges_all <- 
  res_temp_large_ranges[which(res_temp_large_ranges$latAmplitude >= 0),]

pos_latAmp_all <- sum(res_temp_large_ranges_all$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges_all) * 100
neg_latAmp_all <- sum(res_temp_large_ranges_all$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges_all) * 100

#### 0 to 5 degrees
res_temp_large_ranges_0_5 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 0 &
          res_temp_large_ranges$latAmplitude < 5),]

pos_latAmp_0_5 <- sum(res_temp_large_ranges_0_5$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges_0_5) * 100
neg_latAmp_0_5 <- sum(res_temp_large_ranges_0_5$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges_0_5) * 100

#### 5 to 10 degrees
res_temp_large_ranges_5_10 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 5 &
          res_temp_large_ranges$latAmplitude < 10),]

pos_latAmp_5_10 <- sum(res_temp_large_ranges_5_10$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges_5_10) * 100
neg_latAmp_5_10 <- sum(res_temp_large_ranges_5_10$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges_5_10) * 100

#### 10 to 15 degrees
res_temp_large_ranges_10_15 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 10 &
          res_temp_large_ranges$latAmplitude < 15),]

pos_latAmp_10_15 <- sum(res_temp_large_ranges_10_15$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges_10_15) * 100
neg_latAmp_10_15 <- sum(res_temp_large_ranges_10_15$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges_10_15) * 100

#### 15 to 20 degrees
res_temp_large_ranges_15_20 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 15 &
          res_temp_large_ranges$latAmplitude < 20),]

pos_latAmp_15_20 <- sum(res_temp_large_ranges_15_20$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges_15_20) * 100
neg_latAmp_15_20 <- sum(res_temp_large_ranges_15_20$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges_15_20) * 100

#### more than 20 degrees
res_temp_large_ranges_20 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 20),]

pos_latAmp_20 <- sum(res_temp_large_ranges_20$meanTslope_RP > 0) /
  nrow(res_temp_large_ranges_20) * 100
neg_latAmp_20 <- sum(res_temp_large_ranges_20$meanTslope_RP < 0) /
  nrow(res_temp_large_ranges_20) * 100

#create a matrix with the data
dt_latAmp <- matrix(c(c(pos_latAmp_all, neg_latAmp_all),
                      c(pos_latAmp_0_5, neg_latAmp_0_5),
                      c(pos_latAmp_5_10, neg_latAmp_5_10),
                      c(pos_latAmp_10_15, neg_latAmp_10_15),
                      c(pos_latAmp_15_20, neg_latAmp_15_20),
                      c(pos_latAmp_20, neg_latAmp_20)),
                    ncol = 6)

#name the columns
colnames(dt_latAmp) <-
  c('all', 'LA_0_5', 'LA_5_10', 'LA_10_15', 'LA_15_20', 'LA_20')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_xx <- 
  matrix(c(c(pos_latAmp_all, neg_latAmp_all, xx, xx, xx, xx, xx),
           c(xx, pos_latAmp_0_5, neg_latAmp_0_5, xx, xx, xx, xx),
           c(xx, xx, pos_latAmp_5_10, neg_latAmp_5_10, xx, xx, xx),
           c(xx, xx, xx, pos_latAmp_10_15, neg_latAmp_10_15, xx, xx),
           c(xx, xx, xx, xx, pos_latAmp_15_20, neg_latAmp_15_20, xx),
           c(xx, xx, xx, xx, xx, pos_latAmp_20, neg_latAmp_20)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_xx) <-
  c('all', 'LA_0_5', 'LA_5_10', 'LA_10_15', 'LA_15_20', 'LA_20')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Lat Amplitude')

text(x= bp[1], y = dt_latAmp[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_all$meanTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp[2,1]/2 + dt_latAmp[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_all$meanTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_0_5$meanTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp[2,2]/2 + dt_latAmp[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_0_5$meanTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_5_10$meanTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp[2,3]/2 + dt_latAmp[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_5_10$meanTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_10_15$meanTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp[2,4]/2 + dt_latAmp[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_10_15$meanTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_15_20$meanTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp[2,5]/2 + dt_latAmp[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_15_20$meanTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_20$meanTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp[2,6]/2 + dt_latAmp[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_20$meanTslope_RP < 0)))

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_rangeSize <- res_temp[which(res_temp$meanTslope_RP != 0),]

#### all amplitudes
res_temp_rangeSize_all <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_rangeSize_all$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_all) * 100
neg_rangeSize_all <- sum(res_temp_rangeSize_all$meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_all) * 100

#### less than 100000 km2
res_temp_rangeSize_0_100k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize < 100000),]

pos_rangeSize_0_100k <- sum(res_temp_rangeSize_0_100k$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_0_100k) * 100
neg_rangeSize_0_100k <- sum(res_temp_rangeSize_0_100k$meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_0_100k) * 100

#### 100000 t0 500000 km2
res_temp_rangeSize_100_500k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 100000 &
          res_temp_rangeSize$rangeSize < 500000),]

pos_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_100_500k ) * 100
neg_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_100_500k ) * 100

#### 500000 to 1000000 km2
res_temp_rangeSize_500k_1M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 500000 &
          res_temp_rangeSize$rangeSize < 1000000),]

pos_rangeSize_500k_1M <- 
  sum(res_temp_rangeSize_500k_1M$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100
neg_rangeSize_500k_1M <-
  sum(res_temp_rangeSize_500k_1M $meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100

#### 1000000 to 2000000 km2
res_temp_rangeSize_1_2M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 1000000 &
          res_temp_rangeSize$rangeSize < 2000000),]

pos_rangeSize_1_2M <- 
  sum(res_temp_rangeSize_1_2M$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100
neg_rangeSize_1_2M <-
  sum(res_temp_rangeSize_1_2M $meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100

#### 2000000 to 3000000 km2
res_temp_rangeSize_2_3M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 2000000 &
          res_temp_rangeSize$rangeSize < 3000000),]

pos_rangeSize_2_3M <- 
  sum(res_temp_rangeSize_2_3M$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100
neg_rangeSize_2_3M <-
  sum(res_temp_rangeSize_2_3M $meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100

#### more than 3000000 km2
res_temp_rangeSize_3M  <- res_temp_large_ranges[
  which(res_temp_rangeSize$rangeSize  >= 3000000),]

pos_rangeSize_3M <- 
  sum(res_temp_rangeSize_3M$meanTslope_RP > 0) /
  nrow(res_temp_rangeSize_3M) * 100
neg_rangeSize_3M <-
  sum(res_temp_rangeSize_3M $meanTslope_RP < 0) /
  nrow(res_temp_rangeSize_3M ) * 100

#create a matrix with the data
dt_rangeSize <-
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all),
           c(pos_rangeSize_0_100k, neg_rangeSize_0_100k),
           c(pos_rangeSize_100_500k, neg_rangeSize_100_500k),
           c(pos_rangeSize_500k_1M, neg_rangeSize_500k_1M),
           c(pos_rangeSize_1_2M, neg_rangeSize_1_2M),
           c(pos_rangeSize_2_3M, neg_rangeSize_2_3M),
           c(pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)

#name the columns
colnames(dt_rangeSize) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_rangeSize_xx <- 
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all,xx,xx,xx,xx,xx,xx),
           c(xx,pos_rangeSize_0_100k, neg_rangeSize_0_100k, xx,xx,xx,xx,xx),
           c(xx,xx,pos_rangeSize_100_500k,neg_rangeSize_100_500k,xx,xx,xx,xx),
           c(xx,xx,xx,pos_rangeSize_500k_1M, neg_rangeSize_500k_1M,xx,xx,xx),
           c(xx,xx,xx,xx,pos_rangeSize_1_2M, neg_rangeSize_1_2M, xx,xx),
           c(xx,xx,xx,xx,xx,pos_rangeSize_2_3M, neg_rangeSize_2_3M,xx),
           c(xx,xx,xx,xx,xx,xx,pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize_xx) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_rangeSize_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Range Size')

text(x= bp[1], y = dt_rangeSize[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$meanTslope_RP > 0)))
text(x= bp[1], y = dt_rangeSize[2,1]/2 + dt_rangeSize[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$meanTslope_RP < 0)))

text(x= bp[2], y = dt_rangeSize[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$meanTslope_RP > 0)))
text(x= bp[2], y = dt_rangeSize[2,2]/2 + dt_rangeSize[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$meanTslope_RP < 0)))

text(x= bp[3], y= dt_rangeSize[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$meanTslope_RP > 0)))
text(x= bp[3], y= dt_rangeSize[2,3]/2 + dt_rangeSize[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$meanTslope_RP < 0)))

text(x= bp[4], y= dt_rangeSize[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$meanTslope_RP > 0)))
text(x= bp[4], y= dt_rangeSize[2,4]/2 + dt_rangeSize[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$meanTslope_RP < 0)))

text(x= bp[5], y= dt_rangeSize[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$meanTslope_RP > 0)))
text(x= bp[5], y= dt_rangeSize[2,5]/2 + dt_rangeSize[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$meanTslope_RP < 0)))

text(x= bp[6], y= dt_rangeSize[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$meanTslope_RP > 0)))
text(x= bp[6], y= dt_rangeSize[2,6]/2 + dt_rangeSize[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$meanTslope_RP < 0)))

text(x= bp[7], y= dt_rangeSize[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$meanTslope_RP > 0)))
text(x= bp[7], y= dt_rangeSize[2,7]/2 + dt_rangeSize[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$meanTslope_RP < 0)))

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by mean lat

#eliminate 0s and NAs in the slope
res_temp_meanLat <- res_temp[which(res_temp$meanTslope_RP != 0),]

#make a mean lat col
res_temp_meanLat$meanLat <- 
  (res_temp_meanLat$maxLat + res_temp_meanLat$minLat) / 2

#### all lats
res_temp_meanLat_all <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat > 0),]

pos_meanLat_all <- sum(res_temp_meanLat_all$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_all) * 100
neg_meanLat_all <- sum(res_temp_meanLat_all$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_meanLat_0_10 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat < 10),]

pos_meanLat_0_10 <- sum(res_temp_meanLat_0_10$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_0_10) * 100
neg_meanLat_0_10 <- sum(res_temp_meanLat_0_10$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_meanLat_10_20 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 10 &
          res_temp_meanLat$meanLat < 20),]

pos_meanLat_10_20 <- sum(res_temp_meanLat_10_20$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_10_20) * 100
neg_meanLat_10_20 <- sum(res_temp_meanLat_10_20$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_meanLat_20_30 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 20 &
          res_temp_meanLat$meanLat < 30),]

pos_meanLat_20_30 <- sum(res_temp_meanLat_20_30$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_20_30) * 100
neg_meanLat_20_30 <- sum(res_temp_meanLat_20_30$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_meanLat_30_40 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 30 &
          res_temp_meanLat$meanLat < 40),]

pos_meanLat_30_40 <- sum(res_temp_meanLat_30_40$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_30_40) * 100
neg_meanLat_30_40 <- sum(res_temp_meanLat_30_40$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_meanLat_40_50 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 40 &
          res_temp_meanLat$meanLat < 50),]

pos_meanLat_40_50 <- sum(res_temp_meanLat_40_50$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_40_50) * 100
neg_meanLat_40_50 <- sum(res_temp_meanLat_40_50$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_40_50) * 100

#### more than 50 degress
res_temp_meanLat_50 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 50),]

pos_meanLat_50 <- sum(res_temp_meanLat_50$meanTslope_RP > 0) /
  nrow(res_temp_meanLat_50) * 100
neg_meanLat_50 <- sum(res_temp_meanLat_50$meanTslope_RP < 0) /
  nrow(res_temp_meanLat_50) * 100

#create a matrix with the data
dt_meanLat <-
  matrix(c(c(pos_meanLat_all, neg_meanLat_all),
           c(pos_meanLat_0_10, neg_meanLat_0_10),
           c(pos_meanLat_10_20, neg_meanLat_10_20),
           c(pos_meanLat_20_30, neg_meanLat_20_30),
           c(pos_meanLat_30_40, neg_meanLat_30_40),
           c(pos_meanLat_40_50, neg_meanLat_40_50),
           c(pos_meanLat_50, neg_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_meanLat_xx <-
  matrix(c(c(pos_meanLat_all, neg_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_meanLat_0_10, neg_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_meanLat_10_20, neg_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_meanLat_20_30, neg_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_meanLat_30_40, neg_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_meanLat_40_50, neg_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_meanLat_50, neg_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_meanLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude')

text(x= bp[1], y = dt_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat$meanTslope_RP > 0)))
text(x= bp[1], y = dt_minLat[2,1]/2 + dt_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat$meanTslope_RP < 0)))

text(x= bp[2], y = dt_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_0_10$meanTslope_RP > 0)))
text(x= bp[2], y = dt_minLat[2,2]/2 + dt_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_0_10$meanTslope_RP < 0)))

text(x= bp[3], y= dt_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_10_20$meanTslope_RP > 0)))
text(x= bp[3], y= dt_minLat[2,3]/2 + dt_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_10_20$meanTslope_RP < 0)))

text(x= bp[4], y= dt_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_20_30$meanTslope_RP > 0)))
text(x= bp[4], y= dt_minLat[2,4]/2 + dt_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_20_30$meanTslope_RP < 0)))

text(x= bp[5], y= dt_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_30_40$meanTslope_RP > 0)))
text(x= bp[5], y= dt_minLat[2,5]/2 + dt_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_30_40$meanTslope_RP < 0)))

text(x= bp[6], y= dt_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_40_50$meanTslope_RP > 0)))
text(x= bp[6], y= dt_minLat[2,6]/2 + dt_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_40_50$meanTslope_RP < 0)))

text(x= bp[7], y= dt_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_50$meanTslope_RP > 0)))
text(x= bp[7], y= dt_minLat[2,7]/2 + dt_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_50$meanTslope_RP < 0)))


###### check relationships by mean lat per latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_latAmp_meanLat <- res_temp[which(res_temp$meanTslope_RP != 0),]

#make a mean lat col
res_temp_latAmp_meanLat$meanLat <- 
  (res_temp_latAmp_meanLat$maxLat + res_temp_latAmp_meanLat$minLat) / 2

##### latAmp < 5 degrees
res_temp_latAmp_0_5_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude < 5,]

#### all lats
res_temp_latAmp_0_5_meanLat_all <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat > 0),]

pos_latAmp_0_5_meanLat_all <- 
  sum(res_temp_latAmp_0_5_meanLat_all$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_all) * 100
neg_latAmp_0_5_meanLat_all <- 
  sum(res_temp_latAmp_0_5_meanLat_all$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_0_5_meanLat_0_10 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat < 10),]

pos_latAmp_0_5_meanLat_0_10 <-
  sum(res_temp_latAmp_0_5_meanLat_0_10$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_0_10) * 100
neg_latAmp_0_5_meanLat_0_10 <-
  sum(res_temp_latAmp_0_5_meanLat_0_10$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_0_5_meanLat_10_20 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 10 &
          res_temp_latAmp_0_5_meanLat$meanLat < 20),]

pos_latAmp_0_5_meanLat_10_20 <-
  sum(res_temp_latAmp_0_5_meanLat_10_20$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_10_20) * 100
neg_latAmp_0_5_meanLat_10_20 <-
  sum(res_temp_latAmp_0_5_meanLat_10_20$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_0_5_meanLat_20_30 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 20 &
          res_temp_latAmp_0_5_meanLat$meanLat < 30),]

pos_latAmp_0_5_meanLat_20_30 <-
  sum(res_temp_latAmp_0_5_meanLat_20_30$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_20_30) * 100
neg_latAmp_0_5_meanLat_20_30 <-
  sum(res_temp_latAmp_0_5_meanLat_20_30$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_0_5_meanLat_30_40 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 30 &
          res_temp_latAmp_0_5_meanLat$meanLat < 40),]

pos_latAmp_0_5_meanLat_30_40 <-
  sum(res_temp_latAmp_0_5_meanLat_30_40$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_30_40) * 100
neg_latAmp_0_5_meanLat_30_40 <-
  sum(res_temp_latAmp_0_5_meanLat_30_40$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_latAmp_0_5_meanLat_40_50 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 40 &
          res_temp_latAmp_0_5_meanLat$meanLat < 50),]

pos_latAmp_0_5_meanLat_40_50 <-
  sum(res_temp_latAmp_0_5_meanLat_40_50$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_40_50) * 100
neg_latAmp_0_5_meanLat_40_50 <-
  sum(res_temp_latAmp_0_5_meanLat_40_50$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_40_50) * 100

#### more than 50 degress
res_temp_latAmp_0_5_meanLat_50 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 50),]

pos_latAmp_0_5_meanLat_50 <-
  sum(res_temp_latAmp_0_5_meanLat_50$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_50) * 100
neg_latAmp_0_5_meanLat_50 <-
  sum(res_temp_latAmp_0_5_meanLat_50$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_50) * 100

#create a matrix with the data
dt_latAmp_0_5_minLat <-
  matrix(c(c(pos_latAmp_0_5_meanLat_all, neg_latAmp_0_5_meanLat_all),
           c(pos_latAmp_0_5_meanLat_0_10, neg_latAmp_0_5_meanLat_0_10),
           c(pos_latAmp_0_5_meanLat_10_20, neg_latAmp_0_5_meanLat_10_20),
           c(pos_latAmp_0_5_meanLat_20_30, neg_latAmp_0_5_meanLat_20_30),
           c(pos_latAmp_0_5_meanLat_30_40, neg_latAmp_0_5_meanLat_30_40),
           c(pos_latAmp_0_5_meanLat_40_50, neg_latAmp_0_5_meanLat_40_50),
           c(pos_latAmp_0_5_meanLat_50, neg_latAmp_0_5_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_0_5_minLat_xx <-
  matrix(c(c(pos_latAmp_0_5_meanLat_all,
             neg_latAmp_0_5_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_0_5_meanLat_0_10,
             neg_latAmp_0_5_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_0_5_meanLat_10_20,
             neg_latAmp_0_5_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_0_5_meanLat_20_30,
             neg_latAmp_0_5_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_0_5_meanLat_30_40,
             neg_latAmp_0_5_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_0_5_meanLat_40_50,
             neg_latAmp_0_5_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_latAmp_0_5_meanLat_50,
             neg_latAmp_0_5_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_0_5_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_0_5_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude (latAmp 0 - 5)')

text(x= bp[1], y = dt_latAmp_0_5_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_all$meanTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp_0_5_minLat[2,1]/2 + dt_latAmp_0_5_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_all$meanTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp_0_5_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_0_10$meanTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp_0_5_minLat[2,2]/2 + dt_latAmp_0_5_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_0_10$meanTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp_0_5_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_10_20$meanTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp_0_5_minLat[2,3]/2 + dt_latAmp_0_5_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_10_20$meanTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp_0_5_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_20_30$meanTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp_0_5_minLat[2,4]/2 + dt_latAmp_0_5_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_20_30$meanTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp_0_5_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_30_40$meanTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_0_5_minLat[2,5]/2 + dt_latAmp_0_5_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_30_40$meanTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_0_5_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_40_50$meanTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_0_5_minLat[2,6]/2 + dt_latAmp_0_5_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_40_50$meanTslope_RP < 0)))

text(x= bp[7], y= dt_latAmp_0_5_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_50$meanTslope_RP > 0)))
text(x= bp[7], y= dt_latAmp_0_5_minLat[2,7]/2 + dt_latAmp_0_5_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_50$meanTslope_RP < 0)))


#####. latAmp 5 to 10 degrees
res_temp_latAmp_5_10_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 5 &
                            res_temp_latAmp_meanLat$latAmplitude < 10,]

#### all lats
res_temp_latAmp_5_10_meanLat_all <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat > 0),]

pos_latAmp_5_10_meanLat_all <- 
  sum(res_temp_latAmp_5_10_meanLat_all$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_all) * 100
neg_latAmp_5_10_meanLat_all <- 
  sum(res_temp_latAmp_5_10_meanLat_all$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_5_10_meanLat_0_10 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat < 10),]

pos_latAmp_5_10_meanLat_0_10 <-
  sum(res_temp_latAmp_5_10_meanLat_0_10$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_0_10) * 100
neg_latAmp_5_10_meanLat_0_10 <-
  sum(res_temp_latAmp_5_10_meanLat_0_10$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_5_10_meanLat_10_20 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 10 &
          res_temp_latAmp_5_10_meanLat$meanLat < 20),]

pos_latAmp_5_10_meanLat_10_20 <-
  sum(res_temp_latAmp_5_10_meanLat_10_20$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_10_20) * 100
neg_latAmp_5_10_meanLat_10_20 <-
  sum(res_temp_latAmp_5_10_meanLat_10_20$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_5_10_meanLat_20_30 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 20 &
          res_temp_latAmp_5_10_meanLat$meanLat < 30),]

pos_latAmp_5_10_meanLat_20_30 <-
  sum(res_temp_latAmp_5_10_meanLat_20_30$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_20_30) * 100
neg_latAmp_5_10_meanLat_20_30 <-
  sum(res_temp_latAmp_5_10_meanLat_20_30$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_5_10_meanLat_30_40 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 30 &
          res_temp_latAmp_5_10_meanLat$meanLat < 40),]

pos_latAmp_5_10_meanLat_30_40 <-
  sum(res_temp_latAmp_5_10_meanLat_30_40$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_30_40) * 100
neg_latAmp_5_10_meanLat_30_40 <-
  sum(res_temp_latAmp_5_10_meanLat_30_40$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_latAmp_5_10_meanLat_40_50 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 40 &
          res_temp_latAmp_5_10_meanLat$meanLat < 50),]

pos_latAmp_5_10_meanLat_40_50 <-
  sum(res_temp_latAmp_5_10_meanLat_40_50$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_40_50) * 100
neg_latAmp_5_10_meanLat_40_50 <-
  sum(res_temp_latAmp_5_10_meanLat_40_50$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_40_50) * 100

#### more than 50 degress
res_temp_latAmp_5_10_meanLat_50 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 50),]

pos_latAmp_5_10_meanLat_50 <-
  sum(res_temp_latAmp_5_10_meanLat_50$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_50) * 100
neg_latAmp_5_10_meanLat_50 <-
  sum(res_temp_latAmp_5_10_meanLat_50$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_50) * 100

#create a matrix with the data
dt_latAmp_5_10_minLat <-
  matrix(c(c(pos_latAmp_5_10_meanLat_all, neg_latAmp_5_10_meanLat_all),
           c(pos_latAmp_5_10_meanLat_0_10, neg_latAmp_5_10_meanLat_0_10),
           c(pos_latAmp_5_10_meanLat_10_20, neg_latAmp_5_10_meanLat_10_20),
           c(pos_latAmp_5_10_meanLat_20_30, neg_latAmp_5_10_meanLat_20_30),
           c(pos_latAmp_5_10_meanLat_30_40, neg_latAmp_5_10_meanLat_30_40),
           c(pos_latAmp_5_10_meanLat_40_50, neg_latAmp_5_10_meanLat_40_50),
           c(pos_latAmp_5_10_meanLat_50, neg_latAmp_5_10_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_5_10_minLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_5_10_minLat_xx <-
  matrix(c(c(pos_latAmp_5_10_meanLat_all,
             neg_latAmp_5_10_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_5_10_meanLat_0_10,
             neg_latAmp_5_10_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_5_10_meanLat_10_20,
             neg_latAmp_5_10_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_5_10_meanLat_20_30,
             neg_latAmp_5_10_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_5_10_meanLat_30_40,
             neg_latAmp_5_10_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_5_10_meanLat_40_50,
             neg_latAmp_5_10_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_latAmp_5_10_meanLat_50,
             neg_latAmp_5_10_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_5_10_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_5_10_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude (latAmp 5 - 10)')


text(x= bp[1], y = dt_latAmp_5_10_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_all$meanTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp_5_10_minLat[2,1]/2 + dt_latAmp_5_10_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_all$meanTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp_5_10_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_0_10$meanTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp_5_10_minLat[2,2]/2 + dt_latAmp_5_10_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_0_10$meanTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp_5_10_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_10_20$meanTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp_5_10_minLat[2,3]/2 + dt_latAmp_5_10_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_10_20$meanTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp_5_10_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_20_30$meanTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp_5_10_minLat[2,4]/2 + dt_latAmp_5_10_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_20_30$meanTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp_5_10_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_30_40$meanTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_5_10_minLat[2,5]/2 + dt_latAmp_5_10_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_30_40$meanTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_5_10_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_40_50$meanTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_5_10_minLat[2,6]/2 + dt_latAmp_5_10_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_5_10_meanLat_40_50$meanTslope_RP < 0)))

text(x= bp[7], y= dt_latAmp_5_10_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_50$meanTslope_RP > 0)))
# text(x= bp[7], y= dt_latAmp_5_10_minLat[2,7]/2 + dt_latAmp_5_10_minLat[1,7],
#      pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
#      labels = paste(sum(res_temp_latAmp_5_10_meanLat_50$meanTslope_RP < 0)))



#####. latAmp 10 to 15 degrees
res_temp_latAmp_10_15_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 10 &
                            res_temp_latAmp_meanLat$latAmplitude < 15,]

#### all lats
res_temp_latAmp_10_15_meanLat_all <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat > 0),]

pos_latAmp_10_15_meanLat_all <- 
  sum(res_temp_latAmp_10_15_meanLat_all$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_all) * 100
neg_latAmp_10_15_meanLat_all <- 
  sum(res_temp_latAmp_10_15_meanLat_all$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_10_15_meanLat_0_10 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat < 10),]

pos_latAmp_10_15_meanLat_0_10 <-
  sum(res_temp_latAmp_10_15_meanLat_0_10$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_0_10) * 100
neg_latAmp_10_15_meanLat_0_10 <-
  sum(res_temp_latAmp_10_15_meanLat_0_10$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_10_15_meanLat_10_20 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 10 &
          res_temp_latAmp_10_15_meanLat$meanLat < 20),]

pos_latAmp_10_15_meanLat_10_20 <-
  sum(res_temp_latAmp_10_15_meanLat_10_20$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_10_20) * 100
neg_latAmp_10_15_meanLat_10_20 <-
  sum(res_temp_latAmp_10_15_meanLat_10_20$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_10_15_meanLat_20_30 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 20 &
          res_temp_latAmp_10_15_meanLat$meanLat < 30),]

pos_latAmp_10_15_meanLat_20_30 <-
  sum(res_temp_latAmp_10_15_meanLat_20_30$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_20_30) * 100
neg_latAmp_10_15_meanLat_20_30 <-
  sum(res_temp_latAmp_10_15_meanLat_20_30$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_10_15_meanLat_30_40 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 30 &
          res_temp_latAmp_10_15_meanLat$meanLat < 40),]

pos_latAmp_10_15_meanLat_30_40 <-
  sum(res_temp_latAmp_10_15_meanLat_30_40$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_30_40) * 100
neg_latAmp_10_15_meanLat_30_40 <-
  sum(res_temp_latAmp_10_15_meanLat_30_40$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_30_40) * 100

#### more than 40 degress
res_temp_latAmp_10_15_meanLat_40 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 40),]

pos_latAmp_10_15_meanLat_40 <-
  sum(res_temp_latAmp_10_15_meanLat_40$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_40) * 100
neg_latAmp_10_15_meanLat_40 <-
  sum(res_temp_latAmp_10_15_meanLat_40$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_40) * 100

#create a matrix with the data
dt_latAmp_10_15_minLat <-
  matrix(c(c(pos_latAmp_10_15_meanLat_all, neg_latAmp_10_15_meanLat_all),
           c(pos_latAmp_10_15_meanLat_0_10, neg_latAmp_10_15_meanLat_0_10),
           c(pos_latAmp_10_15_meanLat_10_20, neg_latAmp_10_15_meanLat_10_20),
           c(pos_latAmp_10_15_meanLat_20_30, neg_latAmp_10_15_meanLat_20_30),
           c(pos_latAmp_10_15_meanLat_30_40, neg_latAmp_10_15_meanLat_30_40),
           c(pos_latAmp_10_15_meanLat_40, neg_latAmp_10_15_meanLat_40)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_10_15_minLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_10_15_minLat_xx <-
  matrix(c(c(pos_latAmp_10_15_meanLat_all,
             neg_latAmp_10_15_meanLat_all, xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_10_15_meanLat_0_10,
             neg_latAmp_10_15_meanLat_0_10, xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_10_15_meanLat_10_20,
             neg_latAmp_10_15_meanLat_10_20, xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_10_15_meanLat_20_30,
             neg_latAmp_10_15_meanLat_20_30, xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_10_15_meanLat_30_40,
             neg_latAmp_10_15_meanLat_30_40, xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_10_15_meanLat_40,
             neg_latAmp_10_15_meanLat_40)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_10_15_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_10_15_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude (latAmp 10 - 15)')


text(x= bp[1], 
     y = dt_latAmp_10_15_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_all$meanTslope_RP > 0)))
text(x= bp[1], 
     y = dt_latAmp_10_15_minLat[2,1]/2 + dt_latAmp_10_15_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_all$meanTslope_RP < 0)))

text(x= bp[2], 
     y = dt_latAmp_10_15_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_0_10$meanTslope_RP > 0)))
text(x= bp[2],
     y = dt_latAmp_10_15_minLat[2,2]/2 + dt_latAmp_10_15_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_0_10$meanTslope_RP < 0)))

text(x= bp[3],
     y= dt_latAmp_10_15_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = 
       paste(sum(res_temp_latAmp_10_15_meanLat_10_20$meanTslope_RP > 0)))
text(x= bp[3],
     y= dt_latAmp_10_15_minLat[2,3]/2 + dt_latAmp_10_15_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_10_20$meanTslope_RP < 0)))

text(x= bp[4],
     y= dt_latAmp_10_15_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_20_30$meanTslope_RP > 0)))
text(x= bp[4],
     y= dt_latAmp_10_15_minLat[2,4]/2 + dt_latAmp_10_15_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_20_30$meanTslope_RP < 0)))

text(x= bp[5],
     y= dt_latAmp_10_15_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_30_40$meanTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_10_15_minLat[2,5]/2 + dt_latAmp_10_15_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_30_40$meanTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_10_15_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_40$meanTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_10_15_minLat[2,6]/2 + dt_latAmp_10_15_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_40$meanTslope_RP < 0)))



#####. latAmp more than 15 degrees
res_temp_latAmp_15_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 15,]

#### all lats
res_temp_latAmp_15_meanLat_all <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat > 0),]

pos_latAmp_15_meanLat_all <- 
  sum(res_temp_latAmp_15_meanLat_all$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_all) * 100
neg_latAmp_15_meanLat_all <- 
  sum(res_temp_latAmp_15_meanLat_all$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_all) * 100

#### 0 to 30 degrees
res_temp_latAmp_15_meanLat_0_30 <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat < 30),]

pos_latAmp_15_meanLat_0_30 <-
  sum(res_temp_latAmp_15_meanLat_0_30$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_0_30) * 100
neg_latAmp_15_meanLat_0_30 <-
  sum(res_temp_latAmp_15_meanLat_0_30$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_0_30) * 100

#### more than 30 degrees
res_temp_latAmp_15_meanLat_30 <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat >= 30 &
          res_temp_latAmp_15_meanLat$meanLat < 90),]

pos_latAmp_15_meanLat_30 <-
  sum(res_temp_latAmp_15_meanLat_30$meanTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_30) * 100
neg_latAmp_15_meanLat_30 <-
  sum(res_temp_latAmp_15_meanLat_30$meanTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_30) * 100

#create a matrix with the data
dt_latAmp_15_minLat <-
  matrix(c(c(pos_latAmp_15_meanLat_all, neg_latAmp_15_meanLat_all),
           c(pos_latAmp_15_meanLat_0_30, neg_latAmp_15_meanLat_0_30),
           c(pos_latAmp_15_meanLat_30, neg_latAmp_15_meanLat_30)),
         ncol = 3)

#name the columns
colnames(dt_latAmp_15_minLat) <-
  c('all', 'Lat_0_30', 'Lat_30')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_15_minLat_xx <-
  matrix(c(c(pos_latAmp_15_meanLat_all,
             neg_latAmp_15_meanLat_all, xx,xx),
           c(xx, pos_latAmp_15_meanLat_0_30,
             neg_latAmp_15_meanLat_0_30, xx),
           c(xx,xx, pos_latAmp_15_meanLat_30,
             neg_latAmp_15_meanLat_30)),
         ncol = 3)

#name the columns
colnames(dt_latAmp_15_minLat_xx) <- c('all', 'Lat_0_30', 'Lat_30')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_15_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude (latAmp > 15)')


text(x= bp[1], 
     y = dt_latAmp_15_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_all$meanTslope_RP > 0)))
text(x= bp[1], 
     y = dt_latAmp_15_minLat[2,1]/2 + dt_latAmp_15_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_all$meanTslope_RP < 0)))

text(x= bp[2], 
     y = dt_latAmp_15_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_0_30$meanTslope_RP > 0)))
text(x= bp[2],
     y = dt_latAmp_15_minLat[2,2]/2 + dt_latAmp_15_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_0_30$meanTslope_RP < 0)))

text(x= bp[3],
     y= dt_latAmp_15_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_30$meanTslope_RP > 0)))
text(x= bp[3],
     y= dt_latAmp_15_minLat[2,3]/2 + dt_latAmp_15_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_30$meanTslope_RP < 0)))








############################
######### MAX TEMP #########
############################

###### check relationships by latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_RP != 0),]

#### all amplitudes
res_temp_large_ranges_all <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 0),]

pos_latAmp_all <- sum(res_temp_large_ranges_all$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges_all) * 100
neg_latAmp_all <- sum(res_temp_large_ranges_all$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges_all) * 100

#### 0 to 5 degrees
res_temp_large_ranges_0_5 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 0 &
          res_temp_large_ranges$latAmplitude < 5),]

pos_latAmp_0_5 <- sum(res_temp_large_ranges_0_5$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges_0_5) * 100
neg_latAmp_0_5 <- sum(res_temp_large_ranges_0_5$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges_0_5) * 100

#### 5 to 10 degrees
res_temp_large_ranges_5_10 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 5 &
          res_temp_large_ranges$latAmplitude < 10),]

pos_latAmp_5_10 <- sum(res_temp_large_ranges_5_10$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges_5_10) * 100
neg_latAmp_5_10 <- sum(res_temp_large_ranges_5_10$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges_5_10) * 100

#### 10 to 15 degrees
res_temp_large_ranges_10_15 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 10 &
          res_temp_large_ranges$latAmplitude < 15),]

pos_latAmp_10_15 <- sum(res_temp_large_ranges_10_15$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges_10_15) * 100
neg_latAmp_10_15 <- sum(res_temp_large_ranges_10_15$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges_10_15) * 100

#### 15 to 20 degrees
res_temp_large_ranges_15_20 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 15 &
          res_temp_large_ranges$latAmplitude < 20),]

pos_latAmp_15_20 <- sum(res_temp_large_ranges_15_20$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges_15_20) * 100
neg_latAmp_15_20 <- sum(res_temp_large_ranges_15_20$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges_15_20) * 100

#### more than 20 degrees
res_temp_large_ranges_20 <- res_temp_large_ranges[
  which(res_temp_large_ranges$latAmplitude >= 20),]

pos_latAmp_20 <- sum(res_temp_large_ranges_20$maxTslope_RP > 0) /
  nrow(res_temp_large_ranges_20) * 100
neg_latAmp_20 <- sum(res_temp_large_ranges_20$maxTslope_RP < 0) /
  nrow(res_temp_large_ranges_20) * 100

#create a matrix with the data
dt_latAmp <- matrix(c(c(pos_latAmp_all, neg_latAmp_all),
                      c(pos_latAmp_0_5, neg_latAmp_0_5),
                      c(pos_latAmp_5_10, neg_latAmp_5_10),
                      c(pos_latAmp_10_15, neg_latAmp_10_15),
                      c(pos_latAmp_15_20, neg_latAmp_15_20),
                      c(pos_latAmp_20, neg_latAmp_20)),
                    ncol = 6)

#name the columns
colnames(dt_latAmp) <-
  c('all', 'LA_0_5', 'LA_5_10', 'LA_10_15', 'LA_15_20', 'LA_20')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_xx <- 
  matrix(c(c(pos_latAmp_all, neg_latAmp_all, xx, xx, xx, xx, xx),
           c(xx, pos_latAmp_0_5, neg_latAmp_0_5, xx, xx, xx, xx),
           c(xx, xx, pos_latAmp_5_10, neg_latAmp_5_10, xx, xx, xx),
           c(xx, xx, xx, pos_latAmp_10_15, neg_latAmp_10_15, xx, xx),
           c(xx, xx, xx, xx, pos_latAmp_15_20, neg_latAmp_15_20, xx),
           c(xx, xx, xx, xx, xx, pos_latAmp_20, neg_latAmp_20)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_xx) <-
  c('all', 'LA_0_5', 'LA_5_10', 'LA_10_15', 'LA_15_20', 'LA_20')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Max T Lat Amplitude')

text(x= bp[1], y = dt_latAmp[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_all$maxTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp[2,1]/2 + dt_latAmp[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_all$maxTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_0_5$maxTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp[2,2]/2 + dt_latAmp[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_0_5$maxTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_5_10$maxTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp[2,3]/2 + dt_latAmp[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_5_10$maxTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_10_15$maxTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp[2,4]/2 + dt_latAmp[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_10_15$maxTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_15_20$maxTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp[2,5]/2 + dt_latAmp[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_15_20$maxTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_20$maxTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp[2,6]/2 + dt_latAmp[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_large_ranges_20$maxTslope_RP < 0)))

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_rangeSize <- res_temp[which(res_temp$maxTslope_RP != 0),]

#### all amplitudes
res_temp_rangeSize_all <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_rangeSize_all$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_all) * 100
neg_rangeSize_all <- sum(res_temp_rangeSize_all$maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_all) * 100

#### less than 100000 km2
res_temp_rangeSize_0_100k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize < 100000),]

pos_rangeSize_0_100k <- sum(res_temp_rangeSize_0_100k$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_0_100k) * 100
neg_rangeSize_0_100k <- sum(res_temp_rangeSize_0_100k$maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_0_100k) * 100

#### 100000 t0 500000 km2
res_temp_rangeSize_100_500k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 100000 &
          res_temp_rangeSize$rangeSize < 500000),]

pos_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_100_500k ) * 100
neg_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_100_500k ) * 100

#### 500000 to 1000000 km2
res_temp_rangeSize_500k_1M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 500000 &
          res_temp_rangeSize$rangeSize < 1000000),]

pos_rangeSize_500k_1M <- 
  sum(res_temp_rangeSize_500k_1M$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100
neg_rangeSize_500k_1M <-
  sum(res_temp_rangeSize_500k_1M $maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100

#### 1000000 to 2000000 km2
res_temp_rangeSize_1_2M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 1000000 &
          res_temp_rangeSize$rangeSize < 2000000),]

pos_rangeSize_1_2M <- 
  sum(res_temp_rangeSize_1_2M$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100
neg_rangeSize_1_2M <-
  sum(res_temp_rangeSize_1_2M $maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100

#### 2000000 to 3000000 km2
res_temp_rangeSize_2_3M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 2000000 &
          res_temp_rangeSize$rangeSize < 3000000),]

pos_rangeSize_2_3M <- 
  sum(res_temp_rangeSize_2_3M$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100
neg_rangeSize_2_3M <-
  sum(res_temp_rangeSize_2_3M $maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100

#### more than 3000000 km2
res_temp_rangeSize_3M  <- res_temp_large_ranges[
  which(res_temp_rangeSize$rangeSize  >= 3000000),]

pos_rangeSize_3M <- 
  sum(res_temp_rangeSize_3M$maxTslope_RP > 0) /
  nrow(res_temp_rangeSize_3M) * 100
neg_rangeSize_3M <-
  sum(res_temp_rangeSize_3M $maxTslope_RP < 0) /
  nrow(res_temp_rangeSize_3M ) * 100

#create a matrix with the data
dt_rangeSize <-
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all),
           c(pos_rangeSize_0_100k, neg_rangeSize_0_100k),
           c(pos_rangeSize_100_500k, neg_rangeSize_100_500k),
           c(pos_rangeSize_500k_1M, neg_rangeSize_500k_1M),
           c(pos_rangeSize_1_2M, neg_rangeSize_1_2M),
           c(pos_rangeSize_2_3M, neg_rangeSize_2_3M),
           c(pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)

#name the columns
colnames(dt_rangeSize) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_rangeSize_xx <- 
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all,xx,xx,xx,xx,xx,xx),
           c(xx,pos_rangeSize_0_100k, neg_rangeSize_0_100k, xx,xx,xx,xx,xx),
           c(xx,xx,pos_rangeSize_100_500k,neg_rangeSize_100_500k,xx,xx,xx,xx),
           c(xx,xx,xx,pos_rangeSize_500k_1M, neg_rangeSize_500k_1M,xx,xx,xx),
           c(xx,xx,xx,xx,pos_rangeSize_1_2M, neg_rangeSize_1_2M, xx,xx),
           c(xx,xx,xx,xx,xx,pos_rangeSize_2_3M, neg_rangeSize_2_3M,xx),
           c(xx,xx,xx,xx,xx,xx,pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize_xx) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_rangeSize_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Max T Range Size')

text(x= bp[1], y = dt_rangeSize[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$maxTslope_RP > 0)))
text(x= bp[1], y = dt_rangeSize[2,1]/2 + dt_rangeSize[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$maxTslope_RP < 0)))

text(x= bp[2], y = dt_rangeSize[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$maxTslope_RP > 0)))
text(x= bp[2], y = dt_rangeSize[2,2]/2 + dt_rangeSize[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$maxTslope_RP < 0)))

text(x= bp[3], y= dt_rangeSize[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$maxTslope_RP > 0)))
text(x= bp[3], y= dt_rangeSize[2,3]/2 + dt_rangeSize[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$maxTslope_RP < 0)))

text(x= bp[4], y= dt_rangeSize[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$maxTslope_RP > 0)))
text(x= bp[4], y= dt_rangeSize[2,4]/2 + dt_rangeSize[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$maxTslope_RP < 0)))

text(x= bp[5], y= dt_rangeSize[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$maxTslope_RP > 0)))
text(x= bp[5], y= dt_rangeSize[2,5]/2 + dt_rangeSize[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$maxTslope_RP < 0)))

text(x= bp[6], y= dt_rangeSize[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$maxTslope_RP > 0)))
text(x= bp[6], y= dt_rangeSize[2,6]/2 + dt_rangeSize[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$maxTslope_RP < 0)))

text(x= bp[7], y= dt_rangeSize[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$maxTslope_RP > 0)))
# text(x= bp[7], y= dt_rangeSize[2,7]/2 + dt_rangeSize[1,7],
#      pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
#      labels = paste(sum(res_temp_rangeSize_3M$maxTslope_RP < 0)))

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by mean lat

#eliminate 0s and NAs in the slope
res_temp_meanLat <- res_temp[which(res_temp$minTslope_RP != 0),]

#make a mean lat col
res_temp_meanLat$meanLat <- 
  (res_temp_meanLat$maxLat + res_temp_meanLat$minLat) / 2

#### all lats
res_temp_meanLat_all <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat > 0),]

pos_meanLat_all <- sum(res_temp_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_meanLat_all) * 100
neg_meanLat_all <- sum(res_temp_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_meanLat_0_10 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat < 10),]

pos_meanLat_0_10 <- sum(res_temp_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_meanLat_0_10) * 100
neg_meanLat_0_10 <- sum(res_temp_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_meanLat_10_20 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 10 &
          res_temp_meanLat$meanLat < 20),]

pos_meanLat_10_20 <- sum(res_temp_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_meanLat_10_20) * 100
neg_meanLat_10_20 <- sum(res_temp_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_meanLat_20_30 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 20 &
          res_temp_meanLat$meanLat < 30),]

pos_meanLat_20_30 <- sum(res_temp_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_meanLat_20_30) * 100
neg_meanLat_20_30 <- sum(res_temp_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_meanLat_30_40 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 30 &
          res_temp_meanLat$meanLat < 40),]

pos_meanLat_30_40 <- sum(res_temp_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_meanLat_30_40) * 100
neg_meanLat_30_40 <- sum(res_temp_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_meanLat_40_50 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 40 &
          res_temp_meanLat$meanLat < 50),]

pos_meanLat_40_50 <- sum(res_temp_meanLat_40_50$minTslope_RP > 0) /
  nrow(res_temp_meanLat_40_50) * 100
neg_meanLat_40_50 <- sum(res_temp_meanLat_40_50$minTslope_RP < 0) /
  nrow(res_temp_meanLat_40_50) * 100

#### more than 50 degress
res_temp_meanLat_50 <- res_temp_meanLat[
  which(res_temp_meanLat$meanLat >= 50),]

pos_meanLat_50 <- sum(res_temp_meanLat_50$minTslope_RP > 0) /
  nrow(res_temp_meanLat_50) * 100
neg_meanLat_50 <- sum(res_temp_meanLat_50$minTslope_RP < 0) /
  nrow(res_temp_meanLat_50) * 100

#create a matrix with the data
dt_meanLat <-
  matrix(c(c(pos_meanLat_all, neg_meanLat_all),
           c(pos_meanLat_0_10, neg_meanLat_0_10),
           c(pos_meanLat_10_20, neg_meanLat_10_20),
           c(pos_meanLat_20_30, neg_meanLat_20_30),
           c(pos_meanLat_30_40, neg_meanLat_30_40),
           c(pos_meanLat_40_50, neg_meanLat_40_50),
           c(pos_meanLat_50, neg_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_meanLat_xx <-
  matrix(c(c(pos_meanLat_all, neg_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_meanLat_0_10, neg_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_meanLat_10_20, neg_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_meanLat_20_30, neg_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_meanLat_30_40, neg_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_meanLat_40_50, neg_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_meanLat_50, neg_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_meanLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude')

text(x= bp[1], y = dt_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat$minTslope_RP > 0)))
text(x= bp[1], y = dt_minLat[2,1]/2 + dt_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat$minTslope_RP < 0)))

text(x= bp[2], y = dt_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2], y = dt_minLat[2,2]/2 + dt_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3], y= dt_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3], y= dt_minLat[2,3]/2 + dt_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4], y= dt_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4], y= dt_minLat[2,4]/2 + dt_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5], y= dt_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_minLat[2,5]/2 + dt_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_40_50$minTslope_RP > 0)))
text(x= bp[6], y= dt_minLat[2,6]/2 + dt_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_40_50$minTslope_RP < 0)))

text(x= bp[7], y= dt_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_50$minTslope_RP > 0)))
text(x= bp[7], y= dt_minLat[2,7]/2 + dt_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_meanLat_50$minTslope_RP < 0)))


###### check relationships by mean lat per latitudinal amplitude

#eliminate 0s and NAs in the slope
res_temp_latAmp_meanLat <- res_temp[which(res_temp$minTslope_RP != 0),]

#make a mean lat col
res_temp_latAmp_meanLat$meanLat <- 
  (res_temp_latAmp_meanLat$maxLat + res_temp_latAmp_meanLat$minLat) / 2

##### latAmp < 5 degrees
res_temp_latAmp_0_5_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude < 5,]

#### all lats
res_temp_latAmp_0_5_meanLat_all <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat > 0),]

pos_latAmp_0_5_meanLat_all <- 
  sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_all) * 100
neg_latAmp_0_5_meanLat_all <- 
  sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_0_5_meanLat_0_10 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat < 10),]

pos_latAmp_0_5_meanLat_0_10 <-
  sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_0_10) * 100
neg_latAmp_0_5_meanLat_0_10 <-
  sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_0_5_meanLat_10_20 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 10 &
          res_temp_latAmp_0_5_meanLat$meanLat < 20),]

pos_latAmp_0_5_meanLat_10_20 <-
  sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_10_20) * 100
neg_latAmp_0_5_meanLat_10_20 <-
  sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_0_5_meanLat_20_30 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 20 &
          res_temp_latAmp_0_5_meanLat$meanLat < 30),]

pos_latAmp_0_5_meanLat_20_30 <-
  sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_20_30) * 100
neg_latAmp_0_5_meanLat_20_30 <-
  sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_0_5_meanLat_30_40 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 30 &
          res_temp_latAmp_0_5_meanLat$meanLat < 40),]

pos_latAmp_0_5_meanLat_30_40 <-
  sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_30_40) * 100
neg_latAmp_0_5_meanLat_30_40 <-
  sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_latAmp_0_5_meanLat_40_50 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 40 &
          res_temp_latAmp_0_5_meanLat$meanLat < 50),]

pos_latAmp_0_5_meanLat_40_50 <-
  sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_40_50) * 100
neg_latAmp_0_5_meanLat_40_50 <-
  sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_40_50) * 100

#### more than 50 degress
res_temp_latAmp_0_5_meanLat_50 <- res_temp_latAmp_0_5_meanLat[
  which(res_temp_latAmp_0_5_meanLat$meanLat >= 50),]

pos_latAmp_0_5_meanLat_50 <-
  sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_0_5_meanLat_50) * 100
neg_latAmp_0_5_meanLat_50 <-
  sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_0_5_meanLat_50) * 100

#create a matrix with the data
dt_latAmp_0_5_minLat <-
  matrix(c(c(pos_latAmp_0_5_meanLat_all, neg_latAmp_0_5_meanLat_all),
           c(pos_latAmp_0_5_meanLat_0_10, neg_latAmp_0_5_meanLat_0_10),
           c(pos_latAmp_0_5_meanLat_10_20, neg_latAmp_0_5_meanLat_10_20),
           c(pos_latAmp_0_5_meanLat_20_30, neg_latAmp_0_5_meanLat_20_30),
           c(pos_latAmp_0_5_meanLat_30_40, neg_latAmp_0_5_meanLat_30_40),
           c(pos_latAmp_0_5_meanLat_40_50, neg_latAmp_0_5_meanLat_40_50),
           c(pos_latAmp_0_5_meanLat_50, neg_latAmp_0_5_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_meanLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_0_5_minLat_xx <-
  matrix(c(c(pos_latAmp_0_5_meanLat_all,
             neg_latAmp_0_5_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_0_5_meanLat_0_10,
             neg_latAmp_0_5_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_0_5_meanLat_10_20,
             neg_latAmp_0_5_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_0_5_meanLat_20_30,
             neg_latAmp_0_5_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_0_5_meanLat_30_40,
             neg_latAmp_0_5_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_0_5_meanLat_40_50,
             neg_latAmp_0_5_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_latAmp_0_5_meanLat_50,
             neg_latAmp_0_5_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_0_5_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_0_5_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude (latAmp 0 - 5)')

text(x= bp[1], y = dt_latAmp_0_5_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp_0_5_minLat[2,1]/2 + dt_latAmp_0_5_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp_0_5_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp_0_5_minLat[2,2]/2 + dt_latAmp_0_5_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp_0_5_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp_0_5_minLat[2,3]/2 + dt_latAmp_0_5_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp_0_5_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp_0_5_minLat[2,4]/2 + dt_latAmp_0_5_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp_0_5_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_0_5_minLat[2,5]/2 + dt_latAmp_0_5_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_0_5_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_0_5_minLat[2,6]/2 + dt_latAmp_0_5_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_40_50$minTslope_RP < 0)))

text(x= bp[7], y= dt_latAmp_0_5_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP > 0)))
text(x= bp[7], y= dt_latAmp_0_5_minLat[2,7]/2 + dt_latAmp_0_5_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_0_5_meanLat_50$minTslope_RP < 0)))


#####. latAmp 5 to 10 degrees
res_temp_latAmp_5_10_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 5 &
                            res_temp_latAmp_meanLat$latAmplitude < 10,]

#### all lats
res_temp_latAmp_5_10_meanLat_all <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat > 0),]

pos_latAmp_5_10_meanLat_all <- 
  sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_all) * 100
neg_latAmp_5_10_meanLat_all <- 
  sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_5_10_meanLat_0_10 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat < 10),]

pos_latAmp_5_10_meanLat_0_10 <-
  sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_0_10) * 100
neg_latAmp_5_10_meanLat_0_10 <-
  sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_5_10_meanLat_10_20 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 10 &
          res_temp_latAmp_5_10_meanLat$meanLat < 20),]

pos_latAmp_5_10_meanLat_10_20 <-
  sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_10_20) * 100
neg_latAmp_5_10_meanLat_10_20 <-
  sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_5_10_meanLat_20_30 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 20 &
          res_temp_latAmp_5_10_meanLat$meanLat < 30),]

pos_latAmp_5_10_meanLat_20_30 <-
  sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_20_30) * 100
neg_latAmp_5_10_meanLat_20_30 <-
  sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_5_10_meanLat_30_40 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 30 &
          res_temp_latAmp_5_10_meanLat$meanLat < 40),]

pos_latAmp_5_10_meanLat_30_40 <-
  sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_30_40) * 100
neg_latAmp_5_10_meanLat_30_40 <-
  sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_30_40) * 100

#### 40 to 50 degrees
res_temp_latAmp_5_10_meanLat_40_50 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 40 &
          res_temp_latAmp_5_10_meanLat$meanLat < 50),]

pos_latAmp_5_10_meanLat_40_50 <-
  sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_40_50) * 100
neg_latAmp_5_10_meanLat_40_50 <-
  sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_40_50) * 100

#### more than 50 degress
res_temp_latAmp_5_10_meanLat_50 <- res_temp_latAmp_5_10_meanLat[
  which(res_temp_latAmp_5_10_meanLat$meanLat >= 50),]

pos_latAmp_5_10_meanLat_50 <-
  sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP > 0) /
  nrow(res_temp_latAmp_5_10_meanLat_50) * 100
neg_latAmp_5_10_meanLat_50 <-
  sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP < 0) /
  nrow(res_temp_latAmp_5_10_meanLat_50) * 100

#create a matrix with the data
dt_latAmp_5_10_minLat <-
  matrix(c(c(pos_latAmp_5_10_meanLat_all, neg_latAmp_5_10_meanLat_all),
           c(pos_latAmp_5_10_meanLat_0_10, neg_latAmp_5_10_meanLat_0_10),
           c(pos_latAmp_5_10_meanLat_10_20, neg_latAmp_5_10_meanLat_10_20),
           c(pos_latAmp_5_10_meanLat_20_30, neg_latAmp_5_10_meanLat_20_30),
           c(pos_latAmp_5_10_meanLat_30_40, neg_latAmp_5_10_meanLat_30_40),
           c(pos_latAmp_5_10_meanLat_40_50, neg_latAmp_5_10_meanLat_40_50),
           c(pos_latAmp_5_10_meanLat_50, neg_latAmp_5_10_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_5_10_minLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_5_10_minLat_xx <-
  matrix(c(c(pos_latAmp_5_10_meanLat_all,
             neg_latAmp_5_10_meanLat_all, xx,xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_5_10_meanLat_0_10,
             neg_latAmp_5_10_meanLat_0_10, xx,xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_5_10_meanLat_10_20,
             neg_latAmp_5_10_meanLat_10_20, xx,xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_5_10_meanLat_20_30,
             neg_latAmp_5_10_meanLat_20_30, xx,xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_5_10_meanLat_30_40,
             neg_latAmp_5_10_meanLat_30_40, xx,xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_5_10_meanLat_40_50,
             neg_latAmp_5_10_meanLat_40_50, xx),
           c(xx,xx,xx,xx,xx,xx, pos_latAmp_5_10_meanLat_50,
             neg_latAmp_5_10_meanLat_50)),
         ncol = 7)

#name the columns
colnames(dt_latAmp_5_10_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40_50', 'Lat_50')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_5_10_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude (latAmp 5 - 10)')


text(x= bp[1], y = dt_latAmp_5_10_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], y = dt_latAmp_5_10_minLat[2,1]/2 + dt_latAmp_5_10_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], y = dt_latAmp_5_10_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2], y = dt_latAmp_5_10_minLat[2,2]/2 + dt_latAmp_5_10_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3], y= dt_latAmp_5_10_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3], y= dt_latAmp_5_10_minLat[2,3]/2 + dt_latAmp_5_10_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4], y= dt_latAmp_5_10_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4], y= dt_latAmp_5_10_minLat[2,4]/2 + dt_latAmp_5_10_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5], y= dt_latAmp_5_10_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_5_10_minLat[2,5]/2 + dt_latAmp_5_10_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_5_10_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_5_10_minLat[2,6]/2 + dt_latAmp_5_10_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_40_50$minTslope_RP < 0)))

text(x= bp[7], y= dt_latAmp_5_10_minLat[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP > 0)))
text(x= bp[7], y= dt_latAmp_5_10_minLat[2,7]/2 + dt_latAmp_5_10_minLat[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_5_10_meanLat_50$minTslope_RP < 0)))



#####. latAmp 10 to 15 degrees
res_temp_latAmp_10_15_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 10 &
                            res_temp_latAmp_meanLat$latAmplitude < 15,]

#### all lats
res_temp_latAmp_10_15_meanLat_all <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat > 0),]

pos_latAmp_10_15_meanLat_all <- 
  sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_all) * 100
neg_latAmp_10_15_meanLat_all <- 
  sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_all) * 100

#### 0 to 10 degrees
res_temp_latAmp_10_15_meanLat_0_10 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat < 10),]

pos_latAmp_10_15_meanLat_0_10 <-
  sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_0_10) * 100
neg_latAmp_10_15_meanLat_0_10 <-
  sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_0_10) * 100

#### 10 to 20 degrees
res_temp_latAmp_10_15_meanLat_10_20 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 10 &
          res_temp_latAmp_10_15_meanLat$meanLat < 20),]

pos_latAmp_10_15_meanLat_10_20 <-
  sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_10_20) * 100
neg_latAmp_10_15_meanLat_10_20 <-
  sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_10_20) * 100

#### 20 to 30 degrees
res_temp_latAmp_10_15_meanLat_20_30 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 20 &
          res_temp_latAmp_10_15_meanLat$meanLat < 30),]

pos_latAmp_10_15_meanLat_20_30 <-
  sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_20_30) * 100
neg_latAmp_10_15_meanLat_20_30 <-
  sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_20_30) * 100

#### 30 to 40 degrees
res_temp_latAmp_10_15_meanLat_30_40 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 30 &
          res_temp_latAmp_10_15_meanLat$meanLat < 40),]

pos_latAmp_10_15_meanLat_30_40 <-
  sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_30_40) * 100
neg_latAmp_10_15_meanLat_30_40 <-
  sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_30_40) * 100

#### more than 40 degress
res_temp_latAmp_10_15_meanLat_40 <- res_temp_latAmp_10_15_meanLat[
  which(res_temp_latAmp_10_15_meanLat$meanLat >= 40),]

pos_latAmp_10_15_meanLat_40 <-
  sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP > 0) /
  nrow(res_temp_latAmp_10_15_meanLat_40) * 100
neg_latAmp_10_15_meanLat_40 <-
  sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP < 0) /
  nrow(res_temp_latAmp_10_15_meanLat_40) * 100

#create a matrix with the data
dt_latAmp_10_15_minLat <-
  matrix(c(c(pos_latAmp_10_15_meanLat_all, neg_latAmp_10_15_meanLat_all),
           c(pos_latAmp_10_15_meanLat_0_10, neg_latAmp_10_15_meanLat_0_10),
           c(pos_latAmp_10_15_meanLat_10_20, neg_latAmp_10_15_meanLat_10_20),
           c(pos_latAmp_10_15_meanLat_20_30, neg_latAmp_10_15_meanLat_20_30),
           c(pos_latAmp_10_15_meanLat_30_40, neg_latAmp_10_15_meanLat_30_40),
           c(pos_latAmp_10_15_meanLat_40, neg_latAmp_10_15_meanLat_40)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_10_15_minLat) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_10_15_minLat_xx <-
  matrix(c(c(pos_latAmp_10_15_meanLat_all,
             neg_latAmp_10_15_meanLat_all, xx,xx,xx,xx,xx),
           c(xx, pos_latAmp_10_15_meanLat_0_10,
             neg_latAmp_10_15_meanLat_0_10, xx,xx,xx,xx),
           c(xx,xx, pos_latAmp_10_15_meanLat_10_20,
             neg_latAmp_10_15_meanLat_10_20, xx,xx,xx),
           c(xx,xx,xx, pos_latAmp_10_15_meanLat_20_30,
             neg_latAmp_10_15_meanLat_20_30, xx,xx),
           c(xx,xx,xx,xx, pos_latAmp_10_15_meanLat_30_40,
             neg_latAmp_10_15_meanLat_30_40, xx),
           c(xx,xx,xx,xx,xx, pos_latAmp_10_15_meanLat_40,
             neg_latAmp_10_15_meanLat_40)),
         ncol = 6)

#name the columns
colnames(dt_latAmp_10_15_minLat_xx) <-
  c('all', 'Lat_0_10', 'Lat_10_20', 'Lat_20_30',
    'Lat_30_40', 'Lat_40')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_10_15_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Mean Latitude (latAmp 10 - 15)')


text(x= bp[1], 
     y = dt_latAmp_10_15_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], 
     y = dt_latAmp_10_15_minLat[2,1]/2 + dt_latAmp_10_15_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], 
     y = dt_latAmp_10_15_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP > 0)))
text(x= bp[2],
     y = dt_latAmp_10_15_minLat[2,2]/2 + dt_latAmp_10_15_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_0_10$minTslope_RP < 0)))

text(x= bp[3],
     y= dt_latAmp_10_15_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = 
       paste(sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP > 0)))
text(x= bp[3],
     y= dt_latAmp_10_15_minLat[2,3]/2 + dt_latAmp_10_15_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_10_20$minTslope_RP < 0)))

text(x= bp[4],
     y= dt_latAmp_10_15_minLat[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP > 0)))
text(x= bp[4],
     y= dt_latAmp_10_15_minLat[2,4]/2 + dt_latAmp_10_15_minLat[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_20_30$minTslope_RP < 0)))

text(x= bp[5],
     y= dt_latAmp_10_15_minLat[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP > 0)))
text(x= bp[5], y= dt_latAmp_10_15_minLat[2,5]/2 + dt_latAmp_10_15_minLat[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels =
       paste(sum(res_temp_latAmp_10_15_meanLat_30_40$minTslope_RP < 0)))

text(x= bp[6], y= dt_latAmp_10_15_minLat[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP > 0)))
text(x= bp[6], y= dt_latAmp_10_15_minLat[2,6]/2 + dt_latAmp_10_15_minLat[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_10_15_meanLat_40$minTslope_RP < 0)))



#####. latAmp more than 15 degrees
res_temp_latAmp_15_meanLat <-
  res_temp_latAmp_meanLat[res_temp_latAmp_meanLat$latAmplitude >= 15,]

#### all lats
res_temp_latAmp_15_meanLat_all <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat > 0),]

pos_latAmp_15_meanLat_all <- 
  sum(res_temp_latAmp_15_meanLat_all$minTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_all) * 100
neg_latAmp_15_meanLat_all <- 
  sum(res_temp_latAmp_15_meanLat_all$minTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_all) * 100

#### 0 to 30 degrees
res_temp_latAmp_15_meanLat_0_30 <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat < 30),]

pos_latAmp_15_meanLat_0_30 <-
  sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_0_30) * 100
neg_latAmp_15_meanLat_0_30 <-
  sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_0_30) * 100

#### more than 30 degrees
res_temp_latAmp_15_meanLat_30 <- res_temp_latAmp_15_meanLat[
  which(res_temp_latAmp_15_meanLat$meanLat >= 30 &
          res_temp_latAmp_15_meanLat$meanLat < 90),]

pos_latAmp_15_meanLat_30 <-
  sum(res_temp_latAmp_15_meanLat_30$minTslope_RP > 0) /
  nrow(res_temp_latAmp_15_meanLat_30) * 100
neg_latAmp_15_meanLat_30 <-
  sum(res_temp_latAmp_15_meanLat_30$minTslope_RP < 0) /
  nrow(res_temp_latAmp_15_meanLat_30) * 100

#create a matrix with the data
dt_latAmp_15_minLat <-
  matrix(c(c(pos_latAmp_15_meanLat_all, neg_latAmp_15_meanLat_all),
           c(pos_latAmp_15_meanLat_0_30, neg_latAmp_15_meanLat_0_30),
           c(pos_latAmp_15_meanLat_30, neg_latAmp_15_meanLat_30)),
         ncol = 3)

#name the columns
colnames(dt_latAmp_15_minLat) <-
  c('all', 'Lat_0_30', 'Lat_30')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_latAmp_15_minLat_xx <-
  matrix(c(c(pos_latAmp_15_meanLat_all,
             neg_latAmp_15_meanLat_all, xx,xx),
           c(xx, pos_latAmp_15_meanLat_0_30,
             neg_latAmp_15_meanLat_0_30, xx),
           c(xx,xx, pos_latAmp_15_meanLat_30,
             neg_latAmp_15_meanLat_30)),
         ncol = 3)

#name the columns
colnames(dt_latAmp_15_minLat_xx) <- c('all', 'Lat_0_30', 'Lat_30')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_latAmp_15_minLat_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Mean Latitude (latAmp > 15)')


text(x= bp[1], 
     y = dt_latAmp_15_minLat[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_all$minTslope_RP > 0)))
text(x= bp[1], 
     y = dt_latAmp_15_minLat[2,1]/2 + dt_latAmp_15_minLat[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_all$minTslope_RP < 0)))

text(x= bp[2], 
     y = dt_latAmp_15_minLat[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP > 0)))
text(x= bp[2],
     y = dt_latAmp_15_minLat[2,2]/2 + dt_latAmp_15_minLat[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_0_30$minTslope_RP < 0)))

text(x= bp[3],
     y= dt_latAmp_15_minLat[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_30$minTslope_RP > 0)))
text(x= bp[3],
     y= dt_latAmp_15_minLat[2,3]/2 + dt_latAmp_15_minLat[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_latAmp_15_meanLat_30$minTslope_RP < 0)))














#################################################################
########################## CENTRALNESS ##########################
#################################################################


############################
######### MIN TEMP #########
############################


###### check relationships by range roundness

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_C != 0),]

#### all roundness
res_temp_large_ranges2 <- res_temp_large_ranges[which
              (res_temp_large_ranges$rangeRoundness >= 0),]

pos_round_all <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_all <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.2
res_temp_large_ranges2 <- res_temp_large_ranges[which
              (res_temp_large_ranges$rangeRoundness >= 0.2),]

pos_round_02 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_02 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.4
res_temp_large_ranges2 <- res_temp_large_ranges[which
           (res_temp_large_ranges$rangeRoundness >= 0.4),]

pos_round_04 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_04 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.6
res_temp_large_ranges2 <- res_temp_large_ranges[which
            (res_temp_large_ranges$rangeRoundness >= 0.6),]

pos_round_06 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_06 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


#### more than 0.8
res_temp_large_ranges2 <- res_temp_large_ranges[which
            (res_temp_large_ranges$rangeRoundness >= 0.8),]

pos_round_08 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_08 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_round <- data.frame(all = c(pos_latAmp_all, neg_latAmp_all),
                        min_02 = c(pos_round_02, neg_round_02),
                        min_04 = c(pos_round_04, neg_round_04),
                        min_06 = c(pos_round_06, neg_round_06),
                        min_08 = c(pos_round_08, neg_round_08))

row.names(dt_round) <- c('Positive', 'Negative')

dt_round <-as.matrix(dt_round)    


barplot(dt_round,
        col = c("blue", "red"),
        legend = NA)


#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$minTslope_C != 0),]

#### all amplitudes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_all <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 100000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 100000),]

pos_rangeSize_100000 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_100000 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 500000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 500000),]

pos_rangeSize_500000 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_500000 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 1000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 1000000),]

pos_rangeSize_1000000 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_1000000 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 2000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 2000000),]

pos_rangeSize_2000000 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_2000000 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 3000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 3000000),]

pos_rangeSize_3000000 <- sum(res_temp_large_ranges2$minTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_3000000 <- sum(res_temp_large_ranges2$minTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_rangeSize <- data.frame(all = c(pos_rangeSize_all, neg_rangeSize_all),
                min_100K = c(pos_rangeSize_100000, neg_rangeSize_100000),
                min_500K = c(pos_rangeSize_500000, neg_rangeSize_500000),
                min_1000K = c(pos_rangeSize_1000000, neg_rangeSize_1000000),
                min_2000K = c(pos_rangeSize_2000000, neg_rangeSize_2000000),
                min_3M = c(pos_rangeSize_3000000, neg_rangeSize_3000000))

row.names(dt_rangeSize) <- c('Positive', 'Negative')

dt_rangeSize <-as.matrix(dt_rangeSize)    


barplot(dt_rangeSize,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots



############################
######### MEAN TEMP ########
############################


###### check relationships by range roundness

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_C != 0),]

#### all roundness
res_temp_large_ranges2 <- res_temp_large_ranges[which
           (res_temp_large_ranges$rangeRoundness >= 0),]

pos_round_all <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_all <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.2
res_temp_large_ranges2 <- res_temp_large_ranges[which
         (res_temp_large_ranges$rangeRoundness >= 0.2),]

pos_round_02 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_02 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.4
res_temp_large_ranges2 <- res_temp_large_ranges[which
          (res_temp_large_ranges$rangeRoundness >= 0.4),]

pos_round_04 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_04 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.6
res_temp_large_ranges2 <- res_temp_large_ranges[which
         (res_temp_large_ranges$rangeRoundness >= 0.6),]

pos_round_06 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_06 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


#### more than 0.8
res_temp_large_ranges2 <- res_temp_large_ranges[which
        (res_temp_large_ranges$rangeRoundness >= 0.8),]

pos_round_08 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_08 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_round <- data.frame(all = c(pos_latAmp_all, neg_latAmp_all),
                       min_02 = c(pos_round_02, neg_round_02),
                       min_04 = c(pos_round_04, neg_round_04),
                       min_06 = c(pos_round_06, neg_round_06),
                       min_08 = c(pos_round_08, neg_round_08))

row.names(dt_round) <- c('Positive', 'Negative')

dt_round <-as.matrix(dt_round)    


barplot(dt_round,
        col = c("blue", "red"),
        legend = NA)


#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$meanTslope_C != 0),]

#### all range sizes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_all <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 100000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 100000),]

pos_rangeSize_100000 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_100000 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 500000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 500000),]

pos_rangeSize_500000 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_500000 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 1000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 1000000),]

pos_rangeSize_1000000 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_1000000 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 2000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 2000000),]

pos_rangeSize_2000000 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_2000000 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 3000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 3000000),]

pos_rangeSize_3000000 <- sum(res_temp_large_ranges2$meanTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_3000000 <- sum(res_temp_large_ranges2$meanTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_rangeSize <- data.frame(all = c(pos_rangeSize_all, neg_rangeSize_all),
                  min_100K = c(pos_rangeSize_100000, neg_rangeSize_100000),
                  min_500K = c(pos_rangeSize_500000, neg_rangeSize_500000),
                  min_1000K = c(pos_rangeSize_1000000, neg_rangeSize_1000000),
                  min_2000K = c(pos_rangeSize_2000000, neg_rangeSize_2000000),
                  min_3M = c(pos_rangeSize_3000000, neg_rangeSize_3000000))

row.names(dt_rangeSize) <- c('Positive', 'Negative')

dt_rangeSize <-as.matrix(dt_rangeSize)    


barplot(dt_rangeSize,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots



############################
######### MAX TEMP #########
############################


###### check relationships by range roundness

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_C != 0),]

#### all roundness
res_temp_large_ranges2 <- res_temp_large_ranges[which
        (res_temp_large_ranges$rangeRoundness >= 0),]

pos_round_all <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_all <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.2
res_temp_large_ranges2 <- res_temp_large_ranges[which
         (res_temp_large_ranges$rangeRoundness >= 0.2),]

pos_round_02 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_02 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.4
res_temp_large_ranges2 <- res_temp_large_ranges[which
        (res_temp_large_ranges$rangeRoundness >= 0.4),]

pos_round_04 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_04 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 0.6
res_temp_large_ranges2 <- res_temp_large_ranges[which
        (res_temp_large_ranges$rangeRoundness >= 0.6),]

pos_round_06 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_06 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


#### more than 0.8
res_temp_large_ranges2 <- res_temp_large_ranges[which
          (res_temp_large_ranges$rangeRoundness >= 0.8),]

pos_round_08 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_round_08 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_round <- data.frame(all = c(pos_latAmp_all, neg_latAmp_all),
                       min_02 = c(pos_round_02, neg_round_02),
                       min_04 = c(pos_round_04, neg_round_04),
                       min_06 = c(pos_round_06, neg_round_06),
                       min_08 = c(pos_round_08, neg_round_08))

row.names(dt_round) <- c('Positive', 'Negative')

dt_round <-as.matrix(dt_round)    


barplot(dt_round,
        col = c("blue", "red"),
        legend = NA)


#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_large_ranges <- res_temp[which(res_temp$maxTslope_C != 0),]

#### all range sizes
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_all <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 100000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 100000),]

pos_rangeSize_100000 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_100000 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 500000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 500000),]

pos_rangeSize_500000 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_500000 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 1000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 1000000),]

pos_rangeSize_1000000 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_1000000 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 2000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 2000000),]

pos_rangeSize_2000000 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_2000000 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100

#### more than 3000000 km2
res_temp_large_ranges2 <- res_temp_large_ranges[
  which(res_temp_large_ranges$rangeSize >= 3000000),]

pos_rangeSize_3000000 <- sum(res_temp_large_ranges2$maxTslope_C > 0) /
  nrow(res_temp_large_ranges2) * 100
neg_rangeSize_3000000 <- sum(res_temp_large_ranges2$maxTslope_C < 0) /
  nrow(res_temp_large_ranges2) * 100


dt_rangeSize <- data.frame(all = c(pos_rangeSize_all, neg_rangeSize_all),
                  min_100K = c(pos_rangeSize_100000, neg_rangeSize_100000),
                  min_500K = c(pos_rangeSize_500000, neg_rangeSize_500000),
                  min_1000K = c(pos_rangeSize_1000000, neg_rangeSize_1000000),
                  min_2000K = c(pos_rangeSize_2000000, neg_rangeSize_2000000),
                  min_3M = c(pos_rangeSize_3000000, neg_rangeSize_3000000))

row.names(dt_rangeSize) <- c('Positive', 'Negative')

dt_rangeSize <-as.matrix(dt_rangeSize)    


barplot(dt_rangeSize,
        col = c("blue", "red"),
        legend = NA)

#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots
