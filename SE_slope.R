#load libraries

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'


#..............................................................................#
#..............................................................................#
#................................TEMPERATURE...................................#
#..............................................................................#
#..............................................................................#                


#read table
setwd(wd_tables)
res_temp <- read.csv('Temperature_Rel_Polar_all_points.csv')

#calculate 1/SE * slope
res_temp$one_SE_x_slope_minT_RP <- (1/res_temp$minTSE_RP) * res_temp$minTslope_RP 

#classify species by latitudinal amplitude
res_temp$latAmplitude <- res_temp$maxLat - res_temp$minLat
res_temp$latAmplitude_bin <- NA

res_temp$latAmplitude_bin[res_temp$latAmplitude  < 1] <- 1

res_temp$latAmplitude_bin[res_temp$latAmplitude >= 1 &
                             res_temp$latAmplitude < 5] <- 2

res_temp$latAmplitude_bin[res_temp$latAmplitude >= 5 &
                             res_temp$latAmplitude < 10] <- 3

res_temp$latAmplitude_bin[res_temp$latAmplitude >=  10] <- 4



####### MIN TEMP ##########

#get the limits for the plot
summary(res_temp$one_SE_x_slope_minT_RP)

#order the species from highest to lowest '1/SE * slope' of min T
res_temp2 <- res_temp[order(res_temp$one_SE_x_slope_minT_RP),] #order for the plot

#make an empty plot
par(mar = c(5,10,1,1))

plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
     xlim = c(-30, 30),
     ylim = c(0, nrow(res_temp2))) 

#add X axis
axis(side = 1, 
     at = seq(-30, 30, 10),
     cex.axis = 1, padj = 0, las =1)

#add X axis
axis(side = 2, 
     at = c(1:nrow(res_temp2)),
     labels = res_temp2$species,
     cex.axis = 0.8, padj = 0, las = 2, font = 3)

#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)

#add points minT
points(res_temp2$one_SE_x_slope_minT_RP, c(1:nrow(res_temp2)), pch = 19,
       col = '#74a9cf',
       cex = res_temp2$latAmplitude_bin)




sum(res_temp2$minTslope_RP > 0)
sum(res_temp2$one_SE_x_slope_minT_RP > 0, na.rm = T)

sum(res_temp2$minTslope_RP < 0)
sum(res_temp2$one_SE_x_slope_minT_RP < 0, na.rm = T)

