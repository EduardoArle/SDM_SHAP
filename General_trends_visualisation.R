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

#select rows with at least 10 records (not for now)
# res_temp2 <- res_temp[res_temp$n_records > 9,]
#eliminate rows with NAs
#res_temp3 <- res_temp[complete.cases(res_temp$minTp_value_RP),] 

################################################################################
#########################                        ###############################
#########################      POLARWARDNESS     ###############################
#########################                        ###############################
################################################################################


#classify rows by p-value bins for the minT
res_temp3 <-res_temp
res_temp3$minTp_value_RP_bin <- NA

res_temp3$minTp_value_RP_bin[res_temp3$minTp_value_RP >= 0.1] <- 1
res_temp3$minTp_value_RP_bin[res_temp3$minTp_value_RP < 0.1 &
                               res_temp3$minTp_value_RP >= 0.05] <- 2
res_temp3$minTp_value_RP_bin[res_temp3$minTp_value_RP < 0.05 &
                               res_temp3$minTp_value_RP >= 0.01] <- 3
res_temp3$minTp_value_RP_bin[res_temp3$minTp_value_RP < 0.01 &
                               res_temp3$minTp_value_RP >= 0.001] <- 4
res_temp3$minTp_value_RP_bin[res_temp3$minTp_value_RP < 0.001] <- 5

#make a colour column based on the min p-value
res_temp3$minTp_value_RP_colour <- 'white'
  
res_temp3$minTp_value_RP_colour[res_temp3$minTp_value_RP_bin == 1] <- '#f1eef6'
res_temp3$minTp_value_RP_colour[res_temp3$minTp_value_RP_bin == 2] <- '#bdc9e1'
res_temp3$minTp_value_RP_colour[res_temp3$minTp_value_RP_bin == 3] <- '#74a9cf'
res_temp3$minTp_value_RP_colour[res_temp3$minTp_value_RP_bin == 4] <- '#2b8cbe'
res_temp3$minTp_value_RP_colour[res_temp3$minTp_value_RP_bin == 5] <- '#045a8d'
          
#classify rows by p-value bins for the meanT
res_temp3$meanTp_value_RP_bin <- NA
        
res_temp3$meanTp_value_RP_bin[res_temp3$meanTp_value_RP >= 0.1] <- 1
res_temp3$meanTp_value_RP_bin[res_temp3$meanTp_value_RP < 0.1 &
                              res_temp3$meanTp_value_RP >= 0.05] <- 2
res_temp3$meanTp_value_RP_bin[res_temp3$meanTp_value_RP < 0.05 &
                              res_temp3$meanTp_value_RP >= 0.01] <- 3
res_temp3$meanTp_value_RP_bin[res_temp3$meanTp_value_RP < 0.01 &
                              res_temp3$meanTp_value_RP >= 0.001] <- 4
res_temp3$meanTp_value_RP_bin[res_temp3$meanTp_value_RP < 0.001] <- 5
        
#make a colour column based on the mean p-value
res_temp3$meanTp_value_RP_colour <- 'white'
          
res_temp3$meanTp_value_RP_colour[res_temp3$meanTp_value_RP_bin == 1] <- '#f2f0f7'
res_temp3$meanTp_value_RP_colour[res_temp3$meanTp_value_RP_bin == 2] <- '#cbc9e2'
res_temp3$meanTp_value_RP_colour[res_temp3$meanTp_value_RP_bin == 3] <- '#9e9ac8'
res_temp3$meanTp_value_RP_colour[res_temp3$meanTp_value_RP_bin == 4] <- '#756bb1'
res_temp3$meanTp_value_RP_colour[res_temp3$meanTp_value_RP_bin == 5] <- '#54278f'
                  
#classify rows by p-value bins for the maxT
res_temp3$maxTp_value_RP_bin <- NA
                
res_temp3$maxTp_value_RP_bin[res_temp3$maxTp_value_RP >= 0.1] <- 1
res_temp3$maxTp_value_RP_bin[res_temp3$maxTp_value_RP < 0.1 &
                             res_temp3$maxTp_value_RP >= 0.05] <- 2
res_temp3$maxTp_value_RP_bin[res_temp3$maxTp_value_RP < 0.05 &
                             res_temp3$meanTp_value_RP >= 0.01] <- 3
res_temp3$maxTp_value_RP_bin[res_temp3$maxTp_value_RP < 0.01 &
                             res_temp3$meanTp_value_RP >= 0.001] <- 4
res_temp3$maxTp_value_RP_bin[res_temp3$maxTp_value_RP < 0.001] <- 5
                
#make a colour column based on the mean p-value
res_temp3$maxTp_value_RP_colour <- 'white'
                  
res_temp3$maxTp_value_RP_colour[res_temp3$maxTp_value_RP_bin == 1] <- '#fee5d9'
res_temp3$maxTp_value_RP_colour[res_temp3$maxTp_value_RP_bin == 2] <- '#fcae91'
res_temp3$maxTp_value_RP_colour[res_temp3$maxTp_value_RP_bin == 3] <- '#fb6a4a'
res_temp3$maxTp_value_RP_colour[res_temp3$maxTp_value_RP_bin == 4] <- '#de2d26'
res_temp3$maxTp_value_RP_colour[res_temp3$maxTp_value_RP_bin == 5] <- '#a50f15'
                          
#classify species by latitudinal amplitude
res_temp3$latAmplitude <- res_temp3$maxLat - res_temp3$minLat
res_temp3$latAmplitude_bin <- NA
                        
res_temp3$latAmplitude_bin[res_temp3$latAmplitude  < 1] <- 1
                        
res_temp3$latAmplitude_bin[res_temp3$latAmplitude >= 1 &
                           res_temp3$latAmplitude < 5] <- 2
                        
res_temp3$latAmplitude_bin[res_temp3$latAmplitude >= 5 &
                           res_temp3$latAmplitude < 10] <- 3
                        
res_temp3$latAmplitude_bin[res_temp3$latAmplitude >=  10] <- 4

                        
#get the xlim for the plot
res_temp4$minT_minVals <- res_temp4$minTslope_RP - res_temp4$minTSE_RP
res_temp4$minT_maxVals <- res_temp4$minTslope_RP + res_temp4$minTSE_RP
                        
res_temp4$meanT_minVals <- res_temp4$meanTslope_RP - res_temp4$meanTSE_RP
res_temp4$meanT_maxVals <- res_temp4$meanTslope_RP + res_temp4$meanTSE_RP
                        
res_temp4$maxT_minVals <- res_temp4$maxTslope_RP - res_temp4$maxTSE_RP
res_temp4$maxT_maxVals <- res_temp4$maxTslope_RP + res_temp4$maxTSE_RP
                        
x_lim <- c(res_temp4$minT_minVals, res_temp4$minT_maxVals,
           res_temp4$meanT_minVals, res_temp4$meanT_maxVals,
           res_temp4$maxT_minVals, res_temp4$maxT_maxVals)
                        

####### MIN TEMP ##########


#order the species from highest to lowest slope of min T
res_temp4 <- res_temp3[order(res_temp3$minTslope_RP),] #order for the plot

#make an empty plot
par(mar = c(5,10,1,1))

plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
     xlim = c(-2, 2),
     ylim = c(0, nrow(res_temp4))) 

#add X axis
axis(side = 1, 
     at = seq(-2, 2, 0.5),
     cex.axis = 1, padj = 0, las =1)

#add X axis
axis(side = 2, 
     at = c(1:nrow(res_temp4)),
     labels = res_temp4$species,
     cex.axis = 0.8, padj = 0, las = 2, font = 3)

#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)

#add points minT
points(res_temp4$minTslope_RP, c(1:nrow(res_temp4)), pch = 19,
       col = res_temp4$minTp_value_RP_colour,
       cex = res_temp4$latAmplitude_bin)


####### MEAN TEMP ##########


#order the species from highest to lowest slope of min T
res_temp4 <- res_temp3[order(res_temp3$meanTslope_RP),] #order for the plot


#make an empty plot
par(mar = c(5,10,1,1))

plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
     xlim = c(-2, 2),
     ylim = c(0, nrow(res_temp4))) 

#add X axis
axis(side = 1, 
     at = seq(-2, 2, 0.5),
     cex.axis = 1, padj = 0, las =1)

#add X axis
axis(side = 2, 
     at = c(1:nrow(res_temp4)),
     labels = res_temp4$species,
     cex.axis = 0.8, padj = 0, las = 2, font = 3)

#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)

#add points maxT
points(res_temp4$meanTslope_RP, c(1:nrow(res_temp4)), pch = 19,
       col = res_temp4$meanTp_value_RP_colour,
       cex = res_temp4$latAmplitude_bin)


#################


#order the species from highest to lowest slope of max T
res_temp4 <- res_temp3[order(res_temp3$maxTslope_RP),] #order for the plot


#make an empty plot
par(mar = c(5,10,1,1))

plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
     xlim = c(-2, 2),
     ylim = c(0, nrow(res_temp4))) 

#add X axis
axis(side = 1, 
     at = seq(-2, 2, 0.5),
     cex.axis = 1, padj = 0, las =1)

#add X axis
axis(side = 2, 
     at = c(1:nrow(res_temp4)),
     labels = res_temp4$species,
     cex.axis = 0.8, padj = 0, las = 2, font = 3)

#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)

#add points maxT
points(res_temp4$maxTslope_RP, c(1:nrow(res_temp4)), pch = 19,
       col = res_temp4$maxTp_value_RP_colour,
       cex = res_temp4$latAmplitude_bin)

###########################


#add lines minT
for(j in 1:nrow(res_temp4))
{
  #make horizontal lines
  lines(x = c(res_temp4$minT_minVals[j],
              res_temp4$minT_maxVals[j]),
        y = c(j, j),
        lwd = 2, col = res_temp4$minTp_value_RP_colour[j])
  
  #make ticks in the ends of the lines
  lines(x = c(res_temp4$minT_minVals[j],
              res_temp4$minT_minVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = res_temp4$minTp_value_RP_colour[j])
  
  lines(x = c(res_temp4$minT_maxVals[j],
              res_temp4$minT_maxVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = res_temp4$minTp_value_RP_colour[j])
}

#add points meanT
points(res_temp4$meanTslope_C, c(1:nrow(res_temp4)), pch = 19,
       col = res_temp4$meanTp_value_C_colour,
       cex = res_temp4$rangeSize_bin)


#add lines meanT
for(j in 1:nrow(res_temp4))
{
  #make horizontal lines
  lines(x = c(res_temp4$meanT_minVals_RP[j],
              res_temp4$meanT_maxVals_RP[j]),
        y = c(j, j),
        lwd = 2, col = res_temp4$meanTp_value_RP_colour[j])
  
  #make ticks in the ends of the lines
  lines(x = c(res_temp4$meanT_minVals_RP[j],
              res_temp4$meanT_minVals_RP[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = res_temp4$meanTp_value_RP_colour[j])
  
  lines(x = c(res_temp4$meanT_maxVals_RP[j],
              res_temp4$meanT_maxVals_RP[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = res_temp4$meanTp_value_RP_colour[j])
}


#add points maxT
points(res_temp4$maxTslope_C, c(1:nrow(res_temp4)), pch = 19,
       col = res_temp4$maxTp_value_C_colour,
       cex = res_temp4$rangeSize_bin)


#add lines maxT
for(j in 1:nrow(res_temp4))
{
  #make horizontal lines
  lines(x = c(res_temp4$maxT_minVals_C[j],
              res_temp4$maxT_maxVals_C[j]),
        y = c(j, j),
        lwd = 2, col = res_temp4$maxTp_value_C_colour[j])
  
  #make ticks in the ends of the lines
  lines(x = c(res_temp4$maxT_minVals_C[j],
              res_temp4$maxT_minVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = res_temp4$maxTp_value_C_colour[j])
  
  lines(x = c(res_temp4$maxT_maxVals_C[j],
              res_temp4$maxT_maxVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = res_temp4$maxTp_value_C_colour[j])
}




################ SCRAP



sum(res_temp4$minTslope_RP > 0.05)

sum(res_temp4$minTslope_RP < -0.05)

sum(res_temp4$maxTslope_RP > 0.05)

sum(res_temp4$maxTslope_RP < -0.05)

sum(res_temp4$meanTslope_RP > 0.05)

sum(res_temp4$meanTslope_RP < -0.05)