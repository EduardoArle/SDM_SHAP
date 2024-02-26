#load libraries
library(metafor)

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

#order the species from highest to lowest slope of min T
res_temp4 <- res_temp3[order(res_temp3$minTslope_RP),] #order for the plot

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


#### plot 20 species per plot ####
data_split <- list()

data_split[[1]] <- res_temp4[c(1:20), ]
data_split[[2]] <- res_temp4[c(21:40), ]
data_split[[3]] <- res_temp4[c(41:60), ]
data_split[[4]] <- res_temp4[c(61:80), ]
data_split[[5]] <- res_temp4[c(81:100), ]
data_split[[6]] <- res_temp4[c(101:120), ]
data_split[[7]] <- res_temp4[c(121:140), ]
data_split[[8]] <- res_temp4[c(141:160), ]
data_split[[9]] <- res_temp4[c(161:180), ]
data_split[[10]] <- res_temp4[c(181:200), ]
data_split[[11]] <- res_temp4[c(201:220), ]
data_split[[12]] <- res_temp4[c(221:240), ]
data_split[[13]] <- res_temp4[c(241:260), ]
data_split[[14]] <- res_temp4[c(261:280), ]
data_split[[15]] <- res_temp4[c(281:300), ]
data_split[[16]] <- res_temp4[c(301:320), ]
data_split[[17]] <- res_temp4[c(321:340), ]
data_split[[18]] <- res_temp4[c(341:360), ]
data_split[[19]] <- res_temp4[c(361:374), ]

#### PLOT ####
i=19

#make an empty plot
par(mar = c(5,10,1,1))

plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
     xlim = c(-1.5, 1.5),
     ylim = c(0, nrow(data_split[[i]]))) 

#add X axis
axis(side = 1, 
     at = seq(-1.5, 1.5, 0.5),
     cex.axis = 1, padj = 0, las =1)

#add X axis
axis(side = 2, 
     at = c(1:nrow(data_split[[i]])),
     labels = data_split[[i]]$species,
     cex.axis = 0.8, padj = 0, las = 2, font = 3)

#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)

#add points minT
points(data_split[[i]]$minTslope_RP, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$minTp_value_RP_colour,
       cex = data_split[[i]]$latAmplitude_bin)

#add lines minT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$minT_minVals[j], data_split[[i]]$minT_maxVals[j]),
        y = c(j, j),
        lwd = 2, col = data_split[[i]]$minTp_value_RP_colour[j])
  
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$minT_minVals[j], data_split[[i]]$minT_minVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$minTp_value_RP_colour[j])
  
  lines(x = c(data_split[[i]]$minT_maxVals[j], data_split[[i]]$minT_maxVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$minTp_value_RP_colour[j])
}

#add points meanT
points(data_split[[i]]$meanTslope_RP, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$meanTp_value_RP_colour,
       cex = data_split[[i]]$latAmplitude_bin)


#add lines meanT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$meanT_minVals[j], data_split[[i]]$meanT_maxVals[j]),
        y = c(j, j),
        lwd = 2, col = data_split[[i]]$meanTp_value_RP_colour[j])
  
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$meanT_minVals[j], data_split[[i]]$meanT_minVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$meanTp_value_RP_colour[j])
  
  lines(x = c(data_split[[i]]$meanT_maxVals[j], data_split[[i]]$meanT_maxVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$meanTp_value_RP_colour[j])
}


#add points maxT
points(data_split[[i]]$maxTslope_RP, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$maxTp_value_RP_colour,
       cex = data_split[[i]]$latAmplitude_bin)


#add lines maxT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$maxT_minVals[j], data_split[[i]]$maxT_maxVals[j]),
        y = c(j, j),
        lwd = 2, col = data_split[[i]]$maxTp_value_RP_colour[j])
  
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$maxT_minVals[j], data_split[[i]]$maxT_minVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$maxTp_value_RP_colour[j])
  
  lines(x = c(data_split[[i]]$maxT_maxVals[j], data_split[[i]]$maxT_maxVals[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$maxTp_value_RP_colour[j])
}


################################################################################
#########################                        ###############################
#########################      CENTRALNESS       ###############################
#########################                        ###############################
################################################################################


#classify rows by p-value bins for the minT
res_temp3 <-res_temp
res_temp3$minTp_value_C_bin <- NA

res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C >= 0.1] <- 1
res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.1 &
                            res_temp3$minTp_value_C >= 0.05] <- 2
res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.05 &
                            res_temp3$minTp_value_C >= 0.01] <- 3
res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.01 &
                            res_temp3$minTp_value_C >= 0.001] <- 4
res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.001] <- 5

#make a colour column based on the min p-value
res_temp3$minTp_value_C_colour <- 'white'
  
res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 1] <- '#f1eef6'
res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 2] <- '#bdc9e1'
res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 3] <- '#74a9cf'
res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 4] <- '#2b8cbe'
res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 5] <- '#045a8d'
          
#classify rows by p-value bins for the meanT
res_temp3$meanTp_value_C_bin <- NA

res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C >= 0.1] <- 1
res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.1 &
                             res_temp3$meanTp_value_C >= 0.05] <- 2
res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.05 &
                             res_temp3$meanTp_value_C >= 0.01] <- 3
res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.01 &
                             res_temp3$meanTp_value_C >= 0.001] <- 4
res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.001] <- 5
        
#make a colour column based on the mean p-value
res_temp3$meanTp_value_C_colour <- 'white'
          
res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 1] <- '#f2f0f7'
res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 2] <- '#cbc9e2'
res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 3] <- '#9e9ac8'
res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 4] <- '#756bb1'
res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 5] <- '#54278f'
                  
#classify rows by p-value bins for the maxT
res_temp3$maxTp_value_C_bin <- NA
                
res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C >= 0.1] <- 1
res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.1 &
                             res_temp3$maxTp_value_C >= 0.05] <- 2
res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.05 &
                             res_temp3$maxTp_value_C >= 0.01] <- 3
res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.01 &
                             res_temp3$maxTp_value_C >= 0.001] <- 4
res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.001] <- 5

res_temp3$maxTp_value_C[which(res_temp3$maxTp_value_C < 0.001)] 

#make a colour column based on the mean p-value
res_temp3$maxTp_value_C_colour <- 'white'
                  
res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 1] <- '#fee5d9'
res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 2] <- '#fcae91'
res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 3] <- '#fb6a4a'
res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 4] <- '#de2d26'
res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 5] <- '#a50f15'

#classify species by range size (try also including roundness???)
res_temp3$rangeSize_bin <- NA
                        
res_temp3$rangeSize_bin[res_temp3$rangeSize < 100000] <- 1
                        
res_temp3$rangeSize_bin[res_temp3$rangeSize >= 100000 &
                        res_temp3$rangeSize < 500000] <- 2
                        
res_temp3$rangeSize_bin[res_temp3$rangeSize >= 500000 &
                        res_temp3$rangeSize < 1000000] <- 3
                        
res_temp3$rangeSize_bin[res_temp3$rangeSize >=  1000000] <- 4
                        
#order the species from highest to lowest slope of min T
res_temp4 <- res_temp3[order(res_temp3$minTslope_C),] #order for the plot
                        
#get the xlim for the plot
res_temp4$minT_minVals_C <- res_temp4$minTslope_C - res_temp4$minTSE_C
res_temp4$minT_maxVals_C <- res_temp4$minTslope_C + res_temp4$minTSE_C
                        
res_temp4$meanT_minVals_C <- res_temp4$meanTslope_C - res_temp4$meanTSE_C
res_temp4$meanT_maxVals_C <- res_temp4$meanTslope_C + res_temp4$meanTSE_C
                        
res_temp4$maxT_minVals_C <- res_temp4$maxTslope_C - res_temp4$maxTSE_C
res_temp4$maxT_maxVals_C <- res_temp4$maxTslope_C + res_temp4$maxTSE_C
                        
x_lim <- c(res_temp4$maxT_minVals_C, res_temp4$maxT_minVals_C,
           res_temp4$maxT_minVals_C, res_temp4$maxT_minVals_C,
           res_temp4$maxT_minVals_C, res_temp4$maxT_minVals_C)
                        
#### plot 20 species per plot ####
data_split <- list()
                        
data_split[[1]] <- res_temp4[c(1:20), ]
data_split[[2]] <- res_temp4[c(21:40), ]
data_split[[3]] <- res_temp4[c(41:60), ]
data_split[[4]] <- res_temp4[c(61:80), ]
data_split[[5]] <- res_temp4[c(81:100), ]
data_split[[6]] <- res_temp4[c(101:120), ]
data_split[[7]] <- res_temp4[c(121:140), ]
data_split[[8]] <- res_temp4[c(141:160), ]
data_split[[9]] <- res_temp4[c(161:180), ]
data_split[[10]] <- res_temp4[c(181:200), ]
data_split[[11]] <- res_temp4[c(201:220), ]
data_split[[12]] <- res_temp4[c(221:240), ]
data_split[[13]] <- res_temp4[c(241:260), ]
data_split[[14]] <- res_temp4[c(261:280), ]
data_split[[15]] <- res_temp4[c(281:300), ]
data_split[[16]] <- res_temp4[c(301:320), ]
data_split[[17]] <- res_temp4[c(321:340), ]
data_split[[18]] <- res_temp4[c(341:360), ]
data_split[[19]] <- res_temp4[c(361:374), ]
                        
#### PLOT ####
i=19
                        
#make an empty plot
par(mar = c(5,10,1,1))
                        
plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
     xlim = c(-1, 1),
     ylim = c(0, nrow(data_split[[i]]))) 
                        
#add X axis
axis(side = 1, 
     at = seq(-1, 1, 0.5),
     cex.axis = 1, padj = 0, las =1)
                        
#add X axis
axis(side = 2, 
    at = c(1:nrow(data_split[[i]])),
    labels = data_split[[i]]$species,
    cex.axis = 0.8, padj = 0, las = 2, font = 3)
                        
#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)
                        
#add points minT
points(data_split[[i]]$minTslope_C, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$minTp_value_C_colour,
       cex = data_split[[i]]$rangeSize_bin)
                        

#add lines minT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$minT_minVals_C[j],
              data_split[[i]]$minT_maxVals_C[j]),
        y = c(j, j),
        lwd = 2, col = data_split[[i]]$minTp_value_C_colour[j])
                          
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$minT_minVals_C[j],
              data_split[[i]]$minT_minVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$minTp_value_C_colour[j])
                          
  lines(x = c(data_split[[i]]$minT_maxVals_C[j],
              data_split[[i]]$minT_maxVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$minTp_value_C_colour[j])
}

#add points meanT
points(data_split[[i]]$meanTslope_C, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$meanTp_value_C_colour,
       cex = data_split[[i]]$rangeSize_bin)
                        
                        
#add lines meanT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$meanT_minVals_C[j],
              data_split[[i]]$meanT_maxVals_C[j]),
        y = c(j, j),
        lwd = 2, col = data_split[[i]]$meanTp_value_C_colour[j])
                          
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$meanT_minVals_C[j],
              data_split[[i]]$meanT_minVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$meanTp_value_C_colour[j])
                          
   lines(x = c(data_split[[i]]$meanT_maxVals_C[j],
               data_split[[i]]$meanT_maxVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$meanTp_value_C_colour[j])
}
                        
                        
#add points maxT
points(data_split[[i]]$maxTslope_C, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$maxTp_value_C_colour,
       cex = data_split[[i]]$rangeSize_bin)
                        
                        
#add lines maxT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$maxT_minVals_C[j],
              data_split[[i]]$maxT_maxVals_C[j]),
        y = c(j, j),
        lwd = 2, col = data_split[[i]]$maxTp_value_C_colour[j])
                          
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$maxT_minVals_C[j],
              data_split[[i]]$maxT_minVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$maxTp_value_C_colour[j])
                          
  lines(x = c(data_split[[i]]$maxT_maxVals_C[j],
              data_split[[i]]$maxT_maxVals_C[j]),
        y = c(j - 0.2, j + 0.2),
        lwd = 2, col = data_split[[i]]$maxTp_value_C_colour[j])
}



#..............................................................................#
#..............................................................................#
#...............................PRECIPITATION..................................#
#..............................................................................#
#..............................................................................#   

#read table
setwd(wd_tables)
res_prec<- read.csv('Precipitation_Rel_Polar_all_points.csv')


################################################################################
#########################                        ###############################
#########################      POLARWARDNESS     ###############################
#########################                        ###############################
################################################################################


#classify rows by p-value bins for the minPPT
res_prec3 <-res_prec
res_prec3$minPPTp_value_RP_bin <- NA

res_prec3$minPPTp_value_RP_bin[res_prec3$minPPTp_value_RP >= 0.1] <- 1
res_prec3$minPPTp_value_RP_bin[res_prec3$minPPTp_value_RP < 0.1 &
                               res_prec3$minPPTp_value_RP >= 0.05] <- 2
res_prec3$minPPTp_value_RP_bin[res_prec3$minPPTp_value_RP < 0.05 &
                               res_prec3$minPPTp_value_RP >= 0.01] <- 3
res_prec3$minPPTp_value_RP_bin[res_prec3$minPPTp_value_RP < 0.01 &
                               res_prec3$minPPTp_value_RP >= 0.001] <- 4
res_prec3$minPPTp_value_RP_bin[res_prec3$minPPTp_value_RP < 0.001] <- 5

#make a colour column based on the min p-value
res_prec3$minPPTp_value_RP_colour <- 'white'  

res_prec3$minPPTp_value_RP_colour[res_prec3$minPPTp_value_RP_bin == 1] <- '#feedde'
res_prec3$minPPTp_value_RP_colour[res_prec3$minPPTp_value_RP_bin == 2] <- '#fdbe85'
res_prec3$minPPTp_value_RP_colour[res_prec3$minPPTp_value_RP_bin == 3] <- '#fd8d3c'
res_prec3$minPPTp_value_RP_colour[res_prec3$minPPTp_value_RP_bin == 4] <- '#e6550d'
res_prec3$minPPTp_value_RP_colour[res_prec3$minPPTp_value_RP_bin == 5] <- '#e3350c'
          
#classify rows by p-value bins for the meanPPT
res_prec3$meanPPTp_value_RP_bin <- NA
        
res_prec3$meanPPTp_value_RP_bin[res_prec3$meanPPTp_value_RP >= 0.1] <- 1
res_prec3$meanPPTp_value_RP_bin[res_prec3$meanPPTp_value_RP < 0.1 &
                                res_prec3$meanPPTp_value_RP >= 0.05] <- 2
res_prec3$meanPPTp_value_RP_bin[res_prec3$meanPPTp_value_RP < 0.05 &
                                res_prec3$meanPPTp_value_RP >= 0.01] <- 3
res_prec3$meanPPTp_value_RP_bin[res_prec3$meanPPTp_value_RP < 0.01 &
                                res_prec3$meanPPTp_value_RP >= 0.001] <- 4
res_prec3$meanPPTp_value_RP_bin[res_prec3$meanPPTp_value_RP < 0.001] <- 5
        
#make a colour column based on the mean p-value
res_prec3$meanPPTp_value_RP_colour <- 'white'

res_prec3$meanPPTp_value_RP_colour[res_prec3$meanPPTp_value_RP_bin == 1] <- '#ffffd4'
res_prec3$meanPPTp_value_RP_colour[res_prec3$meanPPTp_value_RP_bin == 2] <- '#fed98e'
res_prec3$meanPPTp_value_RP_colour[res_prec3$meanPPTp_value_RP_bin == 3] <- '#da8050'
res_prec3$meanPPTp_value_RP_colour[res_prec3$meanPPTp_value_RP_bin == 4] <- '#993404'
res_prec3$meanPPTp_value_RP_colour[res_prec3$meanPPTp_value_RP_bin == 5] <- '#662506'
                  
#classify rows by p-value bins for the maxPPT
res_prec3$maxPPTp_value_RP_bin <- NA
                
res_prec3$maxPPTp_value_RP_bin[res_prec3$maxPPTp_value_RP >= 0.1] <- 1
res_prec3$maxPPTp_value_RP_bin[res_prec3$maxPPTp_value_RP < 0.1 &
                               res_prec3$maxPPTp_value_RP >= 0.05] <- 2
res_prec3$maxPPTp_value_RP_bin[res_prec3$maxPPTp_value_RP < 0.05 &
                               res_prec3$meanPPTp_value_RP >= 0.01] <- 3
res_prec3$maxPPTp_value_RP_bin[res_prec3$maxPPTp_value_RP < 0.01 &
                               res_prec3$meanPPTp_value_RP >= 0.001] <- 4
res_prec3$maxPPTp_value_RP_bin[res_prec3$maxPPTp_value_RP < 0.001] <- 5
                
#make a colour column based on the mean p-value
res_prec3$maxPPTp_value_RP_colour <- 'white'

res_prec3$maxPPTp_value_RP_colour[res_prec3$maxPPTp_value_RP_bin == 1] <- '#edf8e9'
res_prec3$maxPPTp_value_RP_colour[res_prec3$maxPPTp_value_RP_bin == 2] <- '#bae4b3'
res_prec3$maxPPTp_value_RP_colour[res_prec3$maxPPTp_value_RP_bin == 3] <- '#74c476'
res_prec3$maxPPTp_value_RP_colour[res_prec3$maxPPTp_value_RP_bin == 4] <- '#31a354'
res_prec3$maxPPTp_value_RP_colour[res_prec3$maxPPTp_value_RP_bin == 5] <- '#006d2c'
                          
#classify species by latitudinal amplitude
res_prec3$latAmplitude <- res_prec3$maxLat - res_prec3$minLat
res_prec3$latAmplitude_bin <- NA
                        
res_prec3$latAmplitude_bin[res_prec3$latAmplitude  < 1] <- 1
                        
res_prec3$latAmplitude_bin[res_prec3$latAmplitude >= 1 &
                           res_prec3$latAmplitude < 5] <- 2
                        
res_prec3$latAmplitude_bin[res_prec3$latAmplitude >= 5 &
                           res_prec3$latAmplitude < 10] <- 3

res_prec3$latAmplitude_bin[res_prec3$latAmplitude >=  10] <- 4
                        
#order the species from highest to lowest slope of min T
res_prec4 <- res_prec3[order(res_prec3$minPPTslope_RP),] #order for the plot
                        
#get the xlim for the plot
res_prec4$minPPT_minVals <- res_prec4$minPPTslope_RP - res_prec4$minPPTSE_RP
res_prec4$minPPT_maxVals <- res_prec4$minPPTslope_RP + res_prec4$minPPTSE_RP
                        
res_prec4$meanPPT_minVals <- res_prec4$meanPPTslope_RP - res_prec4$meanPPTSE_RP
res_prec4$meanPPT_maxVals <- res_prec4$meanPPTslope_RP + res_prec4$meanPPTSE_RP
                        
res_prec4$maxPPT_minVals <- res_prec4$maxPPTslope_RP - res_prec4$maxPPTSE_RP
res_prec4$maxPPT_maxVals <- res_prec4$maxPPTslope_RP + res_prec4$maxPPTSE_RP
                        
x_lim <- c(res_prec4$minPPT_minVals, res_prec4$minPPT_maxVals,
           res_prec4$meanPPT_minVals, res_prec4$meanPPT_maxVals,
           res_prec4$maxPPT_minVals, res_prec4$maxPPT_maxVals)
                        
                        
#### plot 20 species per plot ####
data_split <- list()
                        
data_split[[1]] <- res_prec4[c(1:20), ]
data_split[[2]] <- res_prec4[c(21:40), ]
data_split[[3]] <- res_prec4[c(41:60), ]
data_split[[4]] <- res_prec4[c(61:80), ]
data_split[[5]] <- res_prec4[c(81:100), ]
data_split[[6]] <- res_prec4[c(101:120), ]
data_split[[7]] <- res_prec4[c(121:140), ]
data_split[[8]] <- res_prec4[c(141:160), ]
data_split[[9]] <- res_prec4[c(161:180), ]
data_split[[10]] <- res_prec4[c(181:200), ]
data_split[[11]] <- res_prec4[c(201:220), ]
data_split[[12]] <- res_prec4[c(221:240), ]
data_split[[13]] <- res_prec4[c(241:260), ]
data_split[[14]] <- res_prec4[c(261:280), ]
data_split[[15]] <- res_prec4[c(281:300), ]
data_split[[16]] <- res_prec4[c(301:320), ]
data_split[[17]] <- res_prec4[c(321:340), ]
data_split[[18]] <- res_prec4[c(341:360), ]
data_split[[19]] <- res_prec4[c(361:374), ]
                        
#### PLOT ####
i=4
                        
#make an empty plot
par(mar = c(5,10,1,1))
                        
plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
                             xlim = c(-2, 2),
                             ylim = c(0, nrow(data_split[[i]]))) 
                        
#add X axis
axis(side = 1, 
     at = seq(-2, 2, 2),
     cex.axis = 1, padj = 0, las =1)
                        
#add X axis
axis(side = 2, 
     at = c(1:nrow(data_split[[i]])),
     labels = data_split[[i]]$species,
     cex.axis = 0.8, padj = 0, las = 2, font = 3)
                        
#add a dashed line to show 0
abline(v = 0, lty = 'dotted', lwd = 2)
                        
#add points minPPT
points(data_split[[i]]$minPPTslope_RP, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$minPPTp_value_RP_colour,
       cex = data_split[[i]]$latAmplitude_bin)
                        
#add lines minPPT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$minPPT_minVals[j],
              data_split[[i]]$minPPT_maxVals[j]),
              y = c(j, j),
              lwd = 2, col = data_split[[i]]$minPPTp_value_RP_colour[j])
                          
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$minPPT_minVals[j],
              data_split[[i]]$minPPT_minVals[j]),
              y = c(j - 0.2, j + 0.2),
              lwd = 2, col = data_split[[i]]$minPPTp_value_RP_colour[j])
                          
  lines(x = c(data_split[[i]]$minPPT_maxVals[j],
              data_split[[i]]$minPPT_maxVals[j]),
              y = c(j - 0.2, j + 0.2),
              lwd = 2, col = data_split[[i]]$minPPTp_value_RP_colour[j])
}
                        
#add points meanPPT
points(data_split[[i]]$meanPPTslope_RP, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$meanPPTp_value_RP_colour,
       cex = data_split[[i]]$latAmplitude_bin)
                        
#add lines meanPPT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$meanPPT_minVals[j],
              data_split[[i]]$meanPPT_maxVals[j]),
              y = c(j, j),
              lwd = 2, col = data_split[[i]]$meanPPTp_value_RP_colour[j])
                          
   #make ticks in the ends of the lines
   lines(x = c(data_split[[i]]$meanPPT_minVals[j],
               data_split[[i]]$meanPPT_minVals[j]),
               y = c(j - 0.2, j + 0.2),
               lwd = 2, col = data_split[[i]]$meanPPTp_value_RP_colour[j])
                          
   lines(x = c(data_split[[i]]$meanPPT_maxVals[j],
               data_split[[i]]$meanPPT_maxVals[j]),
               y = c(j - 0.2, j + 0.2),
               lwd = 2, col = data_split[[i]]$meanPPTp_value_RP_colour[j])
}
                        
                        
#add points maxPPT
points(data_split[[i]]$maxPPTslope_RP, c(1:nrow(data_split[[i]])), pch = 19,
       col = data_split[[i]]$maxPPTp_value_RP_colour,
       cex = data_split[[i]]$latAmplitude_bin)
                        
                        
#add lines maxPPT
for(j in 1:nrow(data_split[[i]]))
{
  #make horizontal lines
  lines(x = c(data_split[[i]]$maxPPT_minVals[j],
              data_split[[i]]$maxPPT_maxVals[j]),
              y = c(j, j),
              lwd = 2, col = data_split[[i]]$maxPPTp_value_RP_colour[j])
                          
  #make ticks in the ends of the lines
  lines(x = c(data_split[[i]]$maxPPT_minVals[j],
              data_split[[i]]$maxPPT_minVals[j]),
              y = c(j - 0.2, j + 0.2),
              lwd = 2, col = data_split[[i]]$maxPPTp_value_RP_colour[j])
                          
  lines(x = c(data_split[[i]]$maxPPT_maxVals[j],
              data_split[[i]]$maxPPT_maxVals[j]),
              y = c(j - 0.2, j + 0.2),
              lwd = 2, col = data_split[[i]]$maxPPTp_value_RP_colour[j])
}
                        
                        
                      
################################################################################
#########################                        ###############################
#########################      CENTRALNESS       ###############################
#########################                        ###############################
################################################################################

                        
                        
                        #classify rows by p-value bins for the minT
                        res_temp3 <-res_temp
                        res_temp3$minTp_value_C_bin <- NA
                        
                        res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C >= 0.1] <- 1
                        res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.1 &
                                                      res_temp3$minTp_value_C >= 0.05] <- 2
                        res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.05 &
                                                      res_temp3$minTp_value_C >= 0.01] <- 3
                        res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.01 &
                                                      res_temp3$minTp_value_C >= 0.001] <- 4
                        res_temp3$minTp_value_C_bin[res_temp3$minTp_value_C < 0.001] <- 5
                        
                        #make a colour column based on the min p-value
                        res_temp3$minTp_value_C_colour <- 'white'
                          
                        res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 1] <- '#f1eef6'
                          res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 2] <- '#bdc9e1'
                            res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 3] <- '#74a9cf'
                              res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 4] <- '#2b8cbe'
                                res_temp3$minTp_value_C_colour[res_temp3$minTp_value_C_bin == 5] <- '#045a8d'
                                  
                                #classify rows by p-value bins for the meanT
                                res_temp3$meanTp_value_C_bin <- NA
                                
                                res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C >= 0.1] <- 1
                                res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.1 &
                                                               res_temp3$meanTp_value_C >= 0.05] <- 2
                                res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.05 &
                                                               res_temp3$meanTp_value_C >= 0.01] <- 3
                                res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.01 &
                                                               res_temp3$meanTp_value_C >= 0.001] <- 4
                                res_temp3$meanTp_value_C_bin[res_temp3$meanTp_value_C < 0.001] <- 5
                                
                                #make a colour column based on the mean p-value
                                res_temp3$meanTp_value_C_colour <- 'white'
                                  
                                res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 1] <- '#f2f0f7'
                                  res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 2] <- '#cbc9e2'
                                    res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 3] <- '#9e9ac8'
                                      res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 4] <- '#756bb1'
                                        res_temp3$meanTp_value_C_colour[res_temp3$meanTp_value_C_bin == 5] <- '#54278f'
                                          
                                        #classify rows by p-value bins for the maxT
                                        res_temp3$maxTp_value_C_bin <- NA
                                        
                                        res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C >= 0.1] <- 1
                                        res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.1 &
                                                                      res_temp3$maxTp_value_C >= 0.05] <- 2
                                        res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.05 &
                                                                      res_temp3$maxTp_value_C >= 0.01] <- 3
                                        res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.01 &
                                                                      res_temp3$maxTp_value_C >= 0.001] <- 4
                                        res_temp3$maxTp_value_C_bin[res_temp3$maxTp_value_C < 0.001] <- 5
                                        
                                        res_temp3$maxTp_value_C[which(res_temp3$maxTp_value_C < 0.001)] 
                                        
                                        #make a colour column based on the mean p-value
                                        res_temp3$maxTp_value_C_colour <- 'white'
                                          
                                        res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 1] <- '#fee5d9'
                                          res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 2] <- '#fcae91'
                                            res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 3] <- '#fb6a4a'
                                              res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 4] <- '#de2d26'
                                                res_temp3$maxTp_value_C_colour[res_temp3$maxTp_value_C_bin == 5] <- '#a50f15'
                                                  
                                                #classify species by range size (try also including roundness???)
                                                res_temp3$rangeSize_bin <- NA
                                                
                                                res_temp3$rangeSize_bin[res_temp3$rangeSize < 100000] <- 1
                                                
                                                res_temp3$rangeSize_bin[res_temp3$rangeSize >= 100000 &
                                                                          res_temp3$rangeSize < 500000] <- 2
                                                
                                                res_temp3$rangeSize_bin[res_temp3$rangeSize >= 500000 &
                                                                          res_temp3$rangeSize < 1000000] <- 3
                                                
                                                res_temp3$rangeSize_bin[res_temp3$rangeSize >=  1000000] <- 4
                                                
                                                #order the species from highest to lowest slope of min T
                                                res_temp4 <- res_temp3[order(res_temp3$minTslope_C),] #order for the plot
                                                
                                                #get the xlim for the plot
                                                res_temp4$minT_minVals_C <- res_temp4$minTslope_C - res_temp4$minTSE_C
                                                res_temp4$minT_maxVals_C <- res_temp4$minTslope_C + res_temp4$minTSE_C
                                                
                                                res_temp4$meanT_minVals_C <- res_temp4$meanTslope_C - res_temp4$meanTSE_C
                                                res_temp4$meanT_maxVals_C <- res_temp4$meanTslope_C + res_temp4$meanTSE_C
                                                
                                                res_temp4$maxT_minVals_C <- res_temp4$maxTslope_C - res_temp4$maxTSE_C
                                                res_temp4$maxT_maxVals_C <- res_temp4$maxTslope_C + res_temp4$maxTSE_C
                                                
                                                x_lim <- c(res_temp4$maxT_minVals_C, res_temp4$maxT_minVals_C,
                                                           res_temp4$maxT_minVals_C, res_temp4$maxT_minVals_C,
                                                           res_temp4$maxT_minVals_C, res_temp4$maxT_minVals_C)
                                                
                                                #### plot 20 species per plot ####
                                                data_split <- list()
                                                
                                                data_split[[1]] <- res_temp4[c(1:20), ]
                                                data_split[[2]] <- res_temp4[c(21:40), ]
                                                data_split[[3]] <- res_temp4[c(41:60), ]
                                                data_split[[4]] <- res_temp4[c(61:80), ]
                                                data_split[[5]] <- res_temp4[c(81:100), ]
                                                data_split[[6]] <- res_temp4[c(101:120), ]
                                                data_split[[7]] <- res_temp4[c(121:140), ]
                                                data_split[[8]] <- res_temp4[c(141:160), ]
                                                data_split[[9]] <- res_temp4[c(161:180), ]
                                                data_split[[10]] <- res_temp4[c(181:200), ]
                                                data_split[[11]] <- res_temp4[c(201:220), ]
                                                data_split[[12]] <- res_temp4[c(221:240), ]
                                                data_split[[13]] <- res_temp4[c(241:260), ]
                                                data_split[[14]] <- res_temp4[c(261:280), ]
                                                data_split[[15]] <- res_temp4[c(281:300), ]
                                                data_split[[16]] <- res_temp4[c(301:320), ]
                                                data_split[[17]] <- res_temp4[c(321:340), ]
                                                data_split[[18]] <- res_temp4[c(341:360), ]
                                                data_split[[19]] <- res_temp4[c(361:374), ]
                                                
                                                #### PLOT ####
                                                i=19
                                                
                                                #make an empty plot
                                                par(mar = c(5,10,1,1))
                                                
                                                plot(1, type = "n", xlab = "", ylab = "", yaxt = 'n', xaxt = 'n', frame = F,
                                                     xlim = c(-1, 1),
                                                     ylim = c(0, nrow(data_split[[i]]))) 
                                                
                                                #add X axis
                                                axis(side = 1, 
                                                     at = seq(-1, 1, 0.5),
                                                     cex.axis = 1, padj = 0, las =1)
                                                
                                                #add X axis
                                                axis(side = 2, 
                                                     at = c(1:nrow(data_split[[i]])),
                                                     labels = data_split[[i]]$species,
                                                     cex.axis = 0.8, padj = 0, las = 2, font = 3)
                                                
                                                #add a dashed line to show 0
                                                abline(v = 0, lty = 'dotted', lwd = 2)
                                                
                                                #add points minT
                                                points(data_split[[i]]$minTslope_C, c(1:nrow(data_split[[i]])), pch = 19,
                                                       col = data_split[[i]]$minTp_value_C_colour,
                                                       cex = data_split[[i]]$rangeSize_bin)
                                                
                                                
                                                #add lines minT
                                                for(j in 1:nrow(data_split[[i]]))
                                                {
                                                  #make horizontal lines
                                                  lines(x = c(data_split[[i]]$minT_minVals_C[j],
                                                              data_split[[i]]$minT_maxVals_C[j]),
                                                        y = c(j, j),
                                                        lwd = 2, col = data_split[[i]]$minTp_value_C_colour[j])
                                                  
                                                  #make ticks in the ends of the lines
                                                  lines(x = c(data_split[[i]]$minT_minVals_C[j],
                                                              data_split[[i]]$minT_minVals_C[j]),
                                                        y = c(j - 0.2, j + 0.2),
                                                        lwd = 2, col = data_split[[i]]$minTp_value_C_colour[j])
                                                  
                                                  lines(x = c(data_split[[i]]$minT_maxVals_C[j],
                                                              data_split[[i]]$minT_maxVals_C[j]),
                                                        y = c(j - 0.2, j + 0.2),
                                                        lwd = 2, col = data_split[[i]]$minTp_value_C_colour[j])
                                                }
                                                
                                                #add points meanT
                                                points(data_split[[i]]$meanTslope_C, c(1:nrow(data_split[[i]])), pch = 19,
                                                       col = data_split[[i]]$meanTp_value_C_colour,
                                                       cex = data_split[[i]]$rangeSize_bin)
                                                
                                                
                                                #add lines meanT
                                                for(j in 1:nrow(data_split[[i]]))
                                                {
                                                  #make horizontal lines
                                                  lines(x = c(data_split[[i]]$meanT_minVals_C[j],
                                                              data_split[[i]]$meanT_maxVals_C[j]),
                                                        y = c(j, j),
                                                        lwd = 2, col = data_split[[i]]$meanTp_value_C_colour[j])
                                                  
                                                  #make ticks in the ends of the lines
                                                  lines(x = c(data_split[[i]]$meanT_minVals_C[j],
                                                              data_split[[i]]$meanT_minVals_C[j]),
                                                        y = c(j - 0.2, j + 0.2),
                                                        lwd = 2, col = data_split[[i]]$meanTp_value_C_colour[j])
                                                  
                                                  lines(x = c(data_split[[i]]$meanT_maxVals_C[j],
                                                              data_split[[i]]$meanT_maxVals_C[j]),
                                                        y = c(j - 0.2, j + 0.2),
                                                        lwd = 2, col = data_split[[i]]$meanTp_value_C_colour[j])
                                                }
                                                
                                                
                                                #add points maxT
                                                points(data_split[[i]]$maxTslope_C, c(1:nrow(data_split[[i]])), pch = 19,
                                                       col = data_split[[i]]$maxTp_value_C_colour,
                                                       cex = data_split[[i]]$rangeSize_bin)
                                                
                                                
                                                #add lines maxT
                                                for(j in 1:nrow(data_split[[i]]))
                                                {
                                                  #make horizontal lines
                                                  lines(x = c(data_split[[i]]$maxT_minVals_C[j],
                                                              data_split[[i]]$maxT_maxVals_C[j]),
                                                        y = c(j, j),
                                                        lwd = 2, col = data_split[[i]]$maxTp_value_C_colour[j])
                                                  
                                                  #make ticks in the ends of the lines
                                                  lines(x = c(data_split[[i]]$maxT_minVals_C[j],
                                                              data_split[[i]]$maxT_minVals_C[j]),
                                                        y = c(j - 0.2, j + 0.2),
                                                        lwd = 2, col = data_split[[i]]$maxTp_value_C_colour[j])
                                                  
                                                  lines(x = c(data_split[[i]]$maxT_maxVals_C[j],
                                                              data_split[[i]]$maxT_maxVals_C[j]),
                                                        y = c(j - 0.2, j + 0.2),
                                                        lwd = 2, col = data_split[[i]]$maxTp_value_C_colour[j])
                                                }
                                                

