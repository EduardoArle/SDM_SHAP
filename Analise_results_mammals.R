#load libraries
library(metafor)

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'

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
     labels = data_split[[i]]$species, cex.axis = 0.8, padj = 0, las = 2, font = 3)

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
                             labels = data_split[[i]]$species, cex.axis = 0.8, padj = 0, las = 2, font = 3)
                        
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
