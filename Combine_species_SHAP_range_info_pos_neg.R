####  This script combines the SHAP results for each species with the info
####  about range characteristics and position of points within the ranges

#load libraries

#list wds
wd_res_shap <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_pts_measure <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Point_and_range_measurements'
wd_res <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'


#list species 
setwd(wd_pts_measure)
sps_list <- gsub('_point_range_metrics.csv', '', list.files())

#load all species measurements
setwd(wd_pts_measure)
sps_measures <- lapply(list.files(), read.csv)
names(sps_measures) <- sps_list

#identify only the species that do not cross the equator
colNamesSps <- lapply(sps_measures, names)
cross_eq <- sapply(colNamesSps, function(x){'NOTE' %in% x})
sps_list_sel <- names(cross_eq)[cross_eq == FALSE]

#select measurements for species not crossing the equator
setwd(wd_pts_measure)
sps_m_sel <- lapply(paste0(sps_list_sel,'_point_range_metrics.csv'), read.csv)
names(sps_m_sel) <- sps_list_sel



##############################################
############## EACH SPECIES TEMP #############
##############################################


## Load results for T variables considering all mono-hemisferic species
setwd(wd_res_shap)

#minT
sps_minT <- lapply(sps_list_sel, function(x){read.csv(paste0(x,'_minT_.csv'))})
names(sps_m_sel) <- sps_list_sel

#meanT
sps_meanT <- lapply(sps_list_sel, function(x){read.csv(paste0(x,'_meanT_.csv'))})
names(sps_m_sel) <- sps_list_sel

#maxT
sps_maxT <- lapply(sps_list_sel, function(x){read.csv(paste0(x,'_maxT_.csv'))})
names(sps_m_sel) <- sps_list_sel


## Select only rows representing presence
sps_minT_pr <- lapply(sps_minT, function(x){x[x$Occurrence == 1,]}) #minT
sps_meanT_pr <- lapply(sps_meanT, function(x){x[x$Occurrence == 1,]}) #meanT
sps_maxT_pr <- lapply(sps_maxT, function(x){x[x$Occurrence == 1,]}) #maxT


## Concatenate point and range info to SHAP results

#minT
sps_minT_SHAP_info <- list()
for(j in 1:length(sps_minT_pr))
{
  sps_minT_SHAP_info[[j]] <- cbind(sps_minT_pr[[j]], 
                                  sps_m_sel[[j]][,c('key','datasetKey',
                                  'rangeSize','roundness','centralness',     
                                  'absPolarwardness','relPolarwardness')])
}

#meanT
sps_meanT_SHAP_info <- list()
for(j in 1:length(sps_meanT_pr))
{
  sps_meanT_SHAP_info[[j]] <- cbind(sps_meanT_pr[[j]], 
                                  sps_m_sel[[j]][,c('key','datasetKey',
                                  'rangeSize','roundness','centralness',     
                                  'absPolarwardness','relPolarwardness')])
}

#maxT
sps_maxT_SHAP_info <- list()
for(j in 1:length(sps_maxT_pr))
{
  sps_maxT_SHAP_info[[j]] <- cbind(sps_maxT_pr[[j]], 
                                  sps_m_sel[[j]][,c('key','datasetKey',
                                  'rangeSize','roundness','centralness',     
                                  'absPolarwardness','relPolarwardness')])
}

#create vectors to store the results for each species
species <- character()
minTR2_RP <- numeric()
meanTR2_RP <- numeric()
maxTR2_RP <- numeric()
minTR2_C <- numeric()
meanTR2_C <- numeric()
maxTR2_C <- numeric()
rangeSize <- numeric()
rangeRoundness <- numeric()
maxLat <- numeric()
minLat <- numeric()
  
## Loop through species
for(j in 1:length(sps_list_sel))
{
  #skip sps with only one point
  if(j != 28){
    #list spp
    species[length(species) + 1] <- sps_list_sel[j]
    
    ## Create a folder for the species results
    wd_sps <- paste0(wd_res, '/', gsub(' ', '_', sps_list_sel[j]))
    
    ### TEST ### turn on after
    dir.create(wd_sps)
    
    ## Calculate percentage contribution of the variables (no negatives)
    
    #minT
    cont_minT <- sps_minT_SHAP_info[[j]][,9]
    
    #meanT
    cont_meanT <- sps_meanT_SHAP_info[[j]][,9]
    
    #maxT
    cont_maxT <- sps_maxT_SHAP_info[[j]][,9]
    
    ## Save plot temperature variables contribution per relative polarwardness
    
    #### TEST #### change wd after
    #setwd(wd_res)
    setwd(wd_sps)
    pdf(file = paste0(gsub(' ', '_', sps_list_sel[j]),'_Temperature_RelPolar.pdf'))
    
    #set parametres for plotting
    par(mar = c(5,5,5,5))
    
    #get y and x lims
    ylim <- range(c(cont_minT, cont_meanT, cont_maxT))
    xlim <- range(sps_minT_SHAP_info[[j]]$relPolarwardness) #does not matter which I use
    
    #minT
    plot(sps_minT_SHAP_info[[j]]$relPolarwardness, cont_minT, 
         pch = 19, cex = 0.4, col = '#0000FF15',
         ylab = 'Temperature contribution',
         xlab = 'Relative polarwardness',
         ylim = c(ylim[1], ylim[2]))
    
    #fit linear model
    lin_mod_minT <- lm(cont_minT ~ sps_minT_SHAP_info[[j]]$relPolarwardness)
    abline(lin_mod_minT, col = '#0000FF', lwd = 2)
    
    #get and plot R^2
    r2 <- summary(lin_mod_minT)$r.squared
    text(xlim[1] + ((xlim[2] - xlim[1]) / 20), 
         ylim[1], 
         expression(bold(paste('R'^2*' = '), sep='')), col = '#0000FF', pos = 4)
    text(xlim[1] + ((xlim[2] - xlim[1]) / 8),
         ylim[1] - ((ylim[2] - ylim[1]) / 300), 
         round(r2,3), col = '#0000FF', font = 2, pos = 4)
    
    #populate vector
    minTR2_RP[length(minTR2_RP) + 1] <- r2
    
    #meanT
    points(sps_meanT_SHAP_info[[j]]$relPolarwardness, cont_meanT, 
           pch = 19, cex = 0.4, col = '#80008015')
    
    #fit linear model
    lin_mod_meanT <- lm(cont_meanT ~ sps_meanT_SHAP_info[[j]]$relPolarwardness)
    abline(lin_mod_meanT, col = '#800080', lwd = 2)
    
    #get and plot R^2
    r2 <- summary(lin_mod_meanT)$r.squared
    text(xlim[1] + ((xlim[2] - xlim[1]) / 20), 
         ylim[1] + ((ylim[2] - ylim[1]) / 22), 
         expression(bold(paste('R'^2*' = '), sep='')), col = '#800080', pos = 4)
    text(xlim[1] + ((xlim[2] - xlim[1]) / 8),
         ylim[1] + ((ylim[2] - ylim[1]) / 22) - ((ylim[2] - ylim[1]) / 300), 
         round(r2,3), col = '#800080', font = 2, pos = 4)
    
    #pupulate vector
    meanTR2_RP[length(meanTR2_RP) + 1] <- r2
    
    #maxT
    points(sps_maxT_SHAP_info[[j]]$relPolarwardness, cont_maxT, 
           pch = 19, cex = 0.4, col = '#FF000015')
    
    #fit linear model
    lin_mod_maxT <- lm(cont_maxT ~ sps_maxT_SHAP_info[[j]]$relPolarwardness)
    abline(lin_mod_maxT, col = '#FF0000', lwd = 2)
    
    #get and plot R^2
    r2 <- summary(lin_mod_maxT)$r.squared
    text(xlim[1] + ((xlim[2] - xlim[1]) / 20), 
         ylim[1] + 2 * ((ylim[2] - ylim[1]) / 22), 
         expression(bold(paste('R'^2*' = '), sep='')), col = '#FF0000', pos = 4)
    text(xlim[1] + ((xlim[2] - xlim[1]) / 8),
         ylim[1] + 2 * ((ylim[2] - ylim[1]) / 22) - ((ylim[2] - ylim[1]) / 300), 
         round(r2,3), col = '#FF0000', font = 2, pos = 4)
    
    
    #pupulate vector
    maxTR2_RP[length(maxTR2_RP) + 1] <- r2
    
    #plot species range size and roundness
    text(xlim[2] - ((xlim[2] - xlim[1]) / 4.5), 
         ylim[2] - ((ylim[2] - ylim[1]) / 25), 
         'Range', font = 2, cex = 0.8, pos = 4)
    text(xlim[2] - ((xlim[2] - xlim[1]) / 4.5),
         ylim[2] - 2 * ((ylim[2] - ylim[1]) / 25), 
         paste0('Size  ', unique(round(sps_maxT_SHAP_info[[j]]$rangeSize))), 
         cex = 0.7, pos = 4)
    text(xlim[2] - ((xlim[2] - xlim[1]) / 4.5),
         ylim[2] - 2.8 * ((ylim[2] - ylim[1]) / 25),
         paste0('Roundness  ',
                unique(round(sps_maxT_SHAP_info[[j]]$roundness, 2))),
         cex = 0.7, pos = 4)
    text(xlim[2] - ((xlim[2] - xlim[1]) / 4.5),
         ylim[2] - 3.6 * ((ylim[2] - ylim[1]) / 25),
         paste0('Max lat  ',
                    round(max(abs(sps_maxT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    text(xlim[2] - ((xlim[2] - xlim[1]) / 4.5),
         ylim[2] - 4.4 * ((ylim[2] - ylim[1]) / 25),
         paste0('Min lat  ',
                    round(min(abs(sps_minT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    
    #plot species name
    text(xlim[1] + ((xlim[2] - xlim[1]) / 20),
         ylim[2] - ((ylim[2] - ylim[1]) / 25),
         sps_list_sel[j], font = 4, pos = 4)
    
    dev.off()
    
    
    ## Save plot temperature variables contribution per centralness
    
    #### TEST #### change wd after
    #setwd(wd_res)
    setwd(wd_sps)
    pdf(file = paste0(gsub(' ', '_',
                           sps_list_sel[j]),'_Temperature_Centralness.pdf'))
    
    #set parametres for plotting
    par(mar = c(5,5,5,5))
    
    #get y and x lims
    ylim <- range(c(cont_minT, cont_meanT, cont_maxT))
    xlim <- range(sps_minT_SHAP_info_allSpp$centralness) #does not matter which I use
    
    #minT
    plot(sps_minT_SHAP_info[[j]]$centralness, cont_minT, 
         pch = 19, cex = 0.4, col = '#0000FF15',
         ylab = 'Temperature contribution',
         xlab = 'Centralness',
         ylim = c(ylim[1], ylim[2]))
    
    #fit linear model
    lin_mod_minT <- lm(cont_minT ~ sps_minT_SHAP_info[[j]]$centralness)
    abline(lin_mod_minT, col = '#0000FF', lwd = 2)
    
    #get and plot R^2
    r2 <- summary(lin_mod_minT)$r.squared
    text(xlim[1] + ((xlim[2] - xlim[1]) / 20), 
         ylim[1], 
         expression(bold(paste('R'^2*' = '), sep='')), col = '#0000FF', pos = 4)
    text(xlim[1] + ((xlim[2] - xlim[1]) / 8),
         ylim[1] - ((ylim[2] - ylim[1]) / 300), 
         round(r2,3), col = '#0000FF', font = 2, pos = 4)
    
    #get R^2
    r2 <- summary(lin_mod_minT)$r.squared
    text(0, 2, expression(bold(paste('R'^2*' = '), sep='')), col = '#0000FF', 
         pos = 4)
    text(0.08, 1.7, round(r2,3), col = '#0000FF', font = 2, pos = 4)
    
    #populate vector
    minTR2_C[length(minTR2_C) + 1] <- r2
    
    #meanT
    points(sps_meanT_SHAP_info[[j]]$centralness, cont_meanT, 
           pch = 19, cex = 0.4, col = '#80008010')
    
    #fit linear model
    lin_mod_meanT <- lm(cont_meanT ~ sps_meanT_SHAP_info[[j]]$centralness)
    abline(lin_mod_meanT, col = '#800080', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_meanT)$r.squared
    text(0, 6, expression(bold(paste('R'^2*' = '), sep='')), col = '#800080',
         pos = 4)
    text(0.08, 5.7, round(r2,3), col = '#800080', font = 2, pos = 4)
    
    #pupulate vector
    meanTR2_C[length(meanTR2_C) + 1] <- r2
    
    #maxT
    points(sps_maxT_SHAP_info[[j]]$centralness, cont_maxT, 
           pch = 19, cex = 0.4, col = '#FF000010')
    
    #fit linear model
    lin_mod_maxT <- lm(cont_maxT ~ sps_maxT_SHAP_info[[j]]$centralness)
    abline(lin_mod_maxT, col = '#FF0000', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_maxT)$r.squared
    text(0, 10, expression(bold(paste('R'^2*' = '), sep='')), col = '#FF0000',
         pos = 4)
    text(0.08, 9.7, round(r2,3), col = '#FF0000', font = 2, pos = 4)
    
    #pupulate vector
    maxTR2_C[length(maxTR2_C) + 1] <- r2
    
    #plot species range size and roundness
    text(0.78, 97, 'Range', font = 2, cex = 0.8, pos = 4)
    text(0.78, 92, 
         paste0('Size  ', unique(round(sps_maxT_SHAP_info[[j]]$rangeSize))), 
         cex = 0.7, pos = 4)
    text(0.78, 88, paste0('Roundness  ',
                          unique(round(sps_maxT_SHAP_info[[j]]$roundness, 2))),
         cex = 0.7, pos = 4)
    text(0.78, 84, paste0('Max lat  ',
                          round(max(abs(sps_maxT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    text(0.78, 80, paste0('Min lat  ',
                          round(min(abs(sps_minT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    
    #plot species name
    text(0,97, sps_list_sel[j], font = 4, pos = 4)
    
    dev.off()
    
    rangeSize[length(rangeSize) + 1] <- unique(sps_maxT_SHAP_info[[j]]$rangeSize)
    rangeRoundness[length(rangeRoundness) + 1] <- 
      unique(sps_maxT_SHAP_info[[j]]$roundness) 
    maxLat[length(maxLat) + 1] <- max(abs(sps_maxT_SHAP_info[[j]]$decimalLatitude))
    minLat[length(minLat) + 1] <- min(abs(sps_maxT_SHAP_info[[j]]$decimalLatitude))
    
  }
  
  print(j)
}

#create a dataframe with the results for all species
temp_all_pts <- data.frame(species = species, 
                           minTR2_RP = minTR2_RP,
                           meanTR2_RP = meanTR2_RP, 
                           maxTR2_RP = maxTR2_RP,
                           minTR2_C = minTR2_C,
                           meanTR2_C = meanTR2_C, 
                           maxTR2_C = maxTR2_C,
                           rangeSize = rangeSize,
                           rangeRoundness = rangeRoundness,
                           maxLat = maxLat,
                           minLat = minLat)

#save table
setwd(wd_tables)
write.csv(temp_all_pts, 'Temperature_Rel_Polar_all_points.csv', row.names = F)


##############################################################################

###############################################
############## EACH SPECIES PREC ##############
###############################################


## Load results for T variables considering all mono-hemisferic species
setwd(wd_res_shap)

#minPPT
sps_minPPT <- lapply(sps_list_sel, function(x){read.csv(paste0(x,'_minPPT_.csv'))})
names(sps_m_sel) <- sps_list_sel

#meanPPT
sps_meanPPT <- lapply(sps_list_sel, 
                      function(x){read.csv(paste0(x,'_meanPPT_.csv'))})
names(sps_m_sel) <- sps_list_sel

#maxPPT
sps_maxPPT <- lapply(sps_list_sel, function(x){read.csv(paste0(x,'_maxPPT_.csv'))})
names(sps_m_sel) <- sps_list_sel


## Select only rows representing presence
sps_minPPT_pr <- lapply(sps_minPPT, function(x){x[x$Occurrence == 1,]}) #minPPT
sps_meanPPT_pr <- lapply(sps_meanPPT, function(x){x[x$Occurrence == 1,]}) #meanPPT
sps_maxPPT_pr <- lapply(sps_maxPPT, function(x){x[x$Occurrence == 1,]}) #maxPPT


## Concatenate point and range info to SHAP results

#minPPT
sps_minPPT_SHAP_info <- list()
for(j in 1:length(sps_minPPT_pr))
{
  sps_minPPT_SHAP_info[[j]] <- cbind(sps_minPPT_pr[[j]], 
                                   sps_m_sel[[j]][,c('key','datasetKey',
                                  'rangeSize','roundness','centralness',     
                                  'absPolarwardness','relPolarwardness')])
}

#meanPPT
sps_meanPPT_SHAP_info <- list()
for(j in 1:length(sps_meanPPT_pr))
{
  sps_meanPPT_SHAP_info[[j]] <- cbind(sps_meanPPT_pr[[j]], 
                                    sps_m_sel[[j]][,c('key','datasetKey',
                                   'rangeSize','roundness','centralness',     
                                   'absPolarwardness','relPolarwardness')])
}

#maxPPT
sps_maxPPT_SHAP_info <- list()
for(j in 1:length(sps_maxPPT_pr))
{
  sps_maxPPT_SHAP_info[[j]] <- cbind(sps_maxPPT_pr[[j]], 
                                   sps_m_sel[[j]][,c('key','datasetKey',
                                  'rangeSize','roundness','centralness',     
                                  'absPolarwardness','relPolarwardness')])
}


#create vectors to store the results for each species
species <- character()
minPPTR2_RP <- numeric()
meanPPTR2_RP <- numeric()
maxPPTR2_RP <- numeric()
minPPTR2_C <- numeric()
meanPPTR2_C <- numeric()
maxPPTR2_C <- numeric()
rangeSize <- numeric()
rangeRoundness <- numeric()
maxLat <- numeric()
minLat <- numeric()


## Loop through species
for(j in 1:length(sps_list_sel))
{
  #skip sps with only one point
  if(j != 28){
    #list spp
    species[length(species) + 1] <- sps_list_sel[j]
    
    ## Calculate percentage contribution of the variables (no negatives)
    
    ## Inform folder for the species results
    wd_sps <- paste0(wd_res, '/', gsub(' ', '_', sps_list_sel[j]))
    
    #minPPT
    cont_minPPT <- abs(sps_minPPT_SHAP_info[[j]][,9]) / 
      (abs(sps_minPPT_SHAP_info[[j]][,9]) + 
         abs(sps_minPPT_SHAP_info[[j]][,8])) * 100
    
    #meanPPT
    cont_meanPPT <- abs(sps_meanPPT_SHAP_info[[j]][,9]) / 
      (abs(sps_meanPPT_SHAP_info[[j]][,9]) + 
         abs(sps_meanPPT_SHAP_info[[j]][,8])) * 100
    
    #maxPPT
    cont_maxPPT <- abs(sps_maxPPT_SHAP_info[[j]][,9]) / 
      (abs(sps_maxPPT_SHAP_info[[j]][,9]) + 
         abs(sps_maxPPT_SHAP_info[[j]][,8])) * 100
    
    ## Save plot precipitation variables contribution per relative polarwardness
    
    #### TEST #### change wd after
    #setwd(wd_test)
    setwd(wd_sps)
    pdf(file = paste0(gsub(' ', '_', sps_list_sel[j]),
                      '_Precipitation_RelPolar.pdf'))
    
    #set parametres for plotting
    par(mar = c(5,5,5,5))
    
    #minPPT
    plot(sps_minPPT_SHAP_info[[j]]$relPolarwardness, cont_minPPT, 
         pch = 19, cex = 0.4, col = '#fc8d5910',
         ylab = 'Precipitation contribution',
         xlab = 'Relative Polarwardness',
         ylim = c(0,100),
         xlim = c(0,1))
    
    #fit linear model
    lin_mod_minPPT <- lm(cont_minPPT ~ sps_minPPT_SHAP_info[[j]]$relPolarwardness)
    abline(lin_mod_minPPT, col = '#fc8d59', lwd = 2)
     
    #get R^2
    r2 <- summary(lin_mod_minPPT)$r.squared
    text(0, 2, expression(bold(paste('R'^2*' = '), sep='')), col = '#fc8d59',
         pos = 4)
    text(0.08, 1.7, round(r2,3), col = '#ff8d59', font = 2, pos = 4)
    
    #pupulate vector
    minPPTR2_RP[length(minPPTR2_RP) + 1] <- r2
    
    #meanPPT
    points(sps_meanPPT_SHAP_info[[j]]$relPolarwardness, cont_meanPPT, 
           pch = 19, cex = 0.4, col = '#8c510a10')
    
    #fit linear model
    lin_mod_meanPPT <- lm(cont_meanPPT ~ sps_meanPPT_SHAP_info[[j]]$relPolarwardness)
    abline(lin_mod_meanPPT, col = '#8c510a', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_meanPPT)$r.squared
    text(0, 6, expression(bold(paste('R'^2*' = '), sep='')), col = '#8c510a',
         pos = 4)
    text(0.08, 5.7, round(r2,3), col = '#8c510a', font = 2, pos = 4)
    
    #pupulate vector
    meanPPTR2_RP[length(meanPPTR2_RP) + 1] <- r2
    
    #maxPPT
    points(sps_maxPPT_SHAP_info[[j]]$relPolarwardness, cont_maxPPT, 
           pch = 19, cex = 0.4, col = '#1a985010')
    
    #fit linear model
    lin_mod_maxPPT <- lm(cont_maxPPT ~ sps_maxPPT_SHAP_info[[j]]$relPolarwardness)
    abline(lin_mod_maxPPT, col = '#1a9850', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_maxPPT)$r.squared
    text(0, 10, expression(bold(paste('R'^2*' = '), sep='')), col = '#1a9850',
         pos = 4)
    text(0.08, 9.7, round(r2,3), col = '#1a9850', font = 2, pos = 4)
    
    #pupulate vector
    maxPPTR2_RP[length(maxPPTR2_RP) + 1] <- r2
    
    #plot species range size and roundness
    text(0.78, 97, 'Range', font = 2, cex = 0.8, pos = 4)
    text(0.78, 92, 
         paste0('Size  ', unique(round(sps_maxPPT_SHAP_info[[j]]$rangeSize))), 
         cex = 0.7, pos = 4)
    text(0.78, 88, paste0('Roundness  ',
                          unique(round(sps_maxPPT_SHAP_info[[j]]$roundness, 2))),
         cex = 0.7, pos = 4)
    text(0.78, 84, paste0('Max lat  ',
         round(max(abs(sps_maxPPT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    text(0.78, 80, paste0('Min lat  ',
         round(min(abs(sps_minPPT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    
    #plot species name
    text(0,97, sps_list_sel[j], font = 4, pos = 4)
    
    dev.off()
    
    
    ## Save plot precipitation variables contribution per centralness
    
    #### TEST #### change wd after
    #setwd(wd_test)
    setwd(wd_sps)
    pdf(file = paste0(gsub(' ', '_',
                           sps_list_sel[j]),'_Precipitation_Centralness.pdf'))
    
    #set parametres for plotting
    par(mar = c(5,5,5,5))
    
    #minPPT
    plot(sps_minPPT_SHAP_info[[j]]$centralness, cont_minPPT, 
         pch = 19, cex = 0.4, col = '#fc8d5910',
         ylab = 'Precipitation contribution',
         xlab = 'Centralness',
         ylim = c(0,100),
         xlim = c(0,1))
    
    #fit linear model
    lin_mod_minPPT <- lm(cont_minPPT ~ sps_minPPT_SHAP_info[[j]]$centralness)
    abline(lin_mod_minPPT, col = '#fc8d59', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_minPPT)$r.squared
    text(0, 2, expression(bold(paste('R'^2*' = '), sep='')), col = '#fc8d59',
         pos = 4)
    text(0.08, 1.7, round(r2,3), col = '#ff8d59', font = 2, pos = 4)
    
    #pupulate vector
    minPPTR2_C[length(minPPTR2_C) + 1] <- r2
    
    #meanPPT
    points(sps_meanPPT_SHAP_info[[j]]$centralness, cont_meanPPT, 
           pch = 19, cex = 0.4, col = '#8c510a10')
    
    #fit linear model
    lin_mod_meanPPT <- lm(cont_meanPPT ~ sps_meanPPT_SHAP_info[[j]]$centralness)
    abline(lin_mod_meanPPT, col = '#8c510a', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_meanPPT)$r.squared
    text(0, 6, expression(bold(paste('R'^2*' = '), sep='')), col = '#8c510a',
         pos = 4)
    text(0.08, 5.7, round(r2,3), col = '#8c510a', font = 2, pos = 4)
    
    #pupulate vector
    meanPPTR2_C[length(meanPPTR2_C) + 1] <- r2
    
    #maxPPT
    points(sps_maxPPT_SHAP_info[[j]]$centralness, cont_maxPPT, 
           pch = 19, cex = 0.4, col = '#1a985010')
    
    #fit linear model
    lin_mod_maxPPT <- lm(cont_maxPPT ~ sps_maxPPT_SHAP_info[[j]]$centralness)
    abline(lin_mod_maxPPT, col = '#1a9850', lwd = 2)
    
    #get R^2
    r2 <- summary(lin_mod_maxPPT)$r.squared
    text(0, 10, expression(bold(paste('R'^2*' = '), sep='')), col = '#1a9850',
         pos = 4)
    text(0.08, 9.7, round(r2,3), col = '#1a9850', font = 2, pos = 4)
    
    #pupulate vector
    maxPPTR2_C[length(maxPPTR2_C) + 1] <- r2
    
    #plot species range size and roundness
    text(0.78, 97, 'Range', font = 2, cex = 0.8, pos = 4)
    text(0.78, 92, 
         paste0('Size  ', unique(round(sps_maxPPT_SHAP_info[[j]]$rangeSize))), 
         cex = 0.7, pos = 4)
    text(0.78, 88, paste0('Roundness  ',
                          unique(round(sps_maxPPT_SHAP_info[[j]]$roundness, 2))),
         cex = 0.7, pos = 4)
    text(0.78, 84, paste0('Max lat  ',
                round(max(abs(sps_maxPPT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    text(0.78, 80, paste0('Min lat  ',
                 round(min(abs(sps_minPPT_SHAP_info[[j]]$decimalLatitude)), 2)),
         cex = 0.7, pos = 4)
    
    #plot species name
    text(0,97, sps_list_sel[j], font = 4, pos = 4)
    
    dev.off()
    
    rangeSize[length(rangeSize) + 1] <- unique(sps_maxPPT_SHAP_info[[j]]$rangeSize)
    rangeRoundness[length(rangeRoundness) + 1] <- 
      unique(sps_maxPPT_SHAP_info[[j]]$roundness) 
    maxLat[length(maxLat) + 1] <- max(abs(sps_maxPPT_SHAP_info[[j]]$decimalLatitude))
    minLat[length(minLat) + 1] <- min(abs(sps_maxPPT_SHAP_info[[j]]$decimalLatitude))
    
  }
}

#create a dataframe with the results for all species
prec_all_pts <- data.frame(species = species, 
                           minPPTR2_RP = minPPTR2_RP,
                           meanPPTR2_RP = meanPPTR2_RP, 
                           maxPPTR2_RP = maxPPTR2_RP,
                           minPPTR2_C = minPPTR2_C,
                           meanPPTR2_C = meanPPTR2_C, 
                           maxPPTR2_C = maxPPTR2_C,
                           rangeSize = rangeSize,
                           rangeRoundness = rangeRoundness,
                           maxLat = maxLat,
                           minLat = minLat)

#save table
setwd(wd_tables)
write.csv(prec_all_pts, 'Precipitation_Rel_Polar_all_points.csv', row.names = F)


