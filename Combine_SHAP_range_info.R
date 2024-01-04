####  This script combines the SHAP results for each species with the info
####  about range characteristics and position of points within the ranges

#load libraries
library(data.table)

#list wds
wd_res_shap <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_pts_measure <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Point_and_range_measurements'
wd_res <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/All_species_all_points'

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
############## ALL SPECIES TEMP ##############
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


## Bind all rows in one table

#minT
sps_minT_SHAP_info_allSpp <- rbindlist(sps_minT_SHAP_info)
sps_minT_SHAP_info_allSpp <- as.data.frame(sps_minT_SHAP_info_allSpp)

#meanT
sps_meanT_SHAP_info_allSpp <- rbindlist(sps_meanT_SHAP_info)
sps_meanT_SHAP_info_allSpp <- as.data.frame(sps_meanT_SHAP_info_allSpp)

#maxT
sps_maxT_SHAP_info_allSpp <- rbindlist(sps_maxT_SHAP_info)
sps_maxT_SHAP_info_allSpp <- as.data.frame(sps_maxT_SHAP_info_allSpp)


## Calculate percentage contribution of the variables (no negatives)

#minT
cont_minT <- abs(sps_minT_SHAP_info_allSpp[,9]) / 
  (abs(sps_minT_SHAP_info_allSpp[,9]) + abs(sps_minT_SHAP_info_allSpp[,8])) * 100

#meanT
cont_meanT <- abs(sps_meanT_SHAP_info_allSpp[,9]) / 
  (abs(sps_meanT_SHAP_info_allSpp[,9]) + abs(sps_meanT_SHAP_info_allSpp[,8])) * 100

#maxT
cont_maxT <- abs(sps_maxT_SHAP_info_allSpp[,9]) / 
  (abs(sps_maxT_SHAP_info_allSpp[,9]) + abs(sps_maxT_SHAP_info_allSpp[,8])) * 100



## Save plot image temperature variables contribution per absolute polarwardness
setwd(wd_res)
pdf(file = 'AllSpp_Temperature_AbsPolar.pdf')

#set parametres for plotting
par(mar = c(5,5,5,5))

#minT
plot(sps_minT_SHAP_info_allSpp$absPolarwardness, cont_minT, 
     pch = 19, cex = 0.4, col = '#0000FF10',
     ylab = 'Contribution minT',
     xlab = 'Absolute Polarwardness')

#fit linear model
lin_mod_minT <- lm(cont_minT ~ sps_minT_SHAP_info_allSpp$absPolarwardness)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)

#get R^2
r2 <- summary(lin_mod_minT)$r.squared
text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = '#0000FF')
text(0.10, 4.5, round(r2,3), col = '#0000FF', font = 2)


#meanT
points(sps_meanT_SHAP_info_allSpp$absPolarwardness, cont_meanT, 
     pch = 19, cex = 0.4, col = '#80008010')

#fit linear model
lin_mod_meanT <- lm(cont_meanT ~ sps_meanT_SHAP_info_allSpp$absPolarwardness)
abline(lin_mod_meanT, col = '#800080', lwd = 2)

#get R^2
r2 <- summary(lin_mod_meanT)$r.squared
text(0.05, 9, expression(bold(paste('R'^2*' = '), sep='')), col = '#800080')
text(0.10, 8.5, round(r2,3), col = '#800080', font = 2)


#maxT
points(sps_maxT_SHAP_info_allSpp$absPolarwardness, cont_maxT, 
       pch = 19, cex = 0.4, col = '#FF000010')

#fit linear model
lin_mod_maxT <- lm(cont_maxT ~ sps_maxT_SHAP_info_allSpp$absPolarwardness)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)

#get R^2
r2 <- summary(lin_mod_maxT)$r.squared
text(0.05, 13, expression(bold(paste('R'^2*' = '), sep='')), col = '#FF0000')
text(0.10, 12.5, round(r2,3), col = '#FF0000', font = 2)

dev.off()


## Save plot image temperature variables contribution per relative polarwardness
setwd(wd_res)
pdf(file = 'AllSpp_Temperature_RelPolar.pdf')

#set parametres for plotting
par(mar = c(5,5,5,5))

#minT
plot(sps_minT_SHAP_info_allSpp$relPolarwardness, cont_minT, 
     pch = 19, cex = 0.4, col = '#0000FF10',
     ylab = 'Contribution minT',
     xlab = 'Relative Polarwardness')

#fit linear model
lin_mod_minT <- lm(cont_minT ~ sps_minT_SHAP_info_allSpp$relPolarwardness)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)

#get R^2
r2 <- summary(lin_mod_minT)$r.squared
text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = '#0000FF')
text(0.15, 4.5, round(r2,3), col = '#0000FF', font = 2)


#meanT
points(sps_meanT_SHAP_info_allSpp$relPolarwardness, cont_meanT, 
       pch = 19, cex = 0.4, col = '#80008010')

#fit linear model
lin_mod_meanT <- lm(cont_meanT ~ sps_meanT_SHAP_info_allSpp$relPolarwardness)
abline(lin_mod_meanT, col = '#800080', lwd = 2)

#get R^2
r2 <- summary(lin_mod_meanT)$r.squared
text(0.05, 9, expression(bold(paste('R'^2*' = '), sep='')), col = '#800080')
text(0.15, 8.5, round(r2,3), col = '#800080', font = 2)


#maxT
points(sps_maxT_SHAP_info_allSpp$relPolarwardness, cont_maxT, 
       pch = 19, cex = 0.4, col = '#FF000010')

#fit linear model
lin_mod_maxT <- lm(cont_maxT ~ sps_maxT_SHAP_info_allSpp$relPolarwardness)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)

#get R^2
r2 <- summary(lin_mod_maxT)$r.squared
text(0.05, 13, expression(bold(paste('R'^2*' = '), sep='')), col = '#FF0000')
text(0.15, 12.5, round(r2,3), col = '#FF0000', font = 2)

dev.off()


## Save plot image temperature variables contribution per centralness
setwd(wd_res)
pdf(file = 'AllSpp_Temperature_Centralness.pdf')

#set parametres for plotting
par(mar = c(5,5,5,5))

#minT
plot(sps_minT_SHAP_info_allSpp$centralness, cont_minT, 
     pch = 19, cex = 0.4, col = '#0000FF10',
     ylab = 'Contribution minT',
     xlab = 'Centralness')

#fit linear model
lin_mod_minT <- lm(cont_minT ~ sps_minT_SHAP_info_allSpp$centralness)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)

#get R^2
r2 <- summary(lin_mod_minT)$r.squared
text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = '#0000FF')
text(0.15, 4.5, round(r2,3), col = '#0000FF', font = 2)


#meanT
points(sps_meanT_SHAP_info_allSpp$centralness, cont_meanT, 
       pch = 19, cex = 0.4, col = '#80008010')

#fit linear model
lin_mod_meanT <- lm(cont_meanT ~ sps_meanT_SHAP_info_allSpp$centralness)
abline(lin_mod_meanT, col = '#800080', lwd = 2)

#get R^2
r2 <- summary(lin_mod_meanT)$r.squared
text(0.05, 9, expression(bold(paste('R'^2*' = '), sep='')), col = '#800080')
text(0.15, 8.5, round(r2,3), col = '#800080', font = 2)


#maxT
points(sps_maxT_SHAP_info_allSpp$centralness, cont_maxT, 
       pch = 19, cex = 0.4, col = '#FF000010')

#fit linear model
lin_mod_maxT <- lm(cont_maxT ~ sps_maxT_SHAP_info_allSpp$centralness)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)

#get R^2
r2 <- summary(lin_mod_maxT)$r.squared
text(0.05, 13, expression(bold(paste('R'^2*' = '), sep='')), col = '#FF0000')
text(0.15, 12.5, round(r2,3), col = '#FF0000', font = 2)

dev.off()


##############################################################################

##############################################
############## ALL SPECIES PREC ##############
##############################################


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


## Bind all rows in one table

#minPPT
sps_minPPT_SHAP_info_allSpp <- rbindlist(sps_minPPT_SHAP_info)
sps_minPPT_SHAP_info_allSpp <- as.data.frame(sps_minPPT_SHAP_info_allSpp)

#meanPPT
sps_meanPPT_SHAP_info_allSpp <- rbindlist(sps_meanPPT_SHAP_info)
sps_meanPPT_SHAP_info_allSpp <- as.data.frame(sps_meanPPT_SHAP_info_allSpp)

#maxPPT
sps_maxPPT_SHAP_info_allSpp <- rbindlist(sps_maxPPT_SHAP_info)
sps_maxPPT_SHAP_info_allSpp <- as.data.frame(sps_maxPPT_SHAP_info_allSpp)


## Calculate percentage contribution of the variables (no negatives)

#minPPT
cont_minPPT <- abs(sps_minPPT_SHAP_info_allSpp[,9]) / 
  (abs(sps_minPPT_SHAP_info_allSpp[,9]) + 
     abs(sps_minPPT_SHAP_info_allSpp[,8])) * 100

#meanPPT
cont_meanPPT <- abs(sps_meanPPT_SHAP_info_allSpp[,9]) / 
  (abs(sps_meanPPT_SHAP_info_allSpp[,9]) + 
     abs(sps_meanPPT_SHAP_info_allSpp[,8])) * 100

#maxPPT
cont_maxPPT <- abs(sps_maxPPT_SHAP_info_allSpp[,9]) / 
  (abs(sps_maxPPT_SHAP_info_allSpp[,9]) + 
     abs(sps_maxPPT_SHAP_info_allSpp[,8])) * 100



## Save plot image precipitation variables contribution per absolute polarwardness
setwd(wd_res)
pdf(file = 'AllSpp_Precipitation_AbsPolar.pdf')

#set parametres for plotting
par(mar = c(5,5,5,5))

#minPPT
plot(sps_minPPT_SHAP_info_allSpp$absPolarwardness, cont_minPPT, 
     pch = 19, cex = 0.4, col = '#fc8d5910',
     ylab = 'Contribution minPPT',
     xlab = 'Absolute Polarwardness')

#fit linear model
lin_mod_minPPT <- lm(cont_minPPT ~ sps_minPPT_SHAP_info_allSpp$absPolarwardness)
abline(lin_mod_minPPT, col = '#fc8d59', lwd = 2)

#get R^2
r2 <- summary(lin_mod_minPPT)$r.squared
text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = '#fc8d59')
text(0.10, 4.5, round(r2,3), col = '#ff8d59', font = 2)


#meanPPT
points(sps_meanPPT_SHAP_info_allSpp$absPolarwardness, cont_meanPPT, 
       pch = 19, cex = 0.4, col = '#8c510a10')

#fit linear model
lin_mod_meanPPT <- lm(cont_meanPPT ~ sps_meanPPT_SHAP_info_allSpp$absPolarwardness)
abline(lin_mod_meanPPT, col = '#8c510a', lwd = 2)

#get R^2
r2 <- summary(lin_mod_meanPPT)$r.squared
text(0.05, 9, expression(bold(paste('R'^2*' = '), sep='')), col = '#8c510a')
text(0.10, 8.5, round(r2,3), col = '#8c510a', font = 2)


#maxPPT
points(sps_maxPPT_SHAP_info_allSpp$absPolarwardness, cont_maxPPT, 
       pch = 19, cex = 0.4, col = '#1a985010')

#fit linear model
lin_mod_maxPPT <- lm(cont_maxPPT ~ sps_maxPPT_SHAP_info_allSpp$absPolarwardness)
abline(lin_mod_maxPPT, col = '#1a9850', lwd = 2)

#get R^2
r2 <- summary(lin_mod_maxPPT)$r.squared
text(0.05, 13, expression(bold(paste('R'^2*' = '), sep='')), col = '#1a9850')
text(0.10, 12.5, round(r2,3), col = '#1a9850', font = 2)

dev.off()


## Save plot image precipitation variables contribution per relative polarwardness
setwd(wd_res)
pdf(file = 'AllSpp_Precipitation_RelPolar.pdf')

#set parametres for plotting
par(mar = c(5,5,5,5))

#minPPT
plot(sps_minPPT_SHAP_info_allSpp$relPolarwardness, cont_minPPT, 
     pch = 19, cex = 0.4, col = '#fc8d5910',
     ylab = 'Contribution minPPT',
     xlab = 'Relative Polarwardness')

#fit linear model
lin_mod_minPPT <- lm(cont_minPPT ~ sps_minPPT_SHAP_info_allSpp$relPolarwardness)
abline(lin_mod_minPPT, col = '#fc8d59', lwd = 2)

#get R^2
r2 <- summary(lin_mod_minPPT)$r.squared
text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = '#fc8d59')
text(0.15, 4.5, round(r2,3), col = '#fc8d59', font = 2)


#meanPPT
points(sps_meanPPT_SHAP_info_allSpp$relPolarwardness, cont_meanPPT, 
       pch = 19, cex = 0.4, col = '#8c510a10')

#fit linear model
lin_mod_meanPPT <- lm(cont_meanPPT ~ sps_meanPPT_SHAP_info_allSpp$relPolarwardness)
abline(lin_mod_meanPPT, col = '#8c510a', lwd = 2)

#get R^2
r2 <- summary(lin_mod_meanPPT)$r.squared
text(0.05, 9, expression(bold(paste('R'^2*' = '), sep='')), col = '#8c510a')
text(0.15, 8.5, round(r2,3), col = '#8c510a', font = 2)


#maxPPT
points(sps_maxPPT_SHAP_info_allSpp$relPolarwardness, cont_maxPPT, 
       pch = 19, cex = 0.4, col = '#1a985010')

#fit linear model
lin_mod_maxPPT <- lm(cont_maxPPT ~ sps_maxPPT_SHAP_info_allSpp$relPolarwardness)
abline(lin_mod_maxPPT, col = '#1a9850', lwd = 2)

#get R^2
r2 <- summary(lin_mod_maxPPT)$r.squared
text(0.05, 13, expression(bold(paste('R'^2*' = '), sep='')), col = '#1a9850')
text(0.15, 12.5, round(r2,3), col = '#1a9850', font = 2)

dev.off()


## Save plot image precipitation variables contribution per centralness
setwd(wd_res)
pdf(file = 'AllSpp_Precipitation_Centralness.pdf')

#set parametres for plotting
par(mar = c(5,5,5,5))

#minPPT
plot(sps_minPPT_SHAP_info_allSpp$centralness, cont_minPPT, 
     pch = 19, cex = 0.4, col = '#fc8d5910',
     ylab = 'Contribution minPPT',
     xlab = 'Centralness')

#fit linear model
lin_mod_minPPT <- lm(cont_minPPT ~ sps_minPPT_SHAP_info_allSpp$centralness)
abline(lin_mod_minPPT, col = '#fc8d59', lwd = 2)

#get R^2
r2 <- summary(lin_mod_minPPT)$r.squared
text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = '#fc8d59')
text(0.15, 4.5, round(r2,3), col = '#ff8d59', font = 2)

#meanPPT
points(sps_meanPPT_SHAP_info_allSpp$centralness, cont_meanPPT, 
       pch = 19, cex = 0.4, col = '#8c510a10')

#fit linear model
lin_mod_meanPPT <- lm(cont_meanPPT ~ sps_meanPPT_SHAP_info_allSpp$centralness)
abline(lin_mod_meanPPT, col = '#8c510a', lwd = 2)

#get R^2
r2 <- summary(lin_mod_meanPPT)$r.squared
text(0.05, 9, expression(bold(paste('R'^2*' = '), sep='')), col = '#8c510a')
text(0.15, 8.5, round(r2,3), col = '#8c510a', font = 2)


#maxPPT
points(sps_maxPPT_SHAP_info_allSpp$centralness, cont_maxPPT, 
       pch = 19, cex = 0.4, col = '#1a985010')

#fit linear model
lin_mod_maxPPT <- lm(cont_maxPPT ~ sps_maxPPT_SHAP_info_allSpp$centralness)
abline(lin_mod_maxPPT, col = '#1a9850', lwd = 2)

#get R^2
r2 <- summary(lin_mod_maxPPT)$r.squared
text(0.05, 13, expression(bold(paste('R'^2*' = '), sep='')), col = '#1a9850')
text(0.15, 12.5, round(r2,3), col = '#1a9850', font = 2)

dev.off()





##############################################################################


 #make a list of variables we want to check the contribution
# vars <- c('minT','meanT','maxT','minPPT','meanPPT','maxPPT')

#loop through variables checking results for species that do not cross the equator
for(i in 1:length(vars))
{
  #select variable for each iteration
  # var <- vars[i]
  
  #load results for this variable considering all monohemisferic species
  # setwd(wd_res_shap)
  # sps_m_sel <- lapply(sps_list_sel, function(x){
  #                             read.csv(paste0(x,'_',var,'_.csv'))})
  # names(sps_m_sel) <- sps_list_sel
  
  #select only rows representing presence
  # sps_m_sel_pr <- lapply(sps_m_sel, function(x){x[x$Occurrence == 1,]})
  
  #concatenate point and range info to SHAP results
  # sps_m_sel_SHAP_info <- list()
  # for(j in 1:length(sps_m_sel_pr))
  # {
  #   sps_m_sel_SHAP_info[[j]] <- cbind(sps_m_sel_pr[[j]], 
  #                                   sps_m_sel[[j]][,c('key','datasetKey',
  #                                   'rangeSize','roundness','centralness',     
  #                                   'absPolarwardness','relPolarwardness')])
  # }
    
  #bind all rows in one table
  # sps_m_sel_SHAP_info2 <- rbindlist(sps_m_sel_SHAP_info)
  # sps_m_sel_SHAP_info2 <- as.data.frame(sps_m_sel_SHAP_info2)

  #calculate percentage contribution of the variable
  # cont_var <- abs(sps_m_sel_SHAP_info2[,9]) / 
  #             (abs(sps_m_sel_SHAP_info2[,9]) + abs(sps_m_sel_SHAP_info2[,8])) * 100
  
  ### Variable contribution per absolute polarwardness
  # plot(sps_m_sel_SHAP_info2$absPolarwardness, cont_var, 
  #      pch = 19, cex = 0.4, col = 'gray80',
  #      ylab = paste0('Contribution ', var),
  #      xlab = 'Absolute Polarwardness')
  # 
  # #fit linear model
  # lin_mod <- lm(cont_var ~ sps_m_sel_SHAP_info2$absPolarwardness)
  # abline(lin_mod, col = 'red', lwd = 2)
  # 
  # #get R^2
  # r2 <- summary(lin_mod)$r.squared
  # text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = 'red')
  # text(0.11, 4, round(r2,3), col = 'red', font = 2)
  
  
  # ### Variable contribution per relative polarwardness
  # plot(sps_m_sel_SHAP_info2$relPolarwardness, cont_var, 
  #      pch = 19, cex = 0.4, col = 'gray80',
  #      ylab = paste0('Contribution ', var),
  #      xlab = 'Relative Polarwardness')
  # 
  # #fit linear model
  # lin_mod <- lm(cont_var ~ sps_m_sel_SHAP_info2$relPolarwardness)
  # abline(lin_mod, col = 'red', lwd = 2)
  # 
  # #get R^2
  # r2 <- summary(lin_mod)$r.squared
  # text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = 'red')
  # text(0.16, 4, round(r2,3), col = 'red', font = 2)
  
  
  ### Variable contribution per centralness
  # plot(sps_m_sel_SHAP_info2$centralness, cont_var, 
  #      pch = 19, cex = 0.4, col = 'gray80',
  #      ylab = paste0('Contribution ', var),
  #      xlab = 'Centralness')
  # 
  # #fit linear model
  # lin_mod <- lm(cont_var ~ sps_m_sel_SHAP_info2$centralness)
  # abline(lin_mod, col = 'red', lwd = 2)
  # 
  # #get R^2
  # r2 <- summary(lin_mod)$r.squared
  # text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = 'red')
  # text(0.16, 4, round(r2,3), col = 'red', font = 2)
  # 

# }
# 
# ?lm
# 
# 
# summary(sps_m_sel_SHAP_info2$absPolarwardness)
# 
# as.numeric((sps_m_sel_SHAP_info2[,9]))
# 
# a <- mapply(function(x, y) seq_len(x) + y,
#        c(a =  1, b = 2, c = 3),  # names from first
#        c(A = 10, B = 0, C = -10))
