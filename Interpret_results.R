wd_results <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'

tab_all_pts_T <- read.csv('Temperature_Rel_Polar_all_points.csv')
tab_all_pts_T <- tab_all_pts_T[tab_all_pts_T$n_records > 2,]

# plot R2 values against different measurements

# number of records

#min
plot(tab_all_pts_T$minTR2_RP, tab_all_pts_T$n_records,
     pch = 19, cex = 0.4, col = '#0000FF',
     ylab = 'Number of records',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_minT <- lm(tab_all_pts_T$n_records ~ tab_all_pts_T$minTR2_RP)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)
summary(lin_mod_minT)$r.squared


#mean
plot(tab_all_pts_T$meanTR2_RP, tab_all_pts_T$n_records,
     pch = 19, cex = 0.4, col = '#800080',
     ylab = 'Number of records',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_meanT <- lm(tab_all_pts_T$n_records ~ tab_all_pts_T$meanTR2_RP)
abline(lin_mod_meanT, col = '#800080', lwd = 2)
summary(lin_mod_meanT)$r.squared


#max
plot(tab_all_pts_T$maxTR2_RP, tab_all_pts_T$n_records,
     pch = 19, cex = 0.4, col = '#FF0000',
     ylab = 'Number of records',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_maxT <- lm(tab_all_pts_T$n_records ~ tab_all_pts_T$maxTR2_RP)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)
summary(lin_mod_maxT)$r.squared


# range roundness

#min
plot(tab_all_pts_T$minTR2_RP, tab_all_pts_T$rangeRoundness,
     pch = 19, cex = 0.4, col = '#0000FF',
     ylab = 'Range roundness',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_minT <- lm(tab_all_pts_T$rangeRoundness ~ tab_all_pts_T$minTR2_RP)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)
summary(lin_mod_minT)$r.squared


#mean
plot(tab_all_pts_T$meanTR2_RP, tab_all_pts_T$rangeRoundness,
     pch = 19, cex = 0.4, col = '#800080',
     ylab = 'Range roundness',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_meanT <- lm(tab_all_pts_T$rangeRoundness ~ tab_all_pts_T$meanTR2_RP)
abline(lin_mod_meanT, col = '#800080', lwd = 2)
summary(lin_mod_meanT)$r.squared


#max
plot(tab_all_pts_T$maxTR2_RP, tab_all_pts_T$rangeRoundness,
     pch = 19, cex = 0.4, col = '#FF0000',
     ylab = 'Range roundness',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_maxT <- lm(tab_all_pts_T$rangeRoundness ~ tab_all_pts_T$maxTR2_RP)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)
summary(lin_mod_maxT)$r.squared


# range size

#min
plot(tab_all_pts_T$minTR2_RP, tab_all_pts_T$rangeSize,
     pch = 19, cex = 0.4, col = '#0000FF',
     ylab = 'Range size',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_minT <- lm(tab_all_pts_T$rangeSize ~ tab_all_pts_T$minTR2_RP)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)
summary(lin_mod_minT)$r.squared


#mean
plot(tab_all_pts_T$meanTR2_RP, tab_all_pts_T$rangeSize,
     pch = 19, cex = 0.4, col = '#800080',
     ylab = 'Range size',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_meanT <- lm(tab_all_pts_T$rangeSize ~ tab_all_pts_T$meanTR2_RP)
abline(lin_mod_meanT, col = '#800080', lwd = 2)
summary(lin_mod_meanT)$r.squared


#max
plot(tab_all_pts_T$maxTR2_RP, tab_all_pts_T$rangeSize,
     pch = 19, cex = 0.4, col = '#FF0000',
     ylab = 'Range size',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_maxT <- lm(tab_all_pts_T$rangeSize ~ tab_all_pts_T$maxTR2_RP)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)
summary(lin_mod_maxT)$r.squared


# range latitudinal extension

#min
plot(tab_all_pts_T$minTR2_RP, tab_all_pts_T$maxLat - tab_all_pts_T$minLat,
     pch = 19, cex = 0.4, col = '#0000FF',
     ylab = 'Latitudinal extension',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_minT <- lm(tab_all_pts_T$maxLat - tab_all_pts_T$minLat ~ tab_all_pts_T$minTR2_RP)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)
summary(lin_mod_minT)$r.squared


#mean
plot(tab_all_pts_T$meanTR2_RP, tab_all_pts_T$maxLat - tab_all_pts_T$minLat,
     pch = 19, cex = 0.4, col = '#800080',
     ylab = 'Range size',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_meanT <- lm(tab_all_pts_T$maxLat - tab_all_pts_T$minLat ~ tab_all_pts_T$meanTR2_RP)
abline(lin_mod_meanT, col = '#800080', lwd = 2)
summary(lin_mod_meanT)$r.squared


#max
plot(tab_all_pts_T$maxTR2_RP, tab_all_pts_T$maxLat - tab_all_pts_T$minLat,
     pch = 19, cex = 0.4, col = '#FF0000',
     ylab = 'Range size',
     xlab = expression(paste('R'^2*''), sep=''))

lin_mod_maxT <- lm(tab_all_pts_T$maxLat - tab_all_pts_T$minLat ~ tab_all_pts_T$maxTR2_RP)
abline(lin_mod_maxT, col = '#FF0000', lwd = 2)
summary(lin_mod_maxT)$r.squared

