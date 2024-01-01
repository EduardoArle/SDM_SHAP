####  This script combines the SHAP results for each species with the info
####  about range characteristics and position of points within the ranges

#load libraries
library(data.table)

#list wds
wd_res_shap <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_pts_measure <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Point_and_range_measurements'
wd_res <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Organised results'

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

#make a list of variables we want to check the contribution
vars <- c('minT','meanT','maxT','minPPT','meanPPT','maxPPT')

#loop through variables checking results for species that do not cross the equator
for(i in 1:length(vars))
{
  #select variable for each iteration
  var <- vars[i]
  
  #load results for this variable considering all monohemisferic species
  setwd(wd_res_shap)
  sps_var <- lapply(sps_list_sel, function(x){
                              read.csv(paste0(x,'_',var,'_.csv'))})
  names(sps_var) <- sps_list_sel
  
  #select only rows representing presence
  sps_var_pr <- lapply(sps_var, function(x){x[x$Occurrence == 1,]})
  
  #concatenate point and range info to SHAP results
  sps_var_SHAP_info <- list()
  for(j in 1:length(sps_var_pr))
  {
    sps_var_SHAP_info[[j]] <- cbind(sps_var_pr[[j]], 
                                    sps_m_sel[[j]][,c('key','datasetKey',
                                    'rangeSize','roundness','centralness',     
                                    'absPolarwardness','relPolarwardness')])
  }
    
  #bind all rows in one table
  sps_var_SHAP_info2 <- rbindlist(sps_var_SHAP_info)
  sps_var_SHAP_info2 <- as.data.frame(sps_var_SHAP_info2)

  #calculate percentage contribution of the variable
  cont_var <- abs(sps_var_SHAP_info2[,9]) / 
              (abs(sps_var_SHAP_info2[,9]) + abs(sps_var_SHAP_info2[,8])) * 100
  
  ### Variable contribution per absolute polarwardness
  plot(sps_var_SHAP_info2$absPolarwardness, cont_var, 
       pch = 19, cex = 0.4, col = 'gray80',
       ylab = paste0('Contribution ', var),
       xlab = 'Absolute Polarwardness')
  
  #fit linear model
  lin_mod <- lm(cont_var ~ sps_var_SHAP_info2$absPolarwardness)
  abline(lin_mod, col = 'red', lwd = 2)
  
  #get R^2
  r2 <- summary(lin_mod)$r.squared
  text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = 'red')
  text(0.11, 4, round(r2,3), col = 'red', font = 2)
  
  
  ### Variable contribution per relative polarwardness
  plot(sps_var_SHAP_info2$relPolarwardness, cont_var, 
       pch = 19, cex = 0.4, col = 'gray80',
       ylab = paste0('Contribution ', var),
       xlab = 'Relative Polarwardness')
  
  #fit linear model
  lin_mod <- lm(cont_var ~ sps_var_SHAP_info2$absPolarwardness)
  abline(lin_mod, col = 'red', lwd = 2)
  
  #get R^2
  r2 <- summary(lin_mod)$r.squared
  text(0.05, 5, expression(bold(paste('R'^2*' = '), sep='')), col = 'red')
  text(0.16, 4, round(r2,3), col = 'red', font = 2)
  
  
  ### Variable contribution per centralness
  plot(cont_var, sps_var_SHAP_info2$centralness, pch = 19, cex = 0.4)
  
}

?lm


summary(sps_var_SHAP_info2$absPolarwardness)

as.numeric((sps_var_SHAP_info2[,9]))

a <- mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))
