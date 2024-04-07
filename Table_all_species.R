#load libraries

#list wds
wd_pts_measure <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Point_and_range_measurements'
wd_res_shap <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'

#list species
setwd(wd_pts_measure)
sps_list <- gsub('_point_range_metrics.csv', '', list.files())

#load files
setwd(wd_pts_measure)
sps_tables <- lapply(list.files(), read.csv)

names(sps_tables) <- sps_list #name objects

#create a column with n occurrences for each species
for(i in 1:length(sps_tables))
{
  sps_tables[[i]]$n_occ <- nrow(sps_tables[[i]])
}

#load shap results
setwd(wd_res_shap)
shap_results <- lapply(list.files(), read.csv)

names(shap_results) <- gsub('_.csv', '', list.files())  #name objects

#include shap results in each sps_table
for(i in 1:length(sps_tables))
{
  #select each file with SHAP results for the species
  minT <- shap_results[names(shap_results) == 
                         paste0(names(sps_tables)[i], '_minT')][[1]]
  meanT <- shap_results[names(shap_results) == 
                         paste0(names(sps_tables)[i], '_meanT')][[1]]
  maxT <- shap_results[names(shap_results) == 
                         paste0(names(sps_tables)[i], '_maxT')][[1]]
  
  minPPT <- shap_results[names(shap_results) == 
                         paste0(names(sps_tables)[i], '_minPPT')][[1]]
  meanPPT <- shap_results[names(shap_results) == 
                          paste0(names(sps_tables)[i], '_meanPPT')][[1]]
  maxPPT <- shap_results[names(shap_results) == 
                         paste0(names(sps_tables)[i], '_maxPPT')][[1]]
  
  #select only presences
  minT2 <- minT[minT$Type == 'Presence',]
  meanT2 <- meanT[meanT$Type == 'Presence',]
  maxT2 <- maxT[maxT$Type == 'Presence',]
  
  minPPT2 <- minPPT[minPPT$Type == 'Presence',]
  meanPPT2 <- meanPPT[meanPPT$Type == 'Presence',]
  maxPPT2 <- maxPPT[maxPPT$Type == 'Presence',]

  #rename cols to indicate the 'control variable'
  names(minT2)[names(minT2) == 'Mean_PPT_SHAP'] <- 'control_Min_T_SHAP'
  names(meanT2)[names(meanT2) == 'Mean_PPT_SHAP'] <- 'control_Mean_T_SHAP'
  names(maxT2)[names(maxT2) == 'Mean_PPT_SHAP'] <- 'control_Max_T_SHAP'
  
  names(minPPT2)[names(minPPT2) == 'Mean_T_SHAP'] <- 'control_Min_PPT_SHAP'
  names(meanPPT2)[names(meanPPT2) == 'Mean_T_SHAP'] <- 'control_Mean_PPT_SHAP'
  names(maxPPT2)[names(maxPPT2) == 'Mean_T_SHAP'] <- 'control_Max_PPT_SHAP'
  
  #select only columns of interest in each table
  minT3 <- minT2[c('decimalLongitude','decimalLatitude',
                   'Min_T_SHAP', 'control_Min_T_SHAP')]
  meanT3 <- meanT2[c('decimalLongitude','decimalLatitude',
                     'Mean_T_SHAP', 'control_Mean_T_SHAP')]
  maxT3 <- maxT2[c('decimalLongitude','decimalLatitude',
                   'Max_T_SHAP', 'control_Max_T_SHAP')]
  
  minPPT3 <- minPPT2[c('decimalLongitude','decimalLatitude',
                       'Min_PPT_SHAP', 'control_Min_PPT_SHAP')]
  meanPPT3 <- meanPPT2[c('decimalLongitude','decimalLatitude',
                         'Mean_PPT_SHAP', 'control_Mean_PPT_SHAP')]
  maxPPT3 <- maxPPT2[c('decimalLongitude','decimalLatitude',
                       'Max_PPT_SHAP', 'control_Max_PPT_SHAP')]
  
  #include values for each point into the species table
  sps_tables[[i]] <- merge(sps_tables[[i]], minT3,
                           by = c('decimalLongitude','decimalLatitude'))
  sps_tables[[i]] <- merge(sps_tables[[i]], meanT3,
                           by = c('decimalLongitude','decimalLatitude'))
  sps_tables[[i]] <- merge(sps_tables[[i]], maxT3,
                           by = c('decimalLongitude','decimalLatitude'))
  
  sps_tables[[i]] <- merge(sps_tables[[i]], minPPT3,
                           by = c('decimalLongitude','decimalLatitude'))
  sps_tables[[i]] <- merge(sps_tables[[i]], meanPPT3,
                           by = c('decimalLongitude','decimalLatitude'))
  sps_tables[[i]] <- merge(sps_tables[[i]], maxPPT3,
                           by = c('decimalLongitude','decimalLatitude'))
  
}
