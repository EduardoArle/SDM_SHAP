#load libraries
library(taxize); library(data.table)

#list wds
wd_pts_measure <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Point_and_range_measurements'
wd_res_shap <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_orders <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'
wd_out <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/All_species_analysis'

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

#load table informing each species order
setwd(wd_orders)
sps_orders <- read.csv('Temperature_Rel_Polar_all_points_order.csv')

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
  
  #include order info into the species tables
  #if the info is not in the table, try to fetch from taxize
  if(TRUE %in% unique(sps_orders$species == unique(sps_tables[[i]]$species))){
    sps_tables[[i]]$order <- sps_orders$order[
      sps_orders$species == unique(sps_tables[[i]]$species)]
  }else{
    a <- tax_name(unique(sps_tables[[i]]$species), get = 'order', db = 'ncbi')
    sps_tables[[i]]$order <- a$order 
  }

  
  #include values for each point into the species tables
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
  
  print(i)
}

#check which species do not have info on order
orders <- sapply(sps_tables, function(x){unique(x$order)})
missing <- which(is.na(orders))

#include info manually
unique(sps_tables[[missing[1]]]$species)
sps_tables[[missing[1]]]$order <- 'Cingulata'

unique(sps_tables[[missing[2]]]$species)
sps_tables[[missing[2]]]$order <- 'Chiroptera'

unique(sps_tables[[missing[3]]]$species)
sps_tables[[missing[3]]]$order <- 'Rodentia'

unique(sps_tables[[missing[4]]]$species)
sps_tables[[missing[4]]]$order <- 'Rodentia'

unique(sps_tables[[missing[5]]]$species)
sps_tables[[missing[5]]]$order <- 'Rodentia'

unique(sps_tables[[missing[6]]]$species)
sps_tables[[missing[6]]]$order <- 'Rodentia'

unique(sps_tables[[missing[7]]]$species)
sps_tables[[missing[7]]]$order <- 'Rodentia'

unique(sps_tables[[missing[8]]]$species)
sps_tables[[missing[8]]]$order <- 'Primates'

unique(sps_tables[[missing[9]]]$species)
sps_tables[[missing[9]]]$order <- 'Rodentia'

unique(sps_tables[[missing[10]]]$species)
sps_tables[[missing[10]]]$order <- 'Didelphimorphia'

unique(sps_tables[[missing[11]]]$species)
sps_tables[[missing[11]]]$order <- 'Rodentia'

unique(sps_tables[[missing[12]]]$species)
sps_tables[[missing[12]]]$order <- 'Rodentia'

unique(sps_tables[[missing[13]]]$species)
sps_tables[[missing[13]]]$order <- 'Primates'

unique(sps_tables[[missing[14]]]$species)
sps_tables[[missing[14]]]$order <- 'Primates'

unique(sps_tables[[missing[15]]]$species)
sps_tables[[missing[15]]]$order <- 'Primates'

unique(sps_tables[[missing[16]]]$species)
sps_tables[[missing[16]]]$order <- 'Primates'

unique(sps_tables[[missing[17]]]$species)
sps_tables[[missing[17]]]$order <- 'Primates'

unique(sps_tables[[missing[18]]]$species)
sps_tables[[missing[18]]]$order <- 'Primates'

unique(sps_tables[[missing[19]]]$species)
sps_tables[[missing[19]]]$order <- 'Primates'

unique(sps_tables[[missing[20]]]$species)
sps_tables[[missing[20]]]$order <- 'Primates'

unique(sps_tables[[missing[21]]]$species)
sps_tables[[missing[21]]]$order <- 'Chiroptera'

unique(sps_tables[[missing[22]]]$species)
sps_tables[[missing[22]]]$order <- 'Chiroptera'

unique(sps_tables[[missing[23]]]$species)
sps_tables[[missing[23]]]$order <- 'Chiroptera'

unique(sps_tables[[missing[24]]]$species)
sps_tables[[missing[24]]]$order <- 'Rodentia'

unique(sps_tables[[missing[25]]]$species)
sps_tables[[missing[25]]]$order <- 'Rodentia'

unique(sps_tables[[missing[26]]]$species)
sps_tables[[missing[26]]]$order <- 'Chiroptera'

unique(sps_tables[[missing[27]]]$species)
sps_tables[[missing[27]]]$order <- 'Rodentia'

unique(sps_tables[[missing[28]]]$species)
sps_tables[[missing[28]]]$order <- 'Rodentia'

unique(sps_tables[[missing[29]]]$species)
sps_tables[[missing[29]]]$order <- 'Rodentia'

unique(sps_tables[[missing[30]]]$species)
sps_tables[[missing[30]]]$order <- 'Rodentia'

#make a table with all species values
all_sps_table <- rbindlist(sps_tables, fill = T)

#save all species table
setwd(wd_out)
write.csv(all_sps_table, 'Results_all_sps.csv', row.names = F)

