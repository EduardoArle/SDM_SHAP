#load libraries

#list WDs
wd_data <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Summarise results'

#load data
setwd(wd_data)
load('Clean_MeData_dd_lis.Rdata')
load('pairs.Rdata')

#unite the dark diversity data with the pairs classification
log_data <- merge(dd_data, pairs, all.x = T, sort = F,
                   by = c('data.origin', 'country', 'Site', 'enforcement'))

# remove the data that dont belong to a MPA-control pair 
log_data <- log_data[complete.cases(log_data$pair),]

# fix capital letter in pairs 
log_data$pair <- paste0(toupper(substr(log_data$pair, 1, 1)), 
                         substr(log_data$pair, 2, nchar(log_data$pair)))

# convert pair to factor
log_data$pair <- as.factor(log_data$pair)

# remove unnecessary columns 
log_data <- data.frame(pair = log_data$pair, protection = log_data$protection,
                        enforcement =  log_data$enforcement, 
                        dark =  log_data$dark,
                        observed_richness =  log_data$observed_richness,
                        total =  log_data$total)

# calculate completeness
log_data$completeness <- log(log_data$observed_richness / log_data$dark)

# convert to long format
log_data_A <- log_data[,c(1:3)]
log_data_A$index <- 'dark'
log_data_A$richness <- log_data$dark

log_data_B <- log_data[,c(1:3)]
log_data_B$index <- 'observed_richness'
log_data_B$richness <- log_data$observed_richness

log_data_C <- log_data[,c(1:3)]
log_data_C$index <- 'total'
log_data_C$richness <- log_data$total

log_data_D <- log_data[,c(1:3)]
log_data_D$index <- 'completeness'
log_data_D$richness <- log_data$completeness

log_data <- rbind(log_data_A, log_data_B, log_data_C, log_data_D)

# create list of all fully protected MPAs
fully_protected_pairs <- as.character(unique(log_data$pair[log_data$enforcement > 2]))

# Change protected/no protected to MPA vs Control
log_data$protection <- ifelse(log_data$protection == "Protected", "MPA", "Control")

# keep the dark diversity index only (without dplyr)
d_ratio <- log_data[log_data$index == 'dark',]



##### MAI #######



