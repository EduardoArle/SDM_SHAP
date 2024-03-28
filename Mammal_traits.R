#load packages
library(data.table)

#list WDs
wd_data <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Mammal_trait_data/Smith_etal_2003_Ecology'

#load table
setwd(wd_data)
tab <- read.table('MOMv3.3.txt', sep = '\t')

#figure out what info comes in each column, because the authors couldn't bother
names(tab) <- c('Region', 'Status', 'Order', 'Family', 'Genus', 'Epithet',
                'NPI', 'BodyMass', 'NPI2')

#substitute values from -999 to NA
tab[tab[] == -999] <- NA

#make a column with the species name
tab$Species <- paste0(tab$Genus, ' ', tab$Epithet)

#save as csv
write.csv(tab, 'Mammals_bodymass_Smmith_2003.csv', row.names = F)

############ DATA SET THAT HAD VERY FEW SPECIES ##############



wd_data <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Mammal_trait_data/animaltraits.github.io-1.0.7/data/raw/metabolic rate/Mammalia'

#load all tables
setwd(wd_data)
tabs <- lapply(list.files(), read.csv)
sapply(tabs, nrow)

#rbindlist all tables
tabs2 <- rbindlist(tabs, fill = T)

#separate info into body mass and metabolic rate

#ATTENTION: there are measuments in grams and in kg
bodyMass <- tabs2[tabs2$measurementType == 'body mass',] 

#ATTENTION: tere are measuments in 9 different units
metRate <- tabs2[tabs2$measurementType == 'metabolic rate',]
