library(taxize)

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'

#read temperature results table
setwd(wd_tables)
res_temp <- read.csv('Temperature_Rel_Polar_all_points.csv')

#get orders
res_temp$order <- NA

for(i in 360:nrow(res_temp))
{
  a <- tax_name(res_temp$species[i], get = 'order', db = 'ncbi')
  res_temp$order[i] <- a$order
  
  print(i)
}

#manually include the orders that were not founf
missing <- which(is.na(res_temp$order))

i = 1
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 2
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 3
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 4
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 5
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 6
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Afrosoricida'

i = 7
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 8
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Primates'

i = 9
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Primates'

i = 10
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Primates'

i = 11
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 12
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Primates'

i = 13
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 14
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Chiroptera'

i = 15
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Chiroptera'

i = 16
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Chiroptera'

i = 17
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 18
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 19
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 20
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 21
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Chiroptera'

i = 22
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 23
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 24
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 25
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 26
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 27
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Chiroptera'

i = 28
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 29
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 30
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 31
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 32
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 33
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 34
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Primates'

i = 35
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 36
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Chiroptera'

i = 37
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 38
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 39
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Eulipotyphla'

i = 40
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 41
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 42
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 43
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 44
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 45
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 46
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 47
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 48
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Rodentia'

i = 49
res_temp$species[missing[i]]
res_temp$order[missing[i]] <-  'Didelphimorphia'

i = 50
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'

i = 51
res_temp$species[missing[i]]
res_temp$order[missing[i]] <- 'Rodentia'


res_temp$species[missing[1]]
res_temp$order[missing[1]]  <- 'Rodentia'


#write table with orders
setwd(wd_tables)
res_temp <- write.csv(res_temp, 'Temperature_Rel_Polar_all_points_order.csv',
                      row.names = F)

sort(unique(res_temp$order))
table((res_temp$order))
summary(res_temp$order)
