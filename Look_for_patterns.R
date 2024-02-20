#load libraries

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'


################### TEMPERATURE ###################


#read temperature table
temperature <- read.csv('Temperature_Rel_Polar_all_points.csv')

########### select species with 10 points or more ############
T_10_recs <- temperature[temperature$n_records > 9,]

## Relative polarwardness

#visualise species by positive or negative slope RP
T_10_recs$slope_minT <- ifelse(T_10_recs$minTslope_RP > 0, 'pos', 'neg')
table(T_10_recs$slope_minT)

T_10_recs$slope_meanT <- ifelse(T_10_recs$meanTslope_RP > 0, 'pos', 'neg')
table(T_10_recs$slope_meanT)

T_10_recs$slope_maxT <- ifelse(T_10_recs$maxTslope_RP > 0, 'pos', 'neg')
table(T_10_recs$slope_maxT)


#filter only the ones with kind of decent R2
T_10_recs_OK_min <- T_10_recs[T_10_recs$minTR2_RP > 0.2,]
table(T_10_recs_OK_min$slope_minT)

T_10_recs_OK_mean <- T_10_recs[T_10_recs$meanTR2_RP > 0.2,]
table(T_10_recs_OK_mean$slope_meanT)

T_10_recs_OK_max <- T_10_recs[T_10_recs$maxTR2_RP > 0.2,]
table(T_10_recs_OK_max$slope_maxT)


#filter only the species with more than 5 degrees latitudinal amplitude
T_10_degrees <- T_10_recs[T_10_recs$maxLat - T_10_recs$minLat >= 10,]

#visualise species by positive or negative slope RP
table(T_10_degrees$slope_minT)

table(T_10_degrees$slope_meanT)

table(T_10_degrees$slope_maxT)


#filter only the ones with kind of decent R2
T_10_degrees_OK_min <- T_10_degrees[T_10_degrees$minTR2_RP > 0.2,]
table(T_10_degrees_OK_min$slope_minT)

T_10_degrees_OK_mean <- T_10_degrees[T_10_degrees$meanTR2_RP > 0.2,]
table(T_10_degrees_OK_mean$slope_meanT)

T_10_degrees_OK_max <- T_10_degrees[T_10_degrees$maxTR2_RP > 0.2,]
table(T_10_degrees_OK_max$slope_maxT)


#tests

#plot slope vs range latitudinal amplitude (minT)
plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$minTslope_RP)

plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$meanTslope_RP)

plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$maxTslope_RP)

summary(T_10_recs$minTslope_RP)
summary(T_10_recs$meanTslope_RP)
summary(T_10_recs$maxTslope_RP)



## Centralness

#visualise species by positive or negative slope centralness
T_10_recs$slope_minT_cent <- ifelse(T_10_recs$minTslope_C > 0, 'pos', 'neg')
table(T_10_recs$slope_minT_cent)

T_10_recs$slope_meanT_cent <- ifelse(T_10_recs$meanTslope_C > 0, 'pos', 'neg')
table(T_10_recs$slope_meanT_cent)

T_10_recs$slope_maxT_cent <- ifelse(T_10_recs$maxTslope_C > 0, 'pos', 'neg')
table(T_10_recs$slope_maxT_cent)


#filter only the ones with kind of decent R2
T_10_recs_OK_min_cent <- T_10_recs[T_10_recs$minTR2_C > 0.2,]
table(T_10_recs_OK_min_cent$slope_minT_cent)

T_10_recs_OK_mean_cent <- T_10_recs[T_10_recs$meanTR2_C > 0.2,]
table(T_10_recs_OK_mean_cent$slope_meanT_cent)

T_10_recs_OK_max_cent <- T_10_recs[T_10_recs$maxTR2_C > 0.2,]
table(T_10_recs_OK_max_cent$slope_maxT_cent)


#filter only the species with round-ish ranges
T_round <- T_10_recs[T_10_recs$rangeRoundness >= 0.5,]

#visualise species by positive or negative slope RP
table(T_round$slope_minT)

table(T_round$slope_meanT)

table(T_round$slope_maxT)


#filter only the ones with kind of decent R2
T_round_OK_min <- T_round[T_round$minTR2_C > 0.2,]
table(T_round_OK_min$slope_minT_cent)

T_round_OK_mean <- T_round[T_round$meanTR2_C > 0.2,]
table(T_round_OK_mean$slope_meanT_cent)

T_round_OK_max <- T_round[T_round$maxTR2_C > 0.2,]
table(T_round_OK_max$slope_maxT_cent)


#tests

#plot slope vs range latitudinal amplitude (minT)
plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$minTslope_RP)

plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$meanTslope_RP)

plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$maxTslope_RP)

summary(T_10_recs$minTslope_RP)
summary(T_10_recs$meanTslope_RP)
summary(T_10_recs$maxTslope_RP)


################### PRECIPITATION ###################

#read temperature table
temperature <- read.csv('Temperature_Rel_Polar_all_points.csv')

########### select species with 10 points or more ############
T_10_recs <- temperature[temperature$n_records > 9,]



#visualise species by positive or negative slope RP
T_10_recs$slope_minT <- ifelse(T_10_recs$minTslope_RP > 0, 'pos', 'neg')
table(T_10_recs$slope_minT)

T_10_recs$slope_meanT <- ifelse(T_10_recs$meanTslope_RP > 0, 'pos', 'neg')
table(T_10_recs$slope_meanT)

T_10_recs$slope_maxT <- ifelse(T_10_recs$maxTslope_RP > 0, 'pos', 'neg')
table(T_10_recs$slope_maxT)


#filter only the ones with kind of decent R2
T_10_recs_OK_min <- T_10_recs[T_10_recs$minTR2_RP > 0.2,]
table(T_10_recs_OK_min$slope_minT)

T_10_recs_OK_mean <- T_10_recs[T_10_recs$meanTR2_RP > 0.2,]
table(T_10_recs_OK_mean$slope_meanT)

T_10_recs_OK_max <- T_10_recs[T_10_recs$maxTR2_RP > 0.2,]
table(T_10_recs_OK_max$slope_maxT)


#filter only the species with more than 5 degrees latitudinal amplitude
T_10_degrees <- T_10_recs[T_10_recs$maxLat - T_10_recs$minLat >= 10,]

#visualise species by positive or negative slope RP
table(T_10_degrees$slope_minT)

table(T_10_degrees$slope_meanT)

table(T_10_degrees$slope_maxT)


#filter only the ones with kind of decent R2
T_10_degrees_OK_min <- T_10_degrees[T_10_degrees$minTR2_RP > 0.2,]
table(T_10_degrees_OK_min$slope_minT)

T_10_degrees_OK_mean <- T_10_degrees[T_10_degrees$meanTR2_RP > 0.2,]
table(T_10_degrees_OK_mean$slope_meanT)

T_10_degrees_OK_max <- T_10_degrees[T_10_degrees$maxTR2_RP > 0.2,]
table(T_10_degrees_OK_max$slope_maxT)


#tests

#plot slope vs range latitudinal amplitude (minT)
plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$minTslope_RP)

plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$meanTslope_RP)

plot(T_10_degrees$maxLat - T_10_degrees$minLat, T_10_degrees$maxTslope_RP)

summary(T_10_recs$minTslope_RP)
summary(T_10_recs$meanTslope_RP)
summary(T_10_recs$maxTslope_RP)


#######

nrow(temperature)
nrow(T_10_recs)

nrow(T_10_recs_OK_min)
