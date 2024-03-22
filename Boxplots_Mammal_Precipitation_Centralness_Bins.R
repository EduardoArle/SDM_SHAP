#load packages

#list wds
wd_tables <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Each_species_all_points_posneg'

#read temperature results table

setwd(wd_tables)
res_temp <- read.csv('Temperature_Rel_Polar_all_points_order.csv')


#################################################################
########################## CENTRALNESS ##########################
#################################################################


############################
######### MIN TEMP ########
############################


#eliminate 0s and NAs in the slope
res_temp_roundness <- res_temp[which(res_temp$minTslope_C != 0),]

#### all roundness
res_temp_roundness_all <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0),]

pos_temp_roundness_all <- sum(res_temp_roundness_all$minTslope_C > 0) /
  nrow(res_temp_roundness_all) * 100
neg_temp_roundness_all <- sum(res_temp_roundness_all$minTslope_C < 0) /
  nrow(res_temp_roundness_all) * 100

#### 0 to 0.2
res_temp_roundness_0_02 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0 &
          res_temp_roundness$rangeRoundness < 0.2),]

pos_temp_roundness_0_02 <- sum(res_temp_roundness_0_02$minTslope_C > 0) /
  nrow(res_temp_roundness_0_02) * 100
neg_temp_roundness_0_02 <- sum(res_temp_roundness_0_02$minTslope_C < 0) /
  nrow(res_temp_roundness_0_02) * 100

#### 0.2 to 0.4
res_temp_roundness_02_04 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.2 &
          res_temp_roundness$rangeRoundness < 0.4),]

pos_temp_roundness_02_04 <- sum(res_temp_roundness_02_04$minTslope_C > 0) /
  nrow(res_temp_roundness_02_04) * 100
neg_temp_roundness_02_04 <- sum(res_temp_roundness_02_04$minTslope_C < 0) /
  nrow(res_temp_roundness_02_04) * 100

#### 0.4 to 0.6
res_temp_roundness_04_06 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.4 &
          res_temp_roundness$rangeRoundness < 0.6),]

pos_temp_roundness_04_06 <- sum(res_temp_roundness_04_06$minTslope_C > 0) /
  nrow(res_temp_roundness_04_06) * 100
neg_temp_roundness_04_06 <- sum(res_temp_roundness_04_06$minTslope_C < 0) /
  nrow(res_temp_roundness_04_06) * 100

#### 0.6 to 0.8
res_temp_roundness_06_08 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.6 &
          res_temp_roundness$rangeRoundness < 0.8),]

pos_temp_roundness_06_08 <- sum(res_temp_roundness_06_08$minTslope_C > 0) /
  nrow(res_temp_roundness_06_08) * 100
neg_temp_roundness_06_08 <- sum(res_temp_roundness_06_08$minTslope_C < 0) /
  nrow(res_temp_roundness_06_08) * 100

#### 0.8 to 1
res_temp_roundness_08_1 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.8),]

pos_temp_roundness_08_1 <- sum(res_temp_roundness_08_1$minTslope_C > 0) /
  nrow(res_temp_roundness_08_1) * 100
neg_temp_roundness_08_1 <- sum(res_temp_roundness_08_1$minTslope_C < 0) /
  nrow(res_temp_roundness_08_1) * 100

#create a matrix with the data
dt_round <- matrix(c(c(pos_temp_roundness_all, neg_temp_roundness_all),
                      c(pos_temp_roundness_0_02, neg_temp_roundness_0_02),
                      c(pos_temp_roundness_02_04, neg_temp_roundness_02_04),
                      c(pos_temp_roundness_04_06, neg_temp_roundness_04_06),
                      c(pos_temp_roundness_06_08, neg_temp_roundness_06_08),
                      c(pos_temp_roundness_08_1, neg_temp_roundness_08_1)),
                    ncol = 6)

#name the columns
colnames(dt_round) <-
  c('all', 'R_0_02', 'R_02_04', 'R_04_06', 'R_06_08', 'R_08_1')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_round_xx <- 
  matrix(c(c(pos_temp_roundness_all, neg_temp_roundness_all, xx,xx,xx,xx,xx),
          c(xx,pos_temp_roundness_0_02,neg_temp_roundness_0_02,xx,xx,xx,xx),
          c(xx,xx,pos_temp_roundness_02_04,neg_temp_roundness_02_04,xx,xx,xx),
          c(xx,xx,xx,pos_temp_roundness_04_06,neg_temp_roundness_04_06,xx,xx),
          c(xx,xx,xx,xx,pos_temp_roundness_06_08,neg_temp_roundness_06_08,xx),
          c(xx,xx,xx,xx,xx,pos_temp_roundness_08_1,neg_temp_roundness_08_1)),
         ncol = 6)

#name the columns
colnames(dt_round_xx) <-
  c('all', 'R_0_02', 'R_02_04', 'R_04_06', 'R_06_08', 'R_08_1')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_round_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Range Roundness')

text(x= bp[1], y = dt_round[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_all$minTslope_C > 0)))
text(x= bp[1], y = dt_round[2,1]/2 + dt_round[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_all$minTslope_C < 0)))

text(x= bp[2], y = dt_round[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_0_02$minTslope_C > 0)))
text(x= bp[2], y = dt_round[2,2]/2 + dt_round[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_0_02$minTslope_C < 0)))

text(x= bp[3], y= dt_round[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_02_04$minTslope_C > 0)))
text(x= bp[3], y= dt_round[2,3]/2 + dt_round[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_02_04$minTslope_C < 0)))

text(x= bp[4], y= dt_round[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_04_06$minTslope_C > 0)))
text(x= bp[4], y= dt_round[2,4]/2 + dt_round[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_04_06$minTslope_C < 0)))

text(x= bp[5], y= dt_round[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_06_08$minTslope_C > 0)))
text(x= bp[5], y= dt_round[2,5]/2 + dt_round[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_06_08$minTslope_C < 0)))

text(x= bp[6], y= dt_round[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_08_1$minTslope_C > 0)))
text(x= bp[6], y= dt_round[2,6]/2 + dt_round[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_08_1$minTslope_C < 0)))



#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_rangeSize <- res_temp[which(res_temp$minTslope_C != 0),]

#### all amplitudes
res_temp_rangeSize_all <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_rangeSize_all$minTslope_C > 0) /
  nrow(res_temp_rangeSize_all) * 100
neg_rangeSize_all <- sum(res_temp_rangeSize_all$minTslope_C < 0) /
  nrow(res_temp_rangeSize_all) * 100

#### 0 to 100000 km2
res_temp_rangeSize_0_100k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize < 100000),]

pos_rangeSize_0_100k  <- sum(res_temp_rangeSize_0_100k$minTslope_C > 0) /
  nrow(res_temp_rangeSize_0_100k) * 100
neg_rangeSize_0_100k  <- sum(res_temp_rangeSize_0_100k$minTslope_C < 0) /
  nrow(res_temp_rangeSize_0_100k) * 100

#### 100000 to 500000 km2
res_temp_rangeSize_100_500k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 100000 &
        res_temp_rangeSize$rangeSize <= 500000),]

pos_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$minTslope_C > 0) /
  nrow(res_temp_rangeSize_100_500k) * 100
neg_rangeSize_100_500k  <- sum(res_temp_rangeSize_100_500k$minTslope_C < 0) /
  nrow(res_temp_rangeSize_100_500k) * 100

#### 500000 to 1000000 km2
res_temp_rangeSize_500k_1M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 500000 &
        res_temp_rangeSize$rangeSize < 1000000),]

pos_rangeSize_500k_1M <- 
  sum(res_temp_rangeSize_500k_1M$minTslope_C > 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100
neg_rangeSize_500k_1M <-
  sum(res_temp_rangeSize_500k_1M $minTslope_C < 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100

#### 1000000 to 2000000 km2
res_temp_rangeSize_1_2M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 1000000 &
          res_temp_rangeSize$rangeSize < 2000000),]

pos_rangeSize_1_2M <- 
  sum(res_temp_rangeSize_1_2M$minTslope_C > 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100
neg_rangeSize_1_2M <-
  sum(res_temp_rangeSize_1_2M $minTslope_C < 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100

#### 2000000 to 3000000 km2
res_temp_rangeSize_2_3M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 2000000 &
          res_temp_rangeSize$rangeSize < 3000000),]

pos_rangeSize_2_3M <- 
  sum(res_temp_rangeSize_2_3M$minTslope_C > 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100
neg_rangeSize_2_3M <-
  sum(res_temp_rangeSize_2_3M $minTslope_C < 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100

#### more than 3000000 km2
res_temp_rangeSize_3M  <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize  >= 3000000),]

pos_rangeSize_3M <- 
  sum(res_temp_rangeSize_3M$minTslope_C > 0) /
  nrow(res_temp_rangeSize_3M) * 100
neg_rangeSize_3M <-
  sum(res_temp_rangeSize_3M $minTslope_C < 0) /
  nrow(res_temp_rangeSize_3M ) * 100

#create a matrix with the data
dt_rangeSize <-
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all),
           c(pos_rangeSize_0_100k, neg_rangeSize_0_100k),
           c(pos_rangeSize_100_500k, neg_rangeSize_100_500k),
           c(pos_rangeSize_500k_1M, neg_rangeSize_500k_1M),
           c(pos_rangeSize_1_2M, neg_rangeSize_1_2M),
           c(pos_rangeSize_2_3M, neg_rangeSize_2_3M),
           c(pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_rangeSize_xx <- 
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all,xx,xx,xx,xx,xx,xx),
           c(xx,pos_rangeSize_0_100k, neg_rangeSize_0_100k, xx,xx,xx,xx,xx),
           c(xx,xx,pos_rangeSize_100_500k,neg_rangeSize_100_500k,xx,xx,xx,xx),
           c(xx,xx,xx,pos_rangeSize_500k_1M, neg_rangeSize_500k_1M,xx,xx,xx),
           c(xx,xx,xx,xx,pos_rangeSize_1_2M, neg_rangeSize_1_2M, xx,xx),
           c(xx,xx,xx,xx,xx,pos_rangeSize_2_3M, neg_rangeSize_2_3M,xx),
           c(xx,xx,xx,xx,xx,xx,pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize_xx) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_rangeSize_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Min T Range Size')

text(x= bp[1], y = dt_rangeSize[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$minTslope_C > 0)))
text(x= bp[1], y = dt_rangeSize[2,1]/2 + dt_rangeSize[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$minTslope_C < 0)))

text(x= bp[2], y = dt_rangeSize[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$minTslope_C > 0)))
text(x= bp[2], y = dt_rangeSize[2,2]/2 + dt_rangeSize[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$minTslope_C < 0)))

text(x= bp[3], y= dt_rangeSize[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$minTslope_C > 0)))
text(x= bp[3], y= dt_rangeSize[2,3]/2 + dt_rangeSize[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$minTslope_C < 0)))

text(x= bp[4], y= dt_rangeSize[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$minTslope_C > 0)))
text(x= bp[4], y= dt_rangeSize[2,4]/2 + dt_rangeSize[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$minTslope_C < 0)))

text(x= bp[5], y= dt_rangeSize[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$minTslope_C > 0)))
text(x= bp[5], y= dt_rangeSize[2,5]/2 + dt_rangeSize[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$minTslope_C < 0)))

text(x= bp[6], y= dt_rangeSize[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$minTslope_C > 0)))
text(x= bp[6], y= dt_rangeSize[2,6]/2 + dt_rangeSize[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$minTslope_C < 0)))

text(x= bp[7], y= dt_rangeSize[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$minTslope_C > 0)))
text(x= bp[7], y= dt_rangeSize[2,7]/2 + dt_rangeSize[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$minTslope_C < 0)))



############################
######### MEAN TEMP ########
############################


#eliminate 0s and NAs in the slope
res_temp_roundness <- res_temp[which(res_temp$meanTslope_C != 0),]

#### all roundness
res_temp_roundness_all <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0),]

pos_temp_roundness_all <- sum(res_temp_roundness_all$meanTslope_C > 0) /
  nrow(res_temp_roundness_all) * 100
neg_temp_roundness_all <- sum(res_temp_roundness_all$meanTslope_C < 0) /
  nrow(res_temp_roundness_all) * 100

#### 0 to 0.2
res_temp_roundness_0_02 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0 &
          res_temp_roundness$rangeRoundness < 0.2),]

pos_temp_roundness_0_02 <- sum(res_temp_roundness_0_02$meanTslope_C > 0) /
  nrow(res_temp_roundness_0_02) * 100
neg_temp_roundness_0_02 <- sum(res_temp_roundness_0_02$meanTslope_C < 0) /
  nrow(res_temp_roundness_0_02) * 100

#### 0.2 to 0.4
res_temp_roundness_02_04 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.2 &
          res_temp_roundness$rangeRoundness < 0.4),]

pos_temp_roundness_02_04 <- sum(res_temp_roundness_02_04$meanTslope_C > 0) /
  nrow(res_temp_roundness_02_04) * 100
neg_temp_roundness_02_04 <- sum(res_temp_roundness_02_04$meanTslope_C < 0) /
  nrow(res_temp_roundness_02_04) * 100

#### 0.4 to 0.6
res_temp_roundness_04_06 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.4 &
          res_temp_roundness$rangeRoundness < 0.6),]

pos_temp_roundness_04_06 <- sum(res_temp_roundness_04_06$meanTslope_C > 0) /
  nrow(res_temp_roundness_04_06) * 100
neg_temp_roundness_04_06 <- sum(res_temp_roundness_04_06$meanTslope_C < 0) /
  nrow(res_temp_roundness_04_06) * 100

#### 0.6 to 0.8
res_temp_roundness_06_08 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.6 &
          res_temp_roundness$rangeRoundness < 0.8),]

pos_temp_roundness_06_08 <- sum(res_temp_roundness_06_08$meanTslope_C > 0) /
  nrow(res_temp_roundness_06_08) * 100
neg_temp_roundness_06_08 <- sum(res_temp_roundness_06_08$meanTslope_C < 0) /
  nrow(res_temp_roundness_06_08) * 100

#### 0.8 to 1
res_temp_roundness_08_1 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.8),]

pos_temp_roundness_08_1 <- sum(res_temp_roundness_08_1$meanTslope_C > 0) /
  nrow(res_temp_roundness_08_1) * 100
neg_temp_roundness_08_1 <- sum(res_temp_roundness_08_1$meanTslope_C < 0) /
  nrow(res_temp_roundness_08_1) * 100

#create a matrix with the data
dt_round <- matrix(c(c(pos_temp_roundness_all, neg_temp_roundness_all),
                     c(pos_temp_roundness_0_02, neg_temp_roundness_0_02),
                     c(pos_temp_roundness_02_04, neg_temp_roundness_02_04),
                     c(pos_temp_roundness_04_06, neg_temp_roundness_04_06),
                     c(pos_temp_roundness_06_08, neg_temp_roundness_06_08),
                     c(pos_temp_roundness_08_1, neg_temp_roundness_08_1)),
                   ncol = 6)

#name the columns
colnames(dt_round) <-
  c('all', 'R_0_02', 'R_02_04', 'R_04_06', 'R_06_08', 'R_08_1')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_round_xx <- 
  matrix(c(c(pos_temp_roundness_all, neg_temp_roundness_all, xx,xx,xx,xx,xx),
          c(xx,pos_temp_roundness_0_02, neg_temp_roundness_0_02, xx,xx,xx,xx),
          c(xx,xx,pos_temp_roundness_02_04,neg_temp_roundness_02_04,xx,xx,xx),
          c(xx,xx,xx,pos_temp_roundness_04_06,neg_temp_roundness_04_06,xx,xx),
          c(xx,xx,xx,xx,pos_temp_roundness_06_08,neg_temp_roundness_06_08,xx),
          c(xx,xx,xx,xx,xx,pos_temp_roundness_08_1,neg_temp_roundness_08_1)),
         ncol = 6)

#name the columns
colnames(dt_round_xx) <-
  c('all', 'R_0_02', 'R_02_04', 'R_04_06', 'R_06_08', 'R_08_1')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_round_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Range Roundness')

text(x= bp[1], y = dt_round[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_all$meanTslope_C > 0)))
text(x= bp[1], y = dt_round[2,1]/2 + dt_round[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_all$meanTslope_C < 0)))

text(x= bp[2], y = dt_round[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_0_02$meanTslope_C > 0)))
text(x= bp[2], y = dt_round[2,2]/2 + dt_round[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_0_02$meanTslope_C < 0)))

text(x= bp[3], y= dt_round[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_02_04$meanTslope_C > 0)))
text(x= bp[3], y= dt_round[2,3]/2 + dt_round[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_02_04$meanTslope_C < 0)))

text(x= bp[4], y= dt_round[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_04_06$meanTslope_C > 0)))
text(x= bp[4], y= dt_round[2,4]/2 + dt_round[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_04_06$meanTslope_C < 0)))

text(x= bp[5], y= dt_round[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_06_08$meanTslope_C > 0)))
text(x= bp[5], y= dt_round[2,5]/2 + dt_round[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_06_08$meanTslope_C < 0)))

text(x= bp[6], y= dt_round[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_08_1$meanTslope_C > 0)))
text(x= bp[6], y= dt_round[2,6]/2 + dt_round[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_08_1$meanTslope_C < 0)))



#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_rangeSize <- res_temp[which(res_temp$meanTslope_C != 0),]

#### all amplitudes
res_temp_rangeSize_all <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_rangeSize_all$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_all) * 100
neg_rangeSize_all <- sum(res_temp_rangeSize_all$meanTslope_C < 0) /
  nrow(res_temp_rangeSize_all) * 100

#### 0 to 100000 km2
res_temp_rangeSize_0_100k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize < 100000),]

pos_rangeSize_0_100k  <- sum(res_temp_rangeSize_0_100k$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_0_100k) * 100
neg_rangeSize_0_100k  <- sum(res_temp_rangeSize_0_100k$meanTslope_C < 0) /
  nrow(res_temp_rangeSize_0_100k) * 100

#### 100000 to 500000 km2
res_temp_rangeSize_100_500k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 100000 &
          res_temp_rangeSize$rangeSize <= 500000),]

pos_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_100_500k) * 100
neg_rangeSize_100_500k  <- sum(res_temp_rangeSize_100_500k$meanTslope_C < 0) /
  nrow(res_temp_rangeSize_100_500k) * 100

#### 500000 to 1000000 km2
res_temp_rangeSize_500k_1M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 500000 &
          res_temp_rangeSize$rangeSize < 1000000),]

pos_rangeSize_500k_1M <- 
  sum(res_temp_rangeSize_500k_1M$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100
neg_rangeSize_500k_1M <-
  sum(res_temp_rangeSize_500k_1M $meanTslope_C < 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100

#### 1000000 to 2000000 km2
res_temp_rangeSize_1_2M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 1000000 &
          res_temp_rangeSize$rangeSize < 2000000),]

pos_rangeSize_1_2M <- 
  sum(res_temp_rangeSize_1_2M$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100
neg_rangeSize_1_2M <-
  sum(res_temp_rangeSize_1_2M $meanTslope_C < 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100

#### 2000000 to 3000000 km2
res_temp_rangeSize_2_3M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 2000000 &
          res_temp_rangeSize$rangeSize < 3000000),]

pos_rangeSize_2_3M <- 
  sum(res_temp_rangeSize_2_3M$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100
neg_rangeSize_2_3M <-
  sum(res_temp_rangeSize_2_3M $meanTslope_C < 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100

#### more than 3000000 km2
res_temp_rangeSize_3M  <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize  >= 3000000),]

pos_rangeSize_3M <- 
  sum(res_temp_rangeSize_3M$meanTslope_C > 0) /
  nrow(res_temp_rangeSize_3M) * 100
neg_rangeSize_3M <-
  sum(res_temp_rangeSize_3M $meanTslope_C < 0) /
  nrow(res_temp_rangeSize_3M ) * 100

#create a matrix with the data
dt_rangeSize <-
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all),
           c(pos_rangeSize_0_100k, neg_rangeSize_0_100k),
           c(pos_rangeSize_100_500k, neg_rangeSize_100_500k),
           c(pos_rangeSize_500k_1M, neg_rangeSize_500k_1M),
           c(pos_rangeSize_1_2M, neg_rangeSize_1_2M),
           c(pos_rangeSize_2_3M, neg_rangeSize_2_3M),
           c(pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_rangeSize_xx <- 
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all,xx,xx,xx,xx,xx,xx),
           c(xx,pos_rangeSize_0_100k, neg_rangeSize_0_100k, xx,xx,xx,xx,xx),
           c(xx,xx,pos_rangeSize_100_500k,neg_rangeSize_100_500k,xx,xx,xx,xx),
           c(xx,xx,xx,pos_rangeSize_500k_1M, neg_rangeSize_500k_1M,xx,xx,xx),
           c(xx,xx,xx,xx,pos_rangeSize_1_2M, neg_rangeSize_1_2M, xx,xx),
           c(xx,xx,xx,xx,xx,pos_rangeSize_2_3M, neg_rangeSize_2_3M,xx),
           c(xx,xx,xx,xx,xx,xx,pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize_xx) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_rangeSize_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Mean T Range Size')

text(x= bp[1], y = dt_rangeSize[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$meanTslope_C > 0)))
text(x= bp[1], y = dt_rangeSize[2,1]/2 + dt_rangeSize[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$meanTslope_C < 0)))

text(x= bp[2], y = dt_rangeSize[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$meanTslope_C > 0)))
text(x= bp[2], y = dt_rangeSize[2,2]/2 + dt_rangeSize[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$meanTslope_C < 0)))

text(x= bp[3], y= dt_rangeSize[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$meanTslope_C > 0)))
text(x= bp[3], y= dt_rangeSize[2,3]/2 + dt_rangeSize[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$meanTslope_C < 0)))

text(x= bp[4], y= dt_rangeSize[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$meanTslope_C > 0)))
text(x= bp[4], y= dt_rangeSize[2,4]/2 + dt_rangeSize[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$meanTslope_C < 0)))

text(x= bp[5], y= dt_rangeSize[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$meanTslope_C > 0)))
text(x= bp[5], y= dt_rangeSize[2,5]/2 + dt_rangeSize[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$meanTslope_C < 0)))

text(x= bp[6], y= dt_rangeSize[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$meanTslope_C > 0)))
text(x= bp[6], y= dt_rangeSize[2,6]/2 + dt_rangeSize[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$meanTslope_C < 0)))

text(x= bp[7], y= dt_rangeSize[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$meanTslope_C > 0)))
text(x= bp[7], y= dt_rangeSize[2,7]/2 + dt_rangeSize[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$meanTslope_C < 0)))




############################
######### MAX TEMP #########
############################


#eliminate 0s and NAs in the slope
res_temp_roundness <- res_temp[which(res_temp$maxTslope_C != 0),]

#### all roundness
res_temp_roundness_all <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0),]

pos_temp_roundness_all <- sum(res_temp_roundness_all$maxTslope_C > 0) /
  nrow(res_temp_roundness_all) * 100
neg_temp_roundness_all <- sum(res_temp_roundness_all$maxTslope_C < 0) /
  nrow(res_temp_roundness_all) * 100

#### 0 to 0.2
res_temp_roundness_0_02 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0 &
          res_temp_roundness$rangeRoundness < 0.2),]

pos_temp_roundness_0_02 <- sum(res_temp_roundness_0_02$maxTslope_C > 0) /
  nrow(res_temp_roundness_0_02) * 100
neg_temp_roundness_0_02 <- sum(res_temp_roundness_0_02$maxTslope_C < 0) /
  nrow(res_temp_roundness_0_02) * 100

#### 0.2 to 0.4
res_temp_roundness_02_04 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.2 &
          res_temp_roundness$rangeRoundness < 0.4),]

pos_temp_roundness_02_04 <- sum(res_temp_roundness_02_04$maxTslope_C > 0) /
  nrow(res_temp_roundness_02_04) * 100
neg_temp_roundness_02_04 <- sum(res_temp_roundness_02_04$maxTslope_C < 0) /
  nrow(res_temp_roundness_02_04) * 100

#### 0.4 to 0.6
res_temp_roundness_04_06 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.4 &
          res_temp_roundness$rangeRoundness < 0.6),]

pos_temp_roundness_04_06 <- sum(res_temp_roundness_04_06$maxTslope_C > 0) /
  nrow(res_temp_roundness_04_06) * 100
neg_temp_roundness_04_06 <- sum(res_temp_roundness_04_06$maxTslope_C < 0) /
  nrow(res_temp_roundness_04_06) * 100

#### 0.6 to 0.8
res_temp_roundness_06_08 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.6 &
          res_temp_roundness$rangeRoundness < 0.8),]

pos_temp_roundness_06_08 <- sum(res_temp_roundness_06_08$maxTslope_C > 0) /
  nrow(res_temp_roundness_06_08) * 100
neg_temp_roundness_06_08 <- sum(res_temp_roundness_06_08$maxTslope_C < 0) /
  nrow(res_temp_roundness_06_08) * 100

#### 0.8 to 1
res_temp_roundness_08_1 <- res_temp_roundness[
  which(res_temp_roundness$rangeRoundness >= 0.8),]

pos_temp_roundness_08_1 <- sum(res_temp_roundness_08_1$maxTslope_C > 0) /
  nrow(res_temp_roundness_08_1) * 100
neg_temp_roundness_08_1 <- sum(res_temp_roundness_08_1$maxTslope_C < 0) /
  nrow(res_temp_roundness_08_1) * 100

#create a matrix with the data
dt_round <- matrix(c(c(pos_temp_roundness_all, neg_temp_roundness_all),
                     c(pos_temp_roundness_0_02, neg_temp_roundness_0_02),
                     c(pos_temp_roundness_02_04, neg_temp_roundness_02_04),
                     c(pos_temp_roundness_04_06, neg_temp_roundness_04_06),
                     c(pos_temp_roundness_06_08, neg_temp_roundness_06_08),
                     c(pos_temp_roundness_08_1, neg_temp_roundness_08_1)),
                   ncol = 6)

#name the columns
colnames(dt_round) <-
  c('all', 'R_0_02', 'R_02_04', 'R_04_06', 'R_06_08', 'R_08_1')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_round_xx <- 
  matrix(c(c(pos_temp_roundness_all, neg_temp_roundness_all, xx,xx,xx,xx,xx),
          c(xx,pos_temp_roundness_0_02,neg_temp_roundness_0_02,xx,xx,xx,xx),
          c(xx,xx,pos_temp_roundness_02_04,neg_temp_roundness_02_04,xx,xx,xx),
          c(xx,xx,xx,pos_temp_roundness_04_06,neg_temp_roundness_04_06,xx,xx),
          c(xx,xx,xx,xx,pos_temp_roundness_06_08,neg_temp_roundness_06_08,xx),
          c(xx,xx,xx,xx,xx,pos_temp_roundness_08_1,neg_temp_roundness_08_1)),
         ncol = 6)

#name the columns
colnames(dt_round_xx) <-
  c('all', 'R_0_02', 'R_02_04', 'R_04_06', 'R_06_08', 'R_08_1')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_round_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Max T Range Roundness')

text(x= bp[1], y = dt_round[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_all$maxTslope_C > 0)))
text(x= bp[1], y = dt_round[2,1]/2 + dt_round[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_all$maxTslope_C < 0)))

text(x= bp[2], y = dt_round[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_0_02$maxTslope_C > 0)))
text(x= bp[2], y = dt_round[2,2]/2 + dt_round[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_0_02$maxTslope_C < 0)))

text(x= bp[3], y= dt_round[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_02_04$maxTslope_C > 0)))
text(x= bp[3], y= dt_round[2,3]/2 + dt_round[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_02_04$maxTslope_C < 0)))

text(x= bp[4], y= dt_round[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_04_06$maxTslope_C > 0)))
text(x= bp[4], y= dt_round[2,4]/2 + dt_round[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_04_06$maxTslope_C < 0)))

text(x= bp[5], y= dt_round[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_06_08$maxTslope_C > 0)))
text(x= bp[5], y= dt_round[2,5]/2 + dt_round[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_06_08$maxTslope_C < 0)))

text(x= bp[6], y= dt_round[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_08_1$maxTslope_C > 0)))
text(x= bp[6], y= dt_round[2,6]/2 + dt_round[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_roundness_08_1$maxTslope_C < 0)))



#save boxplot in /Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Plots/Plots_all_species/Box_plots

###### check relationships by range size

#eliminate 0s and NAs in the slope
res_temp_rangeSize <- res_temp[which(res_temp$maxTslope_C != 0),]

#### all amplitudes
res_temp_rangeSize_all <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 0),]

pos_rangeSize_all <- sum(res_temp_rangeSize_all$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_all) * 100
neg_rangeSize_all <- sum(res_temp_rangeSize_all$maxTslope_C < 0) /
  nrow(res_temp_rangeSize_all) * 100

#### 0 to 100000 km2
res_temp_rangeSize_0_100k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize < 100000),]

pos_rangeSize_0_100k  <- sum(res_temp_rangeSize_0_100k$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_0_100k) * 100
neg_rangeSize_0_100k  <- sum(res_temp_rangeSize_0_100k$maxTslope_C < 0) /
  nrow(res_temp_rangeSize_0_100k) * 100

#### 100000 to 500000 km2
res_temp_rangeSize_100_500k <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 100000 &
          res_temp_rangeSize$rangeSize <= 500000),]

pos_rangeSize_100_500k <- sum(res_temp_rangeSize_100_500k$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_100_500k) * 100
neg_rangeSize_100_500k  <- sum(res_temp_rangeSize_100_500k$maxTslope_C < 0) /
  nrow(res_temp_rangeSize_100_500k) * 100

#### 500000 to 1000000 km2
res_temp_rangeSize_500k_1M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 500000 &
          res_temp_rangeSize$rangeSize < 1000000),]

pos_rangeSize_500k_1M <- 
  sum(res_temp_rangeSize_500k_1M$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100
neg_rangeSize_500k_1M <-
  sum(res_temp_rangeSize_500k_1M $maxTslope_C < 0) /
  nrow(res_temp_rangeSize_500k_1M ) * 100

#### 1000000 to 2000000 km2
res_temp_rangeSize_1_2M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 1000000 &
          res_temp_rangeSize$rangeSize < 2000000),]

pos_rangeSize_1_2M <- 
  sum(res_temp_rangeSize_1_2M$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100
neg_rangeSize_1_2M <-
  sum(res_temp_rangeSize_1_2M $maxTslope_C < 0) /
  nrow(res_temp_rangeSize_1_2M ) * 100

#### 2000000 to 3000000 km2
res_temp_rangeSize_2_3M <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize >= 2000000 &
          res_temp_rangeSize$rangeSize < 3000000),]

pos_rangeSize_2_3M <- 
  sum(res_temp_rangeSize_2_3M$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100
neg_rangeSize_2_3M <-
  sum(res_temp_rangeSize_2_3M $maxTslope_C < 0) /
  nrow(res_temp_rangeSize_2_3M ) * 100

#### more than 3000000 km2
res_temp_rangeSize_3M  <- res_temp_rangeSize[
  which(res_temp_rangeSize$rangeSize  >= 3000000),]

pos_rangeSize_3M <- 
  sum(res_temp_rangeSize_3M$maxTslope_C > 0) /
  nrow(res_temp_rangeSize_3M) * 100
neg_rangeSize_3M <-
  sum(res_temp_rangeSize_3M $maxTslope_C < 0) /
  nrow(res_temp_rangeSize_3M ) * 100

#create a matrix with the data
dt_rangeSize <-
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all),
           c(pos_rangeSize_0_100k, neg_rangeSize_0_100k),
           c(pos_rangeSize_100_500k, neg_rangeSize_100_500k),
           c(pos_rangeSize_500k_1M, neg_rangeSize_500k_1M),
           c(pos_rangeSize_1_2M, neg_rangeSize_1_2M),
           c(pos_rangeSize_2_3M, neg_rangeSize_2_3M),
           c(pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

# workaround: extend the matrix so that values correspond to fictitious
# categories, with just one colour per category
xx <- rep(0,2)

#create amother matrix including the workaround 0s
dt_rangeSize_xx <- 
  matrix(c(c(pos_rangeSize_all, neg_rangeSize_all,xx,xx,xx,xx,xx,xx),
           c(xx,pos_rangeSize_0_100k, neg_rangeSize_0_100k, xx,xx,xx,xx,xx),
           c(xx,xx,pos_rangeSize_100_500k,neg_rangeSize_100_500k,xx,xx,xx,xx),
           c(xx,xx,xx,pos_rangeSize_500k_1M, neg_rangeSize_500k_1M,xx,xx,xx),
           c(xx,xx,xx,xx,pos_rangeSize_1_2M, neg_rangeSize_1_2M, xx,xx),
           c(xx,xx,xx,xx,xx,pos_rangeSize_2_3M, neg_rangeSize_2_3M,xx),
           c(xx,xx,xx,xx,xx,xx,pos_rangeSize_3M, neg_rangeSize_3M)),
         ncol = 7)


#name the columns
colnames(dt_rangeSize_xx) <-
  c('all', 'S_0_100K', 'S_100_500K', 'S_500_1M', 'S_1_2M', 'S_2_3M', 'S_3M')

#make the very weird colour list
col = c(c('#0000FF80', "#FF000080"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"),
        c('#0000FF', "#FF0000"))

bp <- barplot(dt_rangeSize_xx,
              col = col,
              cex.axis = 1.5, cex.names = 1, cex.main = 1.5,
              legend = NA, main = 'Max T Range Size')

text(x= bp[1], y = dt_rangeSize[1,1]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$maxTslope_C > 0)))
text(x= bp[1], y = dt_rangeSize[2,1]/2 + dt_rangeSize[1,1],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_all$maxTslope_C < 0)))

text(x= bp[2], y = dt_rangeSize[1,2]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$maxTslope_C > 0)))
text(x= bp[2], y = dt_rangeSize[2,2]/2 + dt_rangeSize[1,2],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_0_100k$maxTslope_C < 0)))

text(x= bp[3], y= dt_rangeSize[1,3]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$maxTslope_C > 0)))
text(x= bp[3], y= dt_rangeSize[2,3]/2 + dt_rangeSize[1,3],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_100_500k$maxTslope_C < 0)))

text(x= bp[4], y= dt_rangeSize[1,4]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$maxTslope_C > 0)))
text(x= bp[4], y= dt_rangeSize[2,4]/2 + dt_rangeSize[1,4],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_500k_1M$maxTslope_C < 0)))

text(x= bp[5], y= dt_rangeSize[1,5]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$maxTslope_C > 0)))
text(x= bp[5], y= dt_rangeSize[2,5]/2 + dt_rangeSize[1,5],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_1_2M$maxTslope_C < 0)))

text(x= bp[6], y= dt_rangeSize[1,6]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$maxTslope_C > 0)))
text(x= bp[6], y= dt_rangeSize[2,6]/2 + dt_rangeSize[1,6],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_2_3M$maxTslope_C < 0)))

text(x= bp[7], y= dt_rangeSize[1,7]/2, 
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$maxTslope_C > 0)))
text(x= bp[7], y= dt_rangeSize[2,7]/2 + dt_rangeSize[1,7],
     pos = 1, font = 2, col = '#FFFFFF', cex = 1.5,
     labels = paste(sum(res_temp_rangeSize_3M$maxTslope_C < 0)))

