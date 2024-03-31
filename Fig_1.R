#load packages

#list wds


##### PLOT MAP #####

##### PLOT GRAPH ######

#create data for plot
varContr <- c(rnorm(10, mean = 0, sd = 0.2),
              rnorm(10, mean = 0.2, sd = 0.2),
              rnorm(10, mean = 0.3, sd = 0.2),
              rnorm(10, mean = 0.4, sd = 0.2),
              rnorm(10, mean = 0.5, sd = 0.2))

relPolewardness <- c(1:50 / 50)

#set y and x lims
ylim <- c(-0.2, 0.5)
xlim <- c(0, 1)

#set parametres for plotting
par(mar = c(5,5,5,5), pty="s")

#minT
plot(relPolewardness, varContr, 
     pch = 19, cex = 0.4, col = '#0000FF15',
     ylab = 'Temperature contribution',
     xlab = 'Relative polarwardness',
     ylim = c(ylim[1], ylim[2]))

#fit linear model
lin_mod_minT <- lm(varContr ~ relPolewardness)
abline(lin_mod_minT, col = '#0000FF', lwd = 3)


summa <- summary(lin_mod_minT)
r2 <- summa$r.squared
r2




#get y and x lims
ylim <- range(c(cont_minT, cont_meanT, cont_maxT))
xlim <- range(sps_minT_SHAP_info[[j]]$relPolarwardness) #no matter which

#minT
plot(sps_minT_SHAP_info[[j]]$relPolarwardness, cont_minT, 
     pch = 19, cex = 0.4, col = '#0000FF15',
     ylab = 'Temperature contribution',
     xlab = 'Relative polarwardness',
     ylim = c(ylim[1], ylim[2]))

#fit linear model
lin_mod_minT <- lm(cont_minT ~ sps_minT_SHAP_info[[j]]$relPolarwardness)
abline(lin_mod_minT, col = '#0000FF', lwd = 2)
