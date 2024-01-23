sps_minT_SHAP_info[[j]]$relPolarwardness

sps_meanT_SHAP_info[[j]]$absPolarwardness

sps_maxT_SHAP_info[[j]]$absPolarwardness

library(data.table)

df1 <- data.frame(var_cont = sps_minT_SHAP_info[[j]]$Min_T_SHAP,
                  measurement = 'min T')

df2 <- data.frame(var_cont = sps_meanT_SHAP_info[[j]]$Mean_T_SHAP,
                  measurement = 'mean T')
                  
df3 <- data.frame(var_cont = sps_maxT_SHAP_info[[j]]$Max_T_SHAP,
                  measurement = 'max T')

df4 <- list(df1, df2, df3) 

df5 <-  rbindlist(df4)


par(mfrow = c(1,1), mar = c(5,5,5,5))

boxplot(df5$var_cont ~ df5$measurement,
        ylab = 'Current.Velocity.Mean', xlab = NA, 
        col = c('#8e6797','#7abcbc'),names = NA, par(cex.lab = 1.5))
