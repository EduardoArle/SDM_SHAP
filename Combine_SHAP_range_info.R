####  This script combines the SHAP results for each species with the info
####  about range characteristics and position of points within the ranges

#load libraries

#list wds
wd_res_shap <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Comparison'
wd_pts_measure <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Point_and_range_measurements'
wd_res <- '/Users/carloseduardoaribeiro/Documents/Post-doc/SHAP/Mammals/Results/Results_analyses/Organised results'

#list species 
setwd(wd_pts_measure)
sps_list <- gsub('_range_metrics.csv', '', list.files())
