#load packages

library(shapviz); library(kernelshap)


##
library(ggplot2)
library(xgboost)

### examples


set.seed(3653)

# Turn ordinal factors into normal ones
ord <- c("clarity", "cut", "color")
diamonds[, ord] <- lapply(diamonds[, ord], factor, ordered = FALSE)

# Fit XGBoost model
x <- c("carat", "clarity", "cut", "color")
dtrain <- xgb.DMatrix(data.matrix(diamonds[x]), label = diamonds$price)

fit <- xgb.train(
  params = list(learning_rate = 0.1, objective = "reg:squarederror"), 
  data = dtrain,
  nrounds = 65L
)

# Pick explanation data
dia_small <- diamonds[sample(nrow(diamonds), 2000L), ]

# We also pass feature data X with originally encoded values
shp <- shapviz(fit, X_pred = data.matrix(dia_small[x]), X = dia_small)

sv_waterfall(shp, row_id = 1L) +
  theme(axis.text = element_text(size = 11))

sv_force(shp, row_id = 1L)

##########
s <- kernelshap(model, data_test[-1], bg_X = data_test)
sv <- shapviz(s) # Step 2: Turn them into a shapviz ggplot object