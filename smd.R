library(sdm); library(raster); library(rgdal)

# get the location of the species
file <- system.file("external/species.shp", package="sdm")

# so, file is simply a filename (with path):
file

# read the species shapefile using the function shapefile:
species <- shapefile(file)
class(species) # it is a SpatialPointsDataFrame
plot(species)

head(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16) 
points(species[species$Occurrence == 0,],col='red',pch=16)


# Let's read predictor variables (raster datasets)
# We have four Ascii-Grids, so, let's just take the name of all files ending to '.asc' to
path <- system.file("external", package="sdm") # path to the folder contains the data
lst <- list.files(path=path,pattern='asc$',full.names = T) # list the name of files in the
lst # this is the name of raster files we want to use as predictor variables

# stack is a function in the raster package, to read/create a multi-layers raster dataset
preds <- stack(lst) # making a raster object
preds # see the specification of the raster layers (e.g., cell size, extent, etc.)

plot(preds)

plot(preds[[4]]) # only plot the 4th layer
plot(species,add=T) # let's add the species point on the previous plot

### prepare the data

d <- sdmData(formula=Occurrence~., train=species, predictors=preds) 
d

##### probably useful

# You may also want to take part of variables:
# d <- sdmData(formula=Occurrence~precipitation+temperature, train=species, predictors=preds 
# d

m1 <- sdm(Occurrence~.,data=d,methods=c('rf')) 
m1

# Here we are going to fit 4 models and evaluate them through 2 runs of subsampling, each 
m2 <- sdm(Occurrence~.,data=d,methods=c('rf'),replicatin='sub',test.percent=30,n=2)
m2

getModelInfo(m2)

roc(m2)

p1 <- predict(m2,newdata=preds) # many commonly used raster format is su 
plot(p1)
