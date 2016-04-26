######

#Common Myna ENM Project
#Rob Pecchia
#Zoogeography Spring 2016

#####

library(dismo)
library(dplyr)
library(maptools)
library(spThin)
library(ggplot2)
library(rgdal)
library(ENMeval)
library(scales)
library(rgeos)
library(rgdal)
data(wrld_simpl)

setwd("/Users/rpecchia/Desktop/Zoogeo Spring 2016/zoogeo-project")

####

# Download Occurrences

####

#myna_from_gbif <- gbif("Acridotheres", "tristis*", geo = TRUE)
# adding * to species will download all subspecies
# if geo = TRUE, ensures all records have latitude AND longitude
dim(myna_from_gbif)
myna_no_absences<-myna_from_gbif[!grepl("EBCC Atlas of European Breeding Birds", myna_from_gbif$datasetName),] #remove entries from EBCC
unique(myna_no_absences$country) #check to make sure absence records removed
myna_only_human_obs<-myna_no_absences[grepl("HUMAN_OBSERVATION", myna_no_absences$basisOfRecord),] #remove entries not observed by human
unique(myna_only_human_obs$basisOfRecord)
myna_points <- select(myna_only_human_obs, species, lon, lat, country) #select only necessary columns
head(myna_points)
myna_points <- myna_points[complete.cases(myna_points),] #ensure no entries are missing lat/lon/country

write.csv(myna_points, file = "myna_from_gbif.csv") #write file of species name and occurrence data

plot(wrld_simpl, axes=TRUE, col="light yellow", main = "Occurrences for Common Myna \n(Acridotheres tristis) from GBIF")
points(myna_points$lon, myna_points$lat ,col = "blue", cex = .35)

myna_unique <- unique(myna_points) #returns only unique combinations of myna points

plot(wrld_simpl, axes=TRUE, col="light yellow", main = "Occurrences for Common Myna \n(Acridotheres tristis) from GBIF")
points(myna_unique$lon, myna_unique$lat ,col = "blue", cex = .35)

####

# filtering out sightings

####
# filter(myna_unique, lat>(80)) #find northern midwest point
# points(22.01667,88.01667) #confirm this is northernmost point
# which(myna_unique$lon == 22.01667, myna_unique$lat== 88.01667) #find it in the data.frame
# myna_unique<- myna_unique[-30623,] #remove that row!
dim(myna_unique)

unique(myna_unique$country)
myna_unique <- subset(myna_unique, country != "Mexico")
myna_unique <- subset(myna_unique, country != "Austria")
myna_unique <- subset(myna_unique, country != "Brunei")
myna_unique <- subset(myna_unique, country != "Namibia")
myna_unique <- subset(myna_unique, country != "Papua New Guinea")
myna_unique <- subset(myna_unique, country != "France")
myna_unique <- subset(myna_unique, country != "Wallis and Futuna")
myna_unique <- subset(myna_unique, country != "Hong Kong")

plot(wrld_simpl, axes=TRUE, col="light yellow", main = "Occurrences for Common Myna \n(Acridotheres tristis) from GBIF")
points(myna_unique$lon, myna_unique$lat ,col = "blue", cex = .35)

#####

# Spatial Thinning

#####

crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') # set coordinate system

myna_thin <-spThin( #thinning function
  myna_unique, 
  x.col = "lon",
  y.col = "lat",
  dist = 5000,
  method= "gurobi", #can change to "gurobi" to make it even faster, but have to install it first
  great.circle.distance=TRUE)
summary(myna_thin)

# Saving the thinned file####
print(tempdir())
write.SpThin(
  myna_thin,
  coords=FALSE,
  dir=tempdir()
)

#read in .csv of all thinned points
thin_myna2<-read.csv("thin_0001.csv", head=T)
head(thin_myna2)
thin_myna2<-thin_myna2[,1:3]
head(thin_myna2)
dim(thin_myna2)

####

# Download environmental data

####

#worldclim <- getData('worldclim', var='bio', res=2.5)
plot(worldclim[[1]], main = "Annual Mean Temp \n and Common Myna Occurrences")
points(thin_myna2$lon, thin_myna2$lat ,col = "blue", cex = .25)
points(points(myna_unique$lon, myna_unique$lat ,col = "blue", cex = .25))

plot(worldclim[[19]], main = "Precip in Coldest Quarter \n and Common Myna Occurrences")
points(points(myna_unique$lon, myna_unique$lat ,col = "blue", cex = .25))

#Pop density from SEDAC @ Columbia
population<-raster(paste(getwd(), "/Pop_density_2000/pop2.tif", sep = ""))
worldclim_and_pop <- stack(worldclim, population)

#Prepare Training and Testing dataset####

# k-fold
folds<-kfold(thin_myna_coords, k=4) #this is a 4 fold test

#random split
train<-thin_myna_coords[folds>1,] #training has 75% of points
test<-thin_myna_coords[folds==1,] #testing has 25% of points
train<-train[,1:2]
test<-test[,1:2]
head(train) #just has lon/lat

#plot training dataset
plot(wrld_simpl)
cols_red<-"red"
points(train, col = alpha(cols_red, 0.4), cex = .5)

#overlay testing dataset
cols_blue<-"blue"
points(test, col = alpha(cols_blue, 0.4), cex = .5)

####

#Making the BioClim Model

####
# myna_bioclim<-bioclim(worldclim,thin_myna_coords) #this runs and saves the BIOCLIM model as an object
# myna_bioclim_predictions <- predict(myna_bioclim, worldclim, progress='text')
# plot(myna_bioclim_predictions)
# 
# myna_bioclim_predictions_as_points <- rasterToPoints(myna_bioclim_predictions) #make predictions raster a set of points for ggplot
# df_myna_bioclim_predictions <- data.frame(myna_bioclim_predictions_as_points) #convert to data.frame
# head(df_myna_bioclim_predictions)
# colnames(df_myna_bioclim_predictions) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
# max(df_myna_bioclim_predictions$Suitability)

#####

# MaxEnt Model

####

#Bounding Box Method
min(thin_myna_coords$lon)
min(thin_myna_coords$lat)
max(thin_myna_coords$lon)
max(thin_myna_coords$lat)

# Jamie's mcp function
# mcp_myna <- mcp(train)
# plot(wrld_simpl)
# points(thin_myna_coords, col = "cyan4", cex = .5)
# plot(mcp_myna, add = T)

#getting background points from mcp
# env_mask <- mask(worldclim_and_pop, mcp_myna) #mask takes all values that are not null, and returns
# env_crop <- crop(env_mask, mcp_myna)
# plot(env_crop[[1]])
# backg<-randomPoints(env_crop[[1]],n=10000)

# plot(wrld_simpl)
# points(backg, col = "darkorchid3", cex = .05)

#now k-fold partition background points
# colnames(backg) = c('lon' , 'lat')
# group <- kfold(backg, 4)
# backg_train <- backg[group != 1, ]
# backg_test <- backg[group == 1, ]

####

#Beth's Method for selecting background points from crop circles

####

# # Generate 1 degree buffered 'crop circle'. occ.sp is a csv file of your occurrence records

#convert list of occurrences to sp.df
myna_occurrences_spdf <- SpatialPointsDataFrame(coords = thin_myna_coords, data = thin_myna_coords,
                                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(myna_occurrences_spdf)
# backg_buffered_extent <- gBuffer(myna_occurrences_spdf, width = 1)
# plot(backg_buffered_extent)
# 
# Crop and mask environmental variables by the 'crop circle' extent
# env_msk is a stack of the environmental data rasters
# env_msk_bg <- mask(worldclim_and_pop, backg_buffered_extent)
# plot(env_msk_bg[[1]])
# set.seed(1)
# backg.env <- randomPoints(env_msk_bg[[1]], n=10000, tryf = 50)
# plot(wrld_simpl)
# points(backg.env, col = "red", cex = .05)

# # k-fold partition background points
# #now k-fold partition background points
# colnames(backg.env) = c('lon' , 'lat')
# group <- kfold(backg, 4)
# backg_train <- backg[group != 1, ]
# backg_test <- backg[group == 1, ]

####

# ENMeval

####
## ENMEVAL TIPS####
#always keep a CSV of the same background!
#AUC full is ACU of all localities
#Mean. AUC over all iterations, then variation of AUC
#mean ormission rate at 10% threshold
#number of parameters used (is from the lambda file). #when selecting AIC, go with Delta AIC = 0
#look @results
#subset to get rid of rows with AUC, take the one with lowest delta.AICc, then plug in optimal parameters
# always keep a CSV of the same background!
# AUC full is ACU of all localities
# Mean. AUC over all iterations, then variation of AUC
# mean ormission rate at 10% threshold
# number of parameters used (is from the lambda file). #when selecting AIC, go with Delta AIC = 0
# enmeval_results <- ENMevaluate(thin_myna_coors, env = worldclim_and_pop, bg.coords = backg_five_degree ,method="block", overlap=TRUE, bin.output=TRUE, clamp=TRUE)
# enmeval_results_only <- enmeval_results@results
# save(enmeval_results, file="enmeval_results_worldclim.rdata")

#look at ENMeval for WorldClim
# plot(enmeval_results@predictions[[which (enmeval_results_worldclim@results$delta.AICc == 0) ]])
# points(enmeval_results@occ.pts, pch=21, bg=enmeval_results@occ.grp)
# head(enmeval_results@results)
# enmeval_results@results #all the results
# Q_enmeval<-enmeval_results@results#arrange by AICc value
# QQ_enmeval<-as.data.frame(Q_enmeval)
# head(QQ_enmeval)
# QQ_enmeval<-QQ_enmeval[,c(1,2,3,14)]
# head(QQ_enmeval)
# arrange(QQ_enmeval,AICc,settings,features,rm) #this will sort ENMeval results so that we can see exact settings for model with lowest AICc
# # #Shows that model with LQ ranging from .5-4.0 all had the lowest AICc
# enmeval_resultsm@overlap

# # ENMeval Figures
# par(mfrow=c(2,2))
# eval.plot(enmeval_results@results, legend.position="topright")
# eval.plot(enmeval_results@results, "Mean.AUC")
# eval.plot(enmeval_results@results, "Mean.AUC.DIFF", variance="Var.AUC.DIFF")
# eval.plot(enmeval_results@results, "Mean.ORmin")
# enmeval_results@results
# specify how data should be partitioned w/ method="jackknife", "randomkfold", "user", "block", "checkerboard1", "checkerboard2".
# n.bg is The number of random background localities to draw from the study extent
# when overlap = TRUE, provides pairwise metric of niche overlap 
# bin.output appends evaluations metrics for each evaluation bin to results table

#subset to get rid of rows with AUC, take the one with lowest delta.AICc, then plug in optimal parameters

# Which settings gave delta.AICc < 2?
aicmods <- which(res@results$delta.AICc < 2)
res@results[aicmods,]

# Visualize how data were partitioned
# Background points:
plot(env[[2]], xlim=c(-67.5,-65.5))
points(res@bg.pts, col= res@bg.grp, cex=.75)

# Occurrence localities:
plot(res@predictions[[which(res@results$delta.AICc == 0)]], xlim=c(-67.5,-65.5))
points(res@occ.pts, pch=16, col= res@occ.grp, cex=.75)

# View predictions in geographic space for these models
plot(res@predictions[[aicmods]])

####

# MaxEnt with default settings, MCH background

####

#Split sample
plot(backg)
plot(wrld_simpl)
points(train)
mx_myna <- maxent(worldclim_and_pop, train, a = backg, args=c('responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_myna)
plot(mx_myna)
mx_myna@results

#Model Evaluation 
e_myna <- evaluate(test, backg, mx_myna, worldclim_and_pop) #evalute test points, pseudo-absences (random background points), the model and predictors
e_myna_with_threshold <- evaluate(test, backg, mx_myna, worldclim_and_pop, tr = 0.3495931)
boxplot(e_myna)
density(e_myna)

boxplot(e_myna_with_threshold)
e_myna_with_threshold@confusion #confusion matrix

#k-fold (10-fold cross validation)
mx_myna_k_fold <- maxent(worldclim_and_pop, thin_myna_coords, a = backg, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_k_fold@results

#all occurrences for final model
mx_myna_all_occs <- maxent(worldclim_and_pop, thin_myna_coords, a = backg, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_myna_all_occs@lambdas
mx_myna_all_occs@results
plot(mx_myna_all_occs)

px_myna_all_occs <- predict(worldclim_and_pop, mx_myna_all_occs) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna_all_occs, main= 'Maxent, raw values')
writeRaster(px_myna_all_occs, filename="myna_all_occs_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

#Plotting Maxent output
map_myna_all_occs <- rasterToPoints(px_myna_all_occs) #make predictions raster a set of points for ggplot
df_myna_all_occs <- data.frame(map_myna_all_occs) #convert to data.frame
head(df_myna_all_occs)
colnames(df_myna_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
max(df_myna_all_occs$Suitability)
plot(wrld_simpl)
points(filter(df_myna_all_occs, Suitability >= .69), col="red")

## Threshold w/ minimum 10% training presence
training_suitability_myna <- extract(px_myna_all_occs, thin_myna_coords) #all predicted values, all occs
ten_thresh_myna <- quantile(training_suitability_myna, 0.1, na.rm = TRUE)
ten_thresh_myna

#creating binary map
threshold_map <- px_myna_all_occs > ten_thresh_myna
plot(px_myna_all_occs > ten_thresh_myna)
plot(threshold_map)
writeRaster(threshold_map, filename="myna_threshold_map.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

#### 

# Maxent w/ Background from Beth's crop circles

####

#one degree crop circle
mx_myna_crop_k_fold <- maxent(env_msk_bg, thin_myna_coords, a = backg.env, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_crop_k_fold@results

#one degree crop circle -- no crop circles in env data, only background ###PROVIDES SAME RESULTS AS ABOVE####
mx_myna_cropcicle_nocropbg_k_fold <- maxent(worldclim_and_pop, thin_myna_coords, a = backg.env, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_cropcicle_nocropbg_k_fold@results

#one degree crop circle, no
mx_myna_no_pop_k_fold <- maxent(worldclim, thin_myna_coords, a = backg.env, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_no_pop_k_fold@results

plot(wrld_simpl)
points(thin_myna_coords, col = "red", cex = .05)
points(backg.env, col = "blue", cex = .05)
#all occurrences for final modle
mx_myna_all_occs <- maxent(worldclim_and_pop, thin_myna_coords, a = backg, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
mx_myna_all_occs@lambdas
mx_myna_all_occs@results
plot(mx_myna_all_occs)

px_myna_all_occs <- predict(worldclim_and_pop, mx_myna_all_occs) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna_all_occs, main= 'Maxent, raw values')
writeRaster(px_myna_all_occs, filename="myna_all_occs_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

#Plotting Maxent output
map_myna_all_occs <- rasterToPoints(px_myna_all_occs) #make predictions raster a set of points for ggplot
df_myna_all_occs <- data.frame(map_myna_all_occs) #convert to data.frame
head(df_myna_all_occs)
colnames(df_myna_all_occs) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
max(df_myna_all_occs$Suitability)
plot(wrld_simpl)
points(filter(df_myna_all_occs, Suitability >= .69), col="red")

## Threshold w/ minimum 10% training presence
training_suitability_myna <- extract(px_myna_all_occs, thin_myna_coords) #all predicted values, all occs
ten_thresh_myna <- quantile(training_suitability_myna, 0.1, na.rm = TRUE)
ten_thresh_myna

#creating binary map
threshold_map <- px_myna_all_occs > ten_thresh_myna
plot(px_myna_all_occs > ten_thresh_myna)
plot(threshold_map)
writeRaster(threshold_map, filename="myna_threshold_map.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff
?predict

####

# Two degree crop circle

###
#write the SPDF of myna points for QGIS
# writeOGR(myna_occurrences_spdf, dsn = "data/",layer = "myna_csv_spdf_as_shp", driver = "ESRI Shapefile")
# 
# #now read in the file w/ buffers
# buffered_region <- readGDAL("data/buffer_layer_cropped.tif")
# projection(buffered_region)
# 
# 
# #convert buffered region to raster
# buffered_region_raster <- raster(buffered_region) #convert africa map to raster
# backg_two_degree <- randomPoints(buffered_region_raster, n=10000)
# 
# plot(wrld_simpl)
# points(backg_two_degree, col = "red", cex = 0.5)
# 
# mx_myna_crop_k_fold_qgis <- maxent(worldclim_and_pop, thin_myna_coords, a = backg_two_degree, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# mx_myna_crop_k_fold_qgis@results
# 
# #Maxent, 2 degree buffer 20,000 background 
# backg_two_degree_twenty_thousand_bkg <- randomPoints(buffered_region_raster, n=20000)
# 
# plot(wrld_simpl)
# points(backg_two_degree_twenty_thousand_bkg, col = "red", cex = 0.25)
# 
# mx_myna_crop_k_fold_qgis_twenty_thousand_backg <- maxent(worldclim_and_pop, thin_myna_coords, a = backg_two_degree_twenty_thousand_bkg, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
# mx_myna_crop_k_fold_qgis_twenty_thousand_backg@results
# 
# 
####

# Three degree crop circle

###

#now read in the file w/ buffers
buffered_region_three <- readGDAL("data/buffer_layer_three_degree.tif")
projection(buffered_region_three)

#convert buffered region to raster
buffered_region_raster_three <- raster(buffered_region_three) #convert africa map to raster
backg_three_degree <- randomPoints(buffered_region_raster_three, n=20000)

plot(wrld_simpl)
points(backg_three_degree, col = "red", cex = 0.5)

mx_myna_crop_k_fold_qgis_three <- maxent(worldclim_and_pop, thin_myna_coords, a = backg_three_degree, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_crop_k_fold_qgis_three@results

####

# four degree crop circle

####

#now read in the file w/ buffers
buffered_region_four <- readGDAL("data/buffer_layer_cropped_four.tif")

#convert buffered region to raster
buffered_region_raster_four <- raster(buffered_region_four) #convert africa map to raster
backg_four_degree <- randomPoints(buffered_region_raster_four, n=20000)

plot(wrld_simpl)
points(backg_four_degree, col = "red", cex = 0.5)

mx_myna_crop_k_fold_qgis_four <- maxent(worldclim_and_pop, thin_myna_coords, a = backg_four_degree, args=c('betamultiplier=3','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_crop_k_fold_qgis_four@results


####

# five degree crop circle

####

#now read in the file w/ buffers
buffered_region_five <- readGDAL("data/buffer_layer_cropped_five.tif")

#convert buffered region to raster
buffered_region_raster_five <- raster(buffered_region_five) #convert africa map to raster
backg_five_degree <- randomPoints(buffered_region_raster_five, n=20000)

plot(wrld_simpl)
points(backg_five_degree, col = "red", cex = 0.2)

mx_myna_crop_k_fold_qgis_five <- maxent(worldclim_and_pop, thin_myna_coords, a = backg_five_degree, args=c('doclamp=TRUE','responsecurves=TRUE', 'replicatetype=crossvalidate', 'replicates=10','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_crop_k_fold_qgis_five

mx_myna_all_occs_five_degree <- maxent(worldclim_and_pop, thin_myna_coords, a = backg_five_degree, args=c('doclamp=TRUE','responsecurves=TRUE','writebackgroundpredictions=TRUE','outputgrids=TRUE'))
mx_myna_all_occs_five_degree@lambdas

#Make predictions
px_myna_all_occs_five_degree <- predict(worldclim_and_pop, mx_myna_all_occs_five_degree) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna_all_occs_five_degree, main= 'Maxent, raw values')
writeRaster(px_myna_all_occs_five_degree, filename="myna_five_degree_all_occs_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff

#Plotting Maxent output
map_myna_five_degree <- rasterToPoints(px_myna_all_occs_five_degree) #make predictions raster a set of points for ggplot
df_myna_five_degree <- data.frame(map_myna_five_degree) #convert to data.frame
head(df_myna_five_degree)
colnames(df_myna_five_degree) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
max(df_myna_five_degree$Suitability)
plot(wrld_simpl)
points(filter(df_myna_five_degree, Suitability >= .73), col="red")

####

#Jamie's Minimum Convex Polygon Function

#####

# mcp <- function (xy) {
#   # handler for spatial objects -- extracts coordinates
#   if (class(xy) == "SpatialPoints" | class(xy) == "SpatialPointsDataFrame") {
#     xy <- as.data.frame(coordinates(xy))
#   }
#   # get mcp indices
#   i <- chull(xy)
#   # get rows in xy for i
#   xy.mcp <- xy[i,]
#   # copy first row to last position to close shape
#   xy.mcp <- rbind(xy.mcp[nrow(xy.mcp),], xy.mcp)
#   # return polygon of mcp
#   return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.mcp))), 1))))
# }

####

# Function for calculating # of parameters in each model

####
# read lambdas file
rf <- read.table(file.path(curpath, 'species.lambdas'), sep=',', fill=TRUE)
# record no. of params (restrict df to rows with four values and no 0 in 2nd column)
p <- nrow(rf[!is.na(rf[3]) & rf[2] != 0,])