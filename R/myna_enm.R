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
data(wrld_simpl)

setwd("/Users/rpecchia/Desktop/Zoogeo Spring 2016/zoogeo-project/R")

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

####

# Download environmental data

####

#worldclim <- getData('worldclim', var='bio', res=2.5)
plot(worldclim[[1]], main = "Annual Mean Temp \n and Common Myna Occurrences")
points(thin_myna2$lon, thin_myna2$lat ,col = "blue", cex = .25)
points(points(myna_unique$lon, myna_unique$lat ,col = "blue", cex = .25))

plot(worldclim[[19]], main = "Precip in Coldest Quarter \n and Common Myna Occurrences")
points(points(myna_unique$lon, myna_unique$lat ,col = "blue", cex = .25))
?points

#Pop density from SEDAC @ Columbia
population<-raster(paste(getwd(), "/Pop_density_2000/pop2.tif", sep = ""))
worldclim_and_pop <- stack(worldclim, population)

#AVHRR landuse categorical grid
landcover<-raster(paste(getwd(), "/AVHRR.tif", sep = ""))
worldclim_pop_landcover <- stack(worldclim, population, landcover)
plot(landcover)

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
points(train, col = "red", cex = .5)

#plot testing dataset
plot(wrld_simpl)
points(test, col = "blue", cex = .5)

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

#cropping the enviro variables
min(thin_myna_coords$lon)
min(thin_myna_coords$lat)
max(thin_myna_coords$lon)
max(thin_myna_coords$lat)

# Jamie's mcp function
mcp_myna <- mcp(thin_myna_coords)
plot(wrld_simpl)
plot(mcp_myna, add = T)

bb_backg <- randomPoints(worldclim_pop_landcover, n=10000, ext = (extent(-179.988, 179.97, -43,51.6714))) #pull background points from specified extent

#getting background of abiotic layers from mcp
env_mask <- mask(worldclim_pop_landcover,mcp_myna) #mask takes all values that are not null, and returns
env_crop <- crop(env_mask, mcp_myna)
plot(env_crop[[1]])
backg<-randomPoints(env_crop[[1]],n=10000)

plot(wrld_simpl)
points(backg)

#now k-fold partition background points
colnames(backg) = c('lon' , 'lat')
group <- kfold(backg, 4)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

colnames(bb_backg) = c('lon' , 'lat')
group <- kfold(bb_backg, 4)
bb_backg_train <- bb_backg[group != 1, ]
bb_backg_test <- bb_backg[group == 1, ]

####

# ENMeval

####
#always keep a CSV of the same background!
#AUC full is ACU of all localities
#Mean. AUC over all iterations, then variation of AUC
#mean ormission rate at 10% threshold
#number of parameters used (is from the lambda file). #when selecting AIC, go with Delta AIC = 0
#look @results

enmeval_results <- ENMevaluate(thin_myna_coords, env=worldclim_pop_landcover, n.bg = 1000 ,method="block", overlap=TRUE, bin.output=TRUE, clamp=TRUE, parallel = TRUE)

save(enmeval_results, file="enmeval_results_worldclim.rdata")
# load("enmeval_results.rdata")

#look at ENMeval for WorldClim
plot(enmeval_results@predictions[[which (enmeval_results_worldclim@results$delta.AICc == 0) ]])
points(enmeval_results@occ.pts, pch=21, bg=enmeval_results@occ.grp)
head(enmeval_results@results)
enmeval_results@results #all the results
Q_enmeval<-enmeval_results@results#arrange by AICc value
QQ_enmeval<-as.data.frame(Q_enmeval)
head(QQ_enmeval)
QQ_enmeval<-QQ_enmeval[,c(1,2,3,14)]
head(QQ_enmeval)
arrange(QQ_enmeval,AICc,settings,features,rm) #this will sort ENMeval results so that we can see exact settings for model with lowest AICc
# #Shows that model with LQ ranging from .5-4.0 all had the lowest AICc
enmeval_resultsm@overlap

#Very important figures
par(mfrow=c(2,2))
eval.plot(enmeval_results@results, legend.position="topright")
eval.plot(enmeval_results@results, "Mean.AUC", )
eval.plot(enmeval_results@results, "Mean.AUC.DIFF", variance="Var.AUC.DIFF")
eval.plot(enmeval_results_worldclim@results, "Mean.ORmin")
 
enmeval_results_worldclim@results
#specify how data should be partitioned w/ method="jackknife", "randomkfold", "user", "block", "checkerboard1", "checkerboard2".
# n.bg is The number of random background localities to draw from the study extent
#when overlap = TRUE, provides pairwise metric of niche overlap 
#bin.output appends evaluations metrics for each evaluation bin to results table

enmeval_results <- enmeval_results@results
#subset to get rid of rows with AUC, take the one with lowest delta.AICc, then plug in optimal parameters

## ENMEVAL TIPS####
#always keep a CSV of the same background!
#AUC full is ACU of all localities
#Mean. AUC over all iterations, then variation of AUC
#mean ormission rate at 10% threshold
#number of parameters used (is from the lambda file). #when selecting AIC, go with Delta AIC = 0
#look @results

results <- res@results
#subset to get ride of rows with AUC, take the one with lowest delta.AICc, then plug in optimal parameters

####

# MaxEnt with default settings full bounding box

####

mx_myna_default_bb <- maxent(worldclim_pop_landcover, train, a = bb_backg, args=c('responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_myna_default_bb)
plot(mx_myna_default_bb)
mx_myna_default_bb@lambdas

#Model Evaluation 
e_myna_default_bb <- evaluate(test, bb_backg_test, mx_myna_default_bb, worldclim_pop_landcover) #evalute test points, pseudo-absences (random background points), the model and predictors
e_myna_default_bb #shows number of presences/absences/AUC and cor
px_myna_default_bb <- predict(worldclim_pop_landcover, mx_myna_default_bb, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna_default_bb, main= 'Maxent, raw values')
writeRaster(px_myna_default_bb, filename="myna_default_bounding_box.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff


tr_myna_default_bb <- threshold(e_myna_default_bb, 'spec_sens' )
tr_myna_default_bb
plot(px_myna_default_bb > tr_myna_default_bb, main='presence/absence')

plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_myna_default_bb, 'ROC')


####

# MaxEnt with default settings, MCH background

####

mx_myna_default <- maxent(worldclim_pop_landcover, train, a = backg, args=c('responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_myna_default)
plot(mx_myna_default)
mx_myna_default@lambdas

#Model Evaluation 
e_myna_default <- evaluate(test, backg_test, mx_myna_default, worldclim_pop_landcover) #evalute test points, pseudo-absences (random background points), the model and predictors
e_myna_default #shows number of presences/absences/AUC and cor
px_myna_default <- predict(worldclim_pop_landcover, mx_myna_default, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna_default, main= 'Maxent, raw values')
writeRaster(px_myna_default, filename="myna_default_for_qgis.tif", format="GTiff", overwrite=TRUE) #exporting a GEOtiff


tr_myna_default <- threshold(e_myna_default, 'spec_sens' )
tr_myna_default
plot(px_myna_default > tr_myna_default, main='presence/absence')

plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_myna_default, 'ROC')

####

#running MaxEnt with ENMeval settings

####

mx_myna_enmeval <- maxent(worldclim_pop_landcover, train, a = back, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_myna)
plot(mx_myna)
mx_myna@lambdas

#Model Evaluation 
e_myna <- evaluate(test, backg_test, mx_myna, worldclim_pop_landcover) #evalute test points, pseudo-absences (random background points), the model and predictors
e_myna #shows number of presences/absences/AUC and cor
px_myna <- predict(worldclim_pop_landcover, mx_myna, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_myna <- threshold(e_myna, 'spec_sens' )
tr_myna
plot(px_myna > tr_myna, main='presence/absence')

plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_myna, 'ROC')

####

# Minimum Convex Polygon Function

#####

mcp <- function (xy) {
  # handler for spatial objects -- extracts coordinates
  if (class(xy) == "SpatialPoints" | class(xy) == "SpatialPointsDataFrame") {
    xy <- as.data.frame(coordinates(xy))
  }
  # get mcp indices
  i <- chull(xy)
  # get rows in xy for i
  xy.mcp <- xy[i,]
  # copy first row to last position to close shape
  xy.mcp <- rbind(xy.mcp[nrow(xy.mcp),], xy.mcp)
  # return polygon of mcp
  return(SpatialPolygons(list(Polygons(list(Polygon(as.matrix(xy.mcp))), 1))))
}

#######

#Test spatial block function from ENMeval

######

block <- get.block(train, backg_mcp)

get.block <- function(occ, bg.coords){
  # SPLIT OCC POINTS INTO FOUR SPATIAL GROUPS
  noccs <- nrow(occ)
  n1 <- ceiling(nrow(occ)/2)
  n2 <- floor(nrow(occ)/2)
  n3 <- ceiling(n1/2)
  n4 <- ceiling(n2/2)
  grpA <- occ[order(occ[, 2]),][1:n1,]
  grpB <- occ[rev(order(occ[, 2])),][1:n2,]
  grp1 <- grpA[order(grpA[, 1]),][1:(n3),]
  grp2 <- grpA[!rownames(grpA)%in%rownames(grp1),]
  grp3 <- grpB[order(grpB[, 1]),][1:(n4),]
  grp4 <- grpB[!rownames(grpB)%in%rownames(grp3),]
  
  # SPLIT BACKGROUND POINTS BASED ON SPATIAL GROUPS
  bvert <- mean(max(grp1[, 1]), min(grp2[, 1]))
  tvert <- mean(max(grp3[, 1]), min(grp4[, 1]))
  horz <- mean(max(grpA[, 2]), min(grpB[, 2]))
  bggrp1 <- bg.coords[bg.coords[, 2] <= horz & bg.coords[, 1]<bvert,]
  bggrp2 <- bg.coords[bg.coords[, 2] < horz & bg.coords[, 1]>=bvert,]
  bggrp3 <- bg.coords[bg.coords[, 2] > horz & bg.coords[, 1]<=tvert,]
  bggrp4 <- bg.coords[bg.coords[, 2] >= horz & bg.coords[, 1]>tvert,]
  
  r <- data.frame()
  if (nrow(grp1) > 0) grp1$grp <- 1; r <- rbind(r, grp1)
  if (nrow(grp2) > 0) grp2$grp <- 2; r <- rbind(r, grp2)
  if (nrow(grp3) > 0) grp3$grp <- 3; r <- rbind(r, grp3)
  if (nrow(grp4) > 0) grp4$grp <- 4; r <- rbind(r, grp4)
  occ.grp <- r[order(as.numeric(rownames(r))),]$grp
  
  bgr <- data.frame()
  if (nrow(bggrp1) > 0) bggrp1$grp <- 1; bgr <- rbind(bgr, bggrp1)
  if (nrow(bggrp2) > 0) bggrp2$grp <- 2; bgr <- rbind(bgr, bggrp2)
  if (nrow(bggrp3) > 0) bggrp3$grp <- 3; bgr <- rbind(bgr, bggrp3)
  if (nrow(bggrp4) > 0) bggrp4$grp <- 4; bgr <- rbind(bgr, bggrp4)
  bg.grp <- bgr[order(as.numeric(rownames(bgr))),]$grp
  
  out <- list(occ.grp=occ.grp, bg.grp=bg.grp)
  return(out)
}
