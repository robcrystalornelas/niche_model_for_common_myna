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
# print temporary dir
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
# print temporary dir
print(tempdir())
write.SpThin(
  myna_thin,
  coords=FALSE,
  dir=tempdir()
)

#can elect to read in .csv of all thinned points
thin_myna2<-read.csv("thin_0001.csv", head=T)
head(thin_myna2)
thin_myna_coords<-thin_myna2[,2:3]
dim(thin_myna_coords)

#Prepare Training and Testing dataset####
folds<-kfold(thin_myna_coords, k=4) #this is a 4 fold test
train<-thin_myna_coords[folds>1,] #training has 75% of points
test<-thin_myna_coords[folds==1,] #testing has 25% of points
train<-train[,1:2]
test<-test[,1:2]
head(train) #just has lon/lat

####

#Making the BioClim Model

####
myna_bioclim<-bioclim(worldclim,thin_myna_coords) #this runs and saves the BIOCLIM model as an object
myna_bioclim_predictions <- predict(myna_bioclim, worldclim, progress='text')
plot(myna_bioclim_predictions)

myna_bioclim_predictions_as_points <- rasterToPoints(myna_bioclim_predictions) #make predictions raster a set of points for ggplot
df_myna_bioclim_predictions <- data.frame(myna_bioclim_predictions_as_points) #convert to data.frame
head(df_myna_bioclim_predictions)
colnames(df_myna_bioclim_predictions) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
max(df_myna_bioclim_predictions$Suitability)

map_bioclim_predictions <-ggplot(data=df_myna_bioclim_predictions, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  theme_bw() +
  coord_equal() +
  ggtitle("Common Myna BIOCLIM Model") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')) +
  #scale_fill_gradientn(colours=c("#ffffcc","#d9f0a3","#addd8e","#78c679","#41ab5d","#238443", "#005a32"), #green
  #scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"), na.value = "black",limits=c(0,.66))
  #scale_fill_gradientn(colours=c("#efedf5","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"), na.value = "black",limits=c(0,.66))
  scale_fill_gradientn(colours=c("dodgerblue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"), na.value = "black",limits=c(0,.66))

map_bioclim_predictions #full world map

map_bioclim_predictions_nc_america<-map_bioclim_predictions + coord_fixed(xlim = c(-125.8,-62.2), ylim = c(3, 50)) #north/central america
map_bioclim_predictions_nc_america

map_bioclim_predictions_nc_america_points <- map_bioclim_predictions_nc_america + geom_point(data=thin_myna_coords, aes(x=lon, y=lat),size=.25, color = "magenta2")
#map_bioclim_predictions_nc_america_points

map_bioclim_predictions_florida <- map_bioclim_predictions + coord_fixed(xlim = c(-88,-79), ylim = c(24, 32)) #florida
map_bioclim_predictions_florida

map_bioclim_predictions_florida_points<- map_bioclim_predictions_florida + geom_point(data=thin_myna_coords, aes(x=lon, y=lat),size=.25, color = "magenta2")
map_bioclim_predictions_florida_points

#####

# MaxEnt Model

####

#cropping the enviro variables
myna_bounding_box<-Plot_ConvexHull(xcoord = thin_myna_coords$lon, ycoord = thin_myna_coords$lat, lcolor = "black")

min(thin_myna_coords$lon)
min(thin_myna_coords$lat)
max(thin_myna_coords$lon)
max(thin_myna_coords$lat)

backg <- randomPoints(worldclim_pop_landcover, n=10000, ext = (extent(-179.988, 179.97, -43,51.6714))) #pull background points from specified extent
plot(wrld_simpl)
points(backg)
points(thin_myna_coords, col= "red")
#now partition background points
colnames(backg) = c('lon' , 'lat')
group <- kfold(backg, 4)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

#running MaxEnt
mx_myna <- maxent(worldclim_pop_landcover, train, a=backg, args=c('betamultiplier=3','responsecurves=TRUE','writebackgroundpredictions=TRUE'))
response(mx_myna)
plot(mx_myna)

#Model Evaluation 
e_myna <- evaluate(test, backg_test, mx_myna, worldclim_pop_landcover) #evalute test points, pseudo-absences (random background points), the model and predictors
e_myna #shows number of presences/absences/AUC and cor
px_myna <- predict(worldclim_pop_landcover, mx_myna, progress= "" ) #make predictions of habitat suitability can include argument ext=ext
plot(px_myna, main= 'Maxent, raw values')
plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch=16, cex=.15, col="cadetblue3") #map of training points
points(test, pch=16, cex=.15, col="purple") #map of testing points
tr_myn2 <- threshold(e_myna, 'spec_sens' )
tr_myna
plot(px_myna > tr_myna, main='presence/absence')

plot(wrld_simpl, add=TRUE, border= 'dark grey' )
points(train, pch= '+')
plot(e_myna, 'ROC')

#Plotting Maxent output
myna_map_raster_to_points <- rasterToPoints(px_myna) #make predictions raster a set of points for ggplot
df_myna <- data.frame(myna_map_raster_to_points) #convert to data.frame
head(df_myna)
colnames(df_myna) <- c('lon', 'lat', 'Suitability') #Make appropriate column headings
plot(wrld_simpl)
max(df_myna$Suitability)
plot(wrld_simpl)
points(filter(df_myna, Suitability >= ***), col="red")

ggmap_myna <-ggplot(data=df_myna, aes(y=lat, x=lon)) +
  geom_raster(aes(fill=Suitability)) +
  theme_bw() +
  coord_equal() +
  ggtitle("MaxEnt Predictions for Common Myna") +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(face="bold", size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'black')
  )

worldmap_myna <- ggmap_myna + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                                    na.value = "black",limits=c(0,.82))
worldmap_myna

us_prediction_map_no_host <- p_no_host_all_worldclim2 + scale_fill_gradientn(colours=c("blue4","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"),
                                                                             na.value = "black",limits=c(0,.90)) + 
  coord_cartesian(xlim = c(-125.8,-62.2), ylim = c(22.8, 50)) #zoom in on US
#us_prediction_map_no_host

####

# Convex hull function from Chit Chat R

###
Plot_ConvexHull<-function(xcoord, ycoord, lcolor){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  lines(xcoord[hpts], ycoord[hpts], col = lcolor)
}  
