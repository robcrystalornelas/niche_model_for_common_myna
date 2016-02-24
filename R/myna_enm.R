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
myna_points <- select(myna_no_absences, species, lon, lat, country) #select only necessary columns
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

filter(myna_unique, lat>(80)) #find northern midwest point
points(22.01667,88.01667) #confirm this is northernmost point
which(myna_unique$lon == 22.01667, myna_unique$lat== 88.01667) #find it in the data.frame
myna_unique<- myna_unique[-30626,] #remove that row!


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
#can elect to read in .csv of all thinned points
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

str(worldclim[[19]])
#Pop density from SEDAC @ Columbia
population<-raster(paste(getwd(), "/Pop_density_2000/pop2.tif", sep = ""))
worldclim_and_pop <- stack(worldclim, population)

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

#Making the BioClim Model
??bioclim

myna_bioclim<-bioclim(worldclim,thin_myna_coords)
#this runs and saves the BIOCLIM model as an object
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
  scale_fill_gradientn(colours=c("#efedf5","dodgerblue1","cyan1","darkolivegreen2","yellow1","darkorange1", "red"), na.value = "black",limits=c(0,.66))
  
map_bioclim_predictions #full world map

map_bioclim_predictions_nc_america<-map_bioclim_predictions + coord_fixed(xlim = c(-125.8,-62.2), ylim = c(3, 50)) #north/central america
map_bioclim_predictions_nc_america
map_bioclim_predictions_florida <- map_bioclim_predictions + coord_fixed(xlim = c(-88,-79), ylim = c(24, 32)) #florida
map_bioclim_predictions_florida

