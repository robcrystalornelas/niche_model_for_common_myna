######

#Common Myna ENM Project
#Rob Pecchia
#Zoogeography Spring 2016

#####

library(dismo)
library(dplyr)
library(maptools)
data(wrld_simpl)

setwd("/Users/rpecchia/Desktop/Zoogeo Spring 2016/zoogeo-project/R")

#myna_from_gbif <- gbif("Acridotheres", "tristis*", geo = TRUE)
# '*' also downloads occurrences of subspecies
# if geo = TRUE, ensures all records have latitude AND longitude

myna_points <- select(myna_from_gbif, species, lon, lat)
head(myna_points)
write.csv(myna_points, file = "myna_from_gbif.csv")

plot(wrld_simpl, axes=TRUE, col="light yellow", main = "Occurrences for Common Myna \n(Acridotheres tristis) from GBIF")
points(myna_points$lon, myna_points$lat, = "blue", cex = .5)

?maxent
