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

myna_from_gbif <- gbif("Acridotheres", "tristis*", geo = TRUE)
# adding * to species will download all subspecies
# if geo = TRUE, ensures all records have latitude AND longitude

myna_absence_removed<-myna_from_gbif[!myna_from_gbif$datasetName == "EBCC Atlas of European Breeding Birds", ] #remove absence records
unique(myna_maybe_no_absence$country) #check to make sure absence records removed

myna_points <- select(myna_absence_removed, species, lon, lat)
write.csv(myna_points, file = "myna_from_gbif.csv")

plot(wrld_simpl, axes=TRUE, col="light yellow", main = "Occurrences for Common Myna \n(Acridotheres tristis) from GBIF")
points(myna_points$lon, myna_points$lat ,col = "blue", cex = 1)
