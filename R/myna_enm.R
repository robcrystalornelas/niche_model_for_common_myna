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
# adding * to species will download all subspecies
# if geo = TRUE, ensures all records have latitude AND longitude
dim(myna_from_gbif)

myna_no_absences<-subset(myna_from_gbif, datasetName!="EBCC Atlas of European Breeding Birds")
unique(myna_no_absences$country) #check to make sure absence records removed
myna_points <- select(myna_no_absences, species, lon, lat) #select only necessary columns
myna_points <- myna_points[complete.cases(myna_points),] #remove any rows where data is NA
write.csv(myna_points, file = "myna_from_gbif.csv") #write file of species name and occurrence data

plot(wrld_simpl, axes=TRUE, col="light yellow", main = "Occurrences for Common Myna \n(Acridotheres tristis) from GBIF")
points(myna_points$lon, myna_points$lat ,col = "blue", cex = .75)
