#Hakken en Zagen
#11 Januari 2016
#Mark ten Vregelaar, Jos Goris

#import libraries
library(sp)
library(rgeos)
library(rgdal)

#read the shapefile railways
dsn_rail = file.path("data","railways.shp")
ogrListLayers(dsn_rail)
railways <- readOGR(dsn_rail, layer = ogrListLayers(dsn_rail))

#read the shapefile places
dsn_place = file.path("data","places.shp")
ogrListLayers(dsn_place)
places <- readOGR(dsn_place, layer = ogrListLayers(dsn_place))

#define CRS object for RD projection
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")

railwayRD <- spTransform(railways, prj_string_RD)
placesRD <- spTransform(places, prj_string_RD)

#select the industrial railways
indrail <- subset(railwayRD, select = type, type == 'industrial')

#buffer the industrial railways
buffrail <- gBuffer(indrail, byid = TRUE, width = 1000)

#Intersect the industrial rail and the places
intersectPlaceRail <- gIntersection(buffrail, placesRD, byid = TRUE )

#Lookup the city
placeID <- strsplit(rownames(intersectPlaceRail@coords), split = " ")
city <- placeID[[1]][2]
cityInBuffer <- placesRD[as.numeric(city),]

#City: Utrecht 
#Population: 100000
print(paste(as.character(cityInBuffer$name), "has a population of", as.character(cityInBuffer$population)))

#Plot the buffer and the city
plot(buffrail, col = "lightgreen", lwd = 2, axes = T, main = "Places in the industrial rail buffer")
plot(indrail, col = "red", lwd = 5, add = T)
plot(cityInBuffer, col = "red", pch = 15, cex = 5, add = T)
text(cityInBuffer@coords[,1], cityInBuffer@coords[,2], labels = cityInBuffer$name)
