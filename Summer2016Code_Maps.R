#Summer 2016 Code for maps
#to reset plot displays
dev.off()

#maps walkthrough
#load packages it depends on 
library("maps")
library("mapdata")
library("maptools")

#to create multi-paneled figures
par(mfrow = c(1,2)) #this creates a plot of 1 row, 2 columns
#resent to one row, one column (one graph), using
par(mfrow = c(1,1))

#create a hi-resolution map of one country by name
map("worldHires", "South Africa")
locator() #use locator to find x, y (used below)
text(24.02189, -27.82328, "South Africa")
#create a map of various countries in the world, colored in darkseagreen3 with a background color "wheat"
map(database="world", regions = c("South Africa", "Cuba"), fill=TRUE, col="darkseagreen3", bg = "wheat")
#put bounding boxes around your map
map.axes()
#add points to an existing map
#df is the name of your data frame
points(df$lat, df$lon)
#connect the points on your map
#this connects them in the order they are in the data frame, so make sure that order is something relevant (like time)
#df is the name of your data frame
lines(df$lat, df$lon)

#find lat/longs once you have a country by name
locator()
#click on plot where you want to know lat/longs of
#output will be x,y coordinates for that location

#pick which color you want by name
colors()

#to plot an area of the world, use lat/longs
map("worldHires", xlim=c(-100,-30), ylim=c(-30,10))
#to fill in a specific country within that area of the world
map("worldHires", regions="Peru", col="red", fill=TRUE, add=TRUE)

#map the world by bounding lat-longs
#this example maps Antarctica 
map("worldHires", xlim=c(-83.25, -82.5), ylim=c(21.4, 22), fill=TRUE, col="darkseagreen3", bg="gray80")
#add a rectangle around a specific area
map(rect(-83.2, 21.5, -83.15, 21.65, border="red", add = TRUE))


#alternative package to plotting maps, has a background with lat/lon lines and still shows some surrounding areas
#this one sometimes works and sometimes crashes my RStudio
library("ggplot2")
library("mapproj")
coast_map <- fortify(map("worldHires", fill=TRUE, plot=TRUE))
gg <- ggplot()
gg <- gg + geom_map(data=coast_map, map=coast_map,
                    aes(x=long, y=lat, map_id=region),
                    fill="white", color="black")
gg <- gg + geom_map(data=data.frame(region="Peru"), map=coast_map,
                    aes(map_id=region), fill="steelblue")
gg <- gg + xlim(-90,-60) + ylim(-20,5) #y = lat, x = lon
gg <- gg + coord_map()
gg <- gg + theme_bw()
gg

#heat map
#traveler can be an individual, a seed, a species, etc
#assign your lat and long to a new data frame
traveler <- df[c("lon", "lat")]
#turn your data frame into a Spatial object
coordinates(traveler) <- df[c("x", "y")]
#get the projection attributes for your points
proj4string(traveler) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#make a heat map of where the traveler is likely to be
heat.map <- kernelUD(traveler, h = "href", grid = 60, same4all = FALSE, hlim = c(0.1,1.5), kern = c("bivnorm", "epa"), extent = 1, boundary = NULL)
#view this map
image(heat.map)
#add the traveler's point data to the map
points(traveler)
#change the type of point displayed on the map
image(heat.map)
points(traveler, pch = 1)
#add a map outline to the territory, replacing "country" with your country
map(map(database="world", regions = "Ireland", add = TRUE))

############ #formal mapping additions 
#to make a title
title(main = 'Name, Country', font.main = 2, cex.main = 1.5,
      sub = "and...", font.sub = 3, 
      col.sub = "blue")

#to make a map legend
#make a square to hold your legend
plot(c(-1.5, 1.5), c(-1.5, 1.5), asp = 1, type = 'n')
rect(-1.5, -1.0, -0.5, -2.0, border = NA, col=rgb(0, 0.5, 0.5, 0.7))
#corners of the rectangle are deliniated first, then border, then color scheme (rgb scale from 0-1 for each color where 0 is invisible) 
#add text to your legend
text(-1.0,-1.25, "legend",cex=0.5) #cex is text size

#to make a scale extent on your map
library("GISTools")
map.scale(x,y,miles2ft(2),"Miles",4,0.5)
#where x and y are the coordinates of the scale bar
#length of the scale bar (2 miles) converted into a scale for the map (miles converted to feet)
#the label on the scale bar
#how many divisions are on each scale bar (here there are 4)
#how long each division is (here it is 0.5 mies)

#to create a north arrow
library("GISTools")
north.arrow(x,y,miles2ft(0.25),col= "lightblue")
#where x and y are the coordinates of the north arrow
#the width of the north arrow relative to the scale of the map
#the color of the north arrow

#to add labels to features on your map
# assign some coordinates
Lat <- data.frame(df)[,1] #Y or North/South
Lon <- data.frame(df)[,2] #X or East/West
# assign some label
Names <- data.frame(df)[,3] #if feature lables are in column 3
# set plot parameters, plot and label
par(mar = c(0,0,0,0)) #reset graph parameters
plot(df, col = NA)
pl <- pointLabel(Lon, Lat, Names, offset = 0, cex =.5)



################### #internet maps
#part 2 of maps walkthrough
#linked to Internet: RgoogleMaps
library("RgoogleMaps")
library("RJSONIO")
lat <- c(-20,5) #define our map's ylim
lon <- c(-90, -60) #define our map's xlim
center = c(mean(lat), mean(lon))  #tell what point to center on
zoom <- 5  #zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "terrain.png")
#lots of visual options, just like google maps: maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")
PlotOnStaticMap(terrmap)

#linked to the internet: Open Street Map
#Open Street Map (OSM) is a crowd-sourced map of the world
#osmar is an R package for downloading and interrogating OSM data that accesses the data directly from the internet via the R command line
library("osmar")
library("sp")
src <- osmsource_api()
#lat/lon is Lima, Peru
bb <- center_bbox(-77.042342, -12.046456, 1000, 1000) #center_lon, center_lat, width, height 
#could also use corner_bbox() and delineate each corner of the box
ctown <- get_osm(bb, source = src)
plot(ctown)
points(-77.042342, -12.046456, col = "red", lwd = 1)

#to find specific features in Open Street Maps
summary(ctown$nodes) #to see what keys are in the data frame that you can look at
ts_ids <- find(ctown, node(tags(v == "traffic_signals"))) #to find all the traffic signals
ts <- subset(ctown, node_ids = ts_ids) #to subset the traffic signals into an osmar object
#to plot point features
plot_nodes(ts, add = TRUE, col = "blue") #to add the traffic signals to the map

#to find a different sort of feature, such as a polygon use as_sp
bg_ids <- find(ctown, way(tags(k == "building")))
bg_ids <- find_down(ctown, way(bg_ids))
bg <- subset(ctown, ids = bg_ids)
bg_poly <- as_sp(bg, "polygons") #could also do this for "points" or "lines" for other data subsets
plot(ctown)
summary(bg_poly)
#plot your polygons and changes in them by a column descriptor
#but this zooms in and changes the scale of the map, check against another map to make sure you know hwere in the frame it has zoomed into
spplot(bg_poly, c("timestamp")) #shows how old each building is
spplot(bg_poly, c("version"), add = TRUE) #shows how many time each building has been modified

#multiple Open Street Maps layers can be stacked
plot(bg_poly, col = "gray")
hw_ids <- find(ctown, way(tags(k == "highway")))
hw_ids <- find_down(ctown, way(hw_ids))
hw <- subset(ctown, ids = hw_ids)
hw_line <- as_sp(ctown, "lines")
plot(hw_line, add = TRUE, col = "green")

#to convert omar into a sp object
ctown_sp <- as_sp(ctown,what = c("points", "lines", "polygons"), crs = osm_crs(), simplify = TRUE)


#linked to Internet: leaflet
library("leaflet")


#############################
#interesting, extra stuff

#to create a heat map based on values in a column that has spatial coordinates
#load some grid data
data(meuse.grid)
# define a SpatialPixelsDataFrame from the data
mat = SpatialPixelsDataFrame(points = meuse.grid[c("x","y")], data = meuse.grid)
# set the plot margins (zooms in for this plot)
par(mar = c(0,0,0,0))
# plot the points based in value in "dist" using the default shading 
image(mat, "dist")
# load the package
library(RColorBrewer)
# select and examine a colour palette with 7 classes 
greenpal <- brewer.pal(7,'Greens')
# and now use this to plot the data
image(mat, "dist", col=greenpal)
