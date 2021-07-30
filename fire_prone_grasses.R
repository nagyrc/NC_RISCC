#Fire-prone invasive grasses for the North Central US
#R. Chelsea Nagy, Emily Fusco, Jenica Allen

library(rgdal)
library(sf)
library(tidyverse)
library(ggplot2)
library(maptools)
library(FRK)

abbrev <- "CO" # set state of interest 

### SUBSET MAP TO SELECTED STATE

getwd()

## read in states shapefile (WGS 84)
srg <- readOGR("data/US_states/US_states.shp", layer="US_states")
proj4string(srg) #"+proj=longlat +datum=WGS84 +no_defs "
head(srg@data)

## Create a state list for reference
states <- as.data.frame(cbind(as.character(srg@data$STUSPS), as.character(srg@data$STATEFP), as.character(srg@data$NAME)), stringsAsFactors = F)
states

head(states)
str(states)
colnames(states) <- c("Abbrev", "STATEFP", "State")

## subset spatial data to state of interest
chosen_state <- srg[grep(abbrev, srg$STUSPS),]
chosen_statefp <- as.character(chosen_state@data$STATEFP)

## check map for accurate subsetting
plot(srg)
plot(chosen_state, col = "red", add = T) #all code above works


#### GET LIST OF GEOID IN CHOSEN STATE

### read in county shapefile
cty <- readOGR("data/US_counties//WGS84_clip.shp", layer="WGS84_clip")
proj4string(cty) #"+proj=longlat +datum=WGS84 +no_defs "
head(cty@data)
#plot(cty) this works but takes forever

counties <- as.data.frame(cbind(as.character(cty@data$GEOID), as.character(cty@data$NAMELSAD), as.character(cty@data$STATEFP)), stringsAsFactors = F)
colnames(counties) <- c("GEOID", "CountyName", "STATEFP")
head(counties)
str(counties)
dim(counties)


## county table does not include state name, add state names for ease of use
## create data object to catch output in loop
counties1 <- cbind("99999", "Example County", "XX", "XX")
colnames(counties1) <- c("GEOID", "CountyName", "STATEFP", "State")
counties1


for (i in unique(counties$STATEFP)){
  
  sub = counties[counties$STATEFP == i, ] #subsets counties of single state
  sub1 = unique(states$Abbrev[states$STATEFP == i]) # returns single state abbreviation
  
  sub$State <- sub1 #adds state abbreviation to all counties in subset
  
  counties1 <- rbind(counties1, sub) #appends new data to county level dataset
  
  print(paste(sub1, "done", " "))
  
}

head(counties1)
dim(counties1)

counties2 <- counties1[2:3109, ] #remove junk row from object creation
head(counties2)
dim(counties2)

cty_select <- counties2[counties2$State == abbrev, ] #get list of counties in chosen state
head(cty_select)
dim(cty_select)
length(unique(cty_select$CountyName))
#now have county info by state

#bring in grass info for each species
BRTE <- read.csv(file = 'data/BRTE_countytable.csv')
IMCY <- read.csv(file = 'data/IMCY_countytable.csv')
MISI <- read.csv(file = 'data/MISI_countytable.csv')
MIVI <- read.csv(file = 'data/MIVI_countytable.csv')
NERE <- read.csv(file = 'data/NERE_countytable.csv')
PECI <- read.csv(file = 'data/PECI_countytable.csv')
SCBA <- read.csv(file = 'data/SCBA_countytable.csv')
TACA <- read.csv(file = 'data/TACA8_countytable.csv')


#add state names to cty
#county_shape@data <- merge(county_info, county_shape@data, by="GEOID", all.x=TRUE)
cty@data <- merge(counties2, cty@data, by=c("GEOID", "STATEFP"), all.x=TRUE)

#subset to NC
NC <- cty[cty@data$State == "CO"|cty@data$State == "KS"|cty@data$State == "NE"|cty@data$State == "WY"|cty@data$State == "SD"|cty@data$State == "ND"|cty@data$State == "MT",]
#460 observations

NC2 <- NC@data %>%
  filter(State == "CO"|State == "KS"|State == "NE"|State == "WY"|State == "SD"|State == "ND"|State == "MT")
#396 observations; not sure why the discrepancy

NC2$lat <- sub('.', '', NC2$INTPTLAT)
NC2$long <- sub('.', '', NC2$INTPTLON)
NC2$long <- str_remove(NC2$long, "^0+")

ggplot(data = NC2) +
  geom_polygon(aes(x = long, y = lat, color = Future_Models_Sum))


#merge grass data
head(NC@data)
head(BRTE)
BRTE$GEOID <- as.factor(BRTE$GEOID)
is.factor(BRTE$GEOID)

#add cheatgrass
NC@data <- merge(BRTE, NC@data, by="GEOID")



ggplot(NC) +
  geom_sf()


#plot(NC)

NCsf <-st_as_sf(NC)
NCdf <- SpatialPolygonsDataFrame_to_df(NC, vars = names(NC))


###
if (require("maptools")) {
  sids <- system.file("shapes/sids.shp", package="maptools")
  nc1 <- readShapePoly(sids,
                       proj4string = CRS("+proj=longlat +datum=NAD27"))
  nc1_df <- fortify(nc1)
}



###
fortify.SpatialPolygonsDataFrame <- function(model, data, region = NULL, ...) {
  attr <- as.data.frame(model)
  # If not specified, split into regions based on polygons
  if (is.null(region)) {
    coords <- ldply(model@polygons,fortify)
    message("Regions defined for each Polygons")
  } else {
    cp <- sp::polygons(model)
    try_require("maptools")
    # Union together all polygons that make up a region
    unioned <- unionSpatialPolygons(cp, attr[, region])
    coords <- fortify(unioned)
    coords$order <- 1:nrow(coords)
  }
  coords
}


fortify.SpatialPolygons <- function(model, data, ...) {
  ldply(model@polygons, fortify)
}


fortify.Polygons <- function(model, data, ...) {
  subpolys <- model@Polygons
  pieces <- ldply(seq_along(subpolys), function(i) {
    df <- fortify(subpolys[[model@plotOrder[i]]])
    df$piece <- i
    df
  })
  within(pieces,{
    order <- 1:nrow(pieces)
    id <- model@ID
    piece <- factor(piece)
    group <- interaction(id, piece)
  })
}


fortify.Polygon <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df$hole <- model@hole
  df
}


fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  ldply(model@lines, fortify)
}


fortify.Lines <- function(model, data, ...) {
  lines <- model@Lines
  pieces <- ldply(seq_along(lines), function(i) {
    df <- fortify(lines[[i]])
    df$piece <- i
    df
  })
  within(pieces,{
    order <- 1:nrow(pieces)
    id <- model@ID
    piece <- factor(piece)
    group <- interaction(id, piece)
  })
}


fortify.Line <- function(model, data, ...) {
  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- 1:nrow(df)
  df
}

###




# in order to plot polygons, first fortify the data
NC@data$id <- rownames(NC@data)
# create a data.frame from our spatial object
NCdata <- fortify(NC, region = "id")
# merge the "fortified" data with the data from our spatial object
NCdf <- merge(NCdata, NC@data, by = "id")


dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected, region = "id")
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")





NC@data$id <- rownames(NC@data)
watershedPoints <- fortify(NC, region = "id")
NCDF <- merge(watershedPoints, NC@data, by = "id")

ggNC <- ggplot(data = NCDF, aes(x=long, y=lat, fill = Future_Models_Sum)) +
  geom_polygon()
ggNC

head(srg@data)
NCstate <- srg@data %>%
  filter(STUSPS == "CO"|STUSPS == "KS"|STUSPS == "NE"|STUSPS == "WY"|STUSPS == "SD"|STUSPS == "ND"|STUSPS == "MT")


plot(NCstate)
plot(NC)
head(NC)
str(NC)

ggplot()+ 
  geom_polygon(data=NC, aes(INTPTLON, INTPTLAT, color = Future_Models_Sum))

#par(mar=c(1,1,1,1))
dev.off()

#join all together
grassesa <- rbind(BRTE, IMCY)
grassesb <- rbind(grassesa, MISI)
grassesc <- rbind(grassesb, MIVI)
grassesd <- rbind(grassesc, NERE)
grassese <- rbind(grassesd, PECI)
grassesf <- rbind(grassese, SCBA)
grasses <- rbind(grassesf, TACA)