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
crs(cty)


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

#now have GEOID, county name, and state together


###don't need this
cty_select <- counties2[counties2$State == abbrev, ] #get list of counties in chosen state
head(cty_select)
dim(cty_select)
length(unique(cty_select$CountyName))
#now have county info by state
###



#add state names to cty
#county_shape@data <- merge(county_info, county_shape@data, by="GEOID", all.x=TRUE)
cty@data <- merge(counties2, cty@data, by=c("GEOID", "STATEFP"), all.x=TRUE)

head(cty@data)

#subset to NC
#NC <- cty[cty@data$State == "CO"|cty@data$State == "KS"|cty@data$State == "NE"|cty@data$State == "WY"|cty@data$State == "SD"|cty@data$State == "ND"|cty@data$State == "MT",]
#460 observations

NC2 <- cty@data %>%
  filter(State == "CO"|State == "KS"|State == "NE"|State == "WY"|State == "SD"|State == "ND"|State == "MT")
#460 observations

NC2$lat <- sub('.', '', NC2$INTPTLAT)
NC2$long <- sub('.', '', NC2$INTPTLON)
NC2$long <- str_remove(NC2$long, "^0+")

head(NC2)

NC2keep <- NC2 %>% select(1, 2, 4, 5, 7, 20:23)
write.table(NC2, file = "NC2.csv", sep = ",", row.names = FALSE)
write.table(NC2keep, file = "NC2keep.csv", sep = ",", row.names = FALSE)





########################################



#bring in grass info for each species
BRTE <- read.csv(file = 'data/BRTE_countytable.csv')
IMCY <- read.csv(file = 'data/IMCY_countytable.csv')
MISI <- read.csv(file = 'data/MISI_countytable.csv')
MIVI <- read.csv(file = 'data/MIVI_countytable.csv')
NERE <- read.csv(file = 'data/NERE_countytable.csv')
PECI <- read.csv(file = 'data/PECI_countytable.csv')
SCBA <- read.csv(file = 'data/SCBA_countytable.csv')
TACA <- read.csv(file = 'data/TACA8_countytable.csv')

#didn't get the for loop to run
species <- list('BRTE', 'IMCY', 'MISI', 'MIVI', 'NERE', 'PECI', 'SCBA', 'TACA')
species

is.factor(species)

#manually swap out i to loop through the species dataframes
i <- TACA

for (i in species) {
#create new column for future agreement in 11 or more models
  BRTE <- BRTE %>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0))

#create a zero 1 column for current observations
  BRTE <- BRTE %>% 
  mutate (expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0))

  BRTE <- BRTE %>% 
  mutate (retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0))

  BRTE <- BRTE %>% 
  mutate (persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))
}

#BRTE$Future_Models_Sum_BRTE <- BRTE$Future_Models_Sum
#IMCY$Future_Models_Sum_IMCY <- IMCY$Future_Models_Sum
#MISI$Future_Models_Sum_MISI <- MISI$Future_Models_Sum
#MIVI$Future_Models_Sum_MIVI <- MIVI$Future_Models_Sum
#NERE$Future_Models_Sum_NERE <- NERE$Future_Models_Sum
#PECI$Future_Models_Sum_PECI <- PECI$Future_Models_Sum
#SCBA$Future_Models_Sum_SCBA <- SCBA$Future_Models_Sum
#TACA$Future_Models_Sum_TACA <- TACA$Future_Models_Sum

BRTEkeep <- BRTE %>% select(1, 5, 20:23)
IMCYkeep <- IMCY %>% select(1, 5, 20:23)
MISIkeep <- MISI %>% select(1, 5, 20:23)
MIVIkeep <- MIVI %>% select(1, 5, 20:23)
NEREkeep <- NERE %>% select(1, 5, 20:23)
PECIkeep <- PECI %>% select(1, 5, 20:23)
SCBAkeep <- SCBA %>% select(1, 5, 20:23)
TACAkeep <- TACA %>% select(1, 5, 20:23)

BRTEkeep <- BRTEkeep %>% rename(current_BRTE = sppRichCtyBias_current, future85_BRTE = future85, 
                        expander_BRTE = expander, retractor_BRTE = retractor, persistant_BRTE = persistant)

IMCYkeep <- IMCYkeep %>% rename(current_IMCY = sppRichCtyBias_current, future85_IMCY = future85, 
                                expander_IMCY = expander, retractor_IMCY = retractor, persistant_IMCY = persistant)

MISIkeep <- MISIkeep %>% rename(current_MISI = sppRichCtyBias_current, future85_MISI = future85, 
                                expander_MISI = expander, retractor_MISI = retractor, persistant_MISI = persistant)

MIVIkeep <- MIVIkeep %>% rename(current_MIVI = sppRichCtyBias_current, future85_MIVI = future85, 
                                expander_MIVI = expander, retractor_MIVI = retractor, persistant_MIVI = persistant)

NEREkeep <- NEREkeep %>% rename(current_NERE = sppRichCtyBias_current, future85_NERE = future85, 
                                expander_NERE = expander, retractor_NERE = retractor, persistant_NERE = persistant)

PECIkeep <- PECIkeep %>% rename(current_PECI = sppRichCtyBias_current, future85_PECI = future85, 
                                expander_PECI = expander, retractor_PECI = retractor, persistant_PECI = persistant)

SCBAkeep <- SCBAkeep %>% rename(current_SCBA = sppRichCtyBias_current, future85_SCBA = future85, 
                                expander_SCBA = expander, retractor_SCBA = retractor, persistant_SCBA = persistant)

TACAkeep <- TACAkeep %>% rename(current_TACA = sppRichCtyBias_current, future85_TACA = future85, 
                                expander_TACA = expander, retractor_TACA = retractor, persistant_TACA = persistant)




grassesa <- left_join(BRTEkeep, IMCYkeep, by = "GEOID")
grassesb <- left_join(grassesa, MISIkeep, by = "GEOID")
grassesc <- left_join(grassesb, MIVIkeep, by = "GEOID")
grassesd <- left_join(grassesc, NEREkeep, by = "GEOID")
grassese <- left_join(grassesd, PECIkeep, by = "GEOID")
grassesf <- left_join(grassese, SCBAkeep, by = "GEOID")
grasses <- left_join(grassesf, TACAkeep, by = "GEOID")

#sum all grasses
grasses <- grasses %>%
  mutate(totfuture = future85_BRTE+future85_IMCY+future85_MISI+future85_MIVI+future85_NERE+future85_PECI+future85_SCBA+future85_TACA)


grasses <- grasses %>%
  mutate(totcurrent = current_BRTE+current_IMCY+current_MISI+current_MIVI+current_NERE+current_PECI+current_SCBA+current_TACA)

grasses <- grasses %>%
  mutate(totexpander = expander_BRTE+expander_IMCY+expander_MISI+expander_MIVI+expander_NERE+expander_PECI+expander_SCBA+expander_TACA)

write.table(grasses, file = "grasses.csv", sep = ",", row.names = FALSE)


######################################
#join grasses to counties

#something happens here in this join step and all grass data becomes NA
grasses$GEOID <-as.factor(grasses$GEOID)
is.factor(NC2keep$GEOID)
is.factor(grasses$GEOID)

GEOIDs <- NC2keep$GEOID

grassesNC <- grasses %>%
  filter(GEOID %in% GEOIDs)
#396 observations; maybe only had grass data for 396 of the 460 counties? or only had 396 matching GEOIDs???
         
countygrasses <- left_join(grassesNC, NC2keep, by = "GEOID")
#396 observations

write.table(countygrasses, file = "countygrasses.csv", sep = ",", row.names = FALSE)


countygrassesslim <- countygrasses %>% select(1, 42:44, 46, 48)

colnames(countygrassesslim)[colnames(countygrassesslim) == 'NAME'] <- 'CountyName'
write.table(countygrassesslim, file = "countygrassesslim.csv", sep = ",", row.names = FALSE)









head(cty@data)


NCsub <- cty@data %>%
  dplyr::filter(GEOID %in% GEOIDs)

#well, this is obviously not correct
plot(NCsub)

polys <- df_to_SpatialPolygons(countygrasses,"GEOID",c("long","lat"), CRS("+proj=longlat +datum=WGS84 +no_defs "))
#Error in attributes(out) <- attributes(col) : 
#'names' attribute [396] must be the same length as the vector [1]


####
#plotting
ggplot(data = countygrasses) +
  geom_polygon(aes(x = long, y = lat, color = totfuture))




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
