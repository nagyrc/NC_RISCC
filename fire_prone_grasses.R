#Fire-prone invasive grasses for the North Central US
#R. Chelsea Nagy, Emily Fusco, Jenica Allen

library(rgdal)
library(sf)
library(tidyverse)
library(ggplot2)
library(maptools)
library(FRK)
#library(broom)
library(RColorBrewer)

#abbrev <- "CO" # set state of interest 

### SUBSET MAP TO SELECTED STATE

getwd()

## read in states shapefile (WGS 84)
srg <- readOGR("data/US_states/US_states.shp", layer="US_states")
proj4string(srg) #"+proj=longlat +datum=WGS84 +no_defs "
head(srg@data)

## Create a state list for reference
states <- as.data.frame(cbind(as.character(srg@data$STUSPS), as.character(srg@data$STATEFP), as.character(srg@data$NAME)), stringsAsFactors = F)
states

NC <- 

head(states)
str(states)
colnames(states) <- c("Abbrev", "STATEFP", "State")

## subset spatial data to state of interest
#chosen_state <- srg[grep(abbrev, srg$STUSPS),]
#chosen_statefp <- as.character(chosen_state@data$STATEFP)

## check map for accurate subsetting
#plot(srg)
#plot(chosen_state, col = "red", add = T) #all code above works




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


countieskeep <- left_join(counties, states)
head(countieskeep)

#filter counties to NC
NC3 <- countieskeep %>%
  filter(Abbrev == "CO"|Abbrev == "KS"|Abbrev == "NE"|Abbrev == "WY"|Abbrev == "SD"|Abbrev == "ND"|Abbrev == "MT")
#460 observations

########################################
#remove
## county table does not include state name, add state names for ease of use
## create data object to catch output in loop
#counties1 <- cbind("99999", "Example County", "XX", "XX")
#colnames(counties1) <- c("GEOID", "CountyName", "STATEFP", "State")
#counties1


#for (i in unique(counties$STATEFP)){
  
  #sub = counties[counties$STATEFP == i, ] #subsets counties of single state
  #sub1 = unique(states$Abbrev[states$STATEFP == i]) # returns single state abbreviation
  
  #sub$State <- sub1 #adds state abbreviation to all counties in subset
  
  #counties1 <- rbind(counties1, sub) #appends new data to county level dataset
  
  #print(paste(sub1, "done", " "))
  
#}

#head(counties1)
#dim(counties1)

#counties2 <- counties1[2:3109, ] #remove junk row from object creation
#head(counties2)
#dim(counties2)

#now have GEOID, county name, and state together


###don't need this
#cty_select <- counties2[counties2$State == abbrev, ] #get list of counties in chosen state
#head(cty_select)
#dim(cty_select)
#length(unique(cty_select$CountyName))
#now have county info by state
###

##################################################

#add state names to cty
#county_shape@data <- merge(county_info, county_shape@data, by="GEOID", all.x=TRUE)
#cty@data <- merge(counties2, cty@data, by=c("GEOID", "STATEFP"), all.x=TRUE)

#head(cty@data)

#subset to NC
#NC <- cty[cty@data$State == "CO"|cty@data$State == "KS"|cty@data$State == "NE"|cty@data$State == "WY"|cty@data$State == "SD"|cty@data$State == "ND"|cty@data$State == "MT",]
#460 observations

#NC2 <- cty@data %>%
  #filter(State == "CO"|State == "KS"|State == "NE"|State == "WY"|State == "SD"|State == "ND"|State == "MT")
#460 observations
  

#NC2$lat <- sub('.', '', NC2$INTPTLAT)
#NC2$long <- sub('.', '', NC2$INTPTLON)
#NC2$long <- str_remove(NC2$long, "^0+")

#head(NC2)

#NC2keep <- NC2 %>% select(1, 2, 4, 5, 7, 20:23)
#write.table(NC2, file = "NC2.csv", sep = ",", row.names = FALSE)
#write.table(NC2keep, file = "NC2keep.csv", sep = ",", row.names = FALSE)


#checkstates <- unique(NC2$State)
#has all 7 states


#CO <- NC2keep %>%
  #filter(State == "CO")
#64 counties in CO

#COGEOIDs <- CO$GEOID
#COGEOIDs
#is.factor(COGEOIDs)  


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



BRTE <- BRTE %>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))
  
IMCY<- IMCY%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))

MISI<- MISI%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))

MIVI<- MIVI%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))

NERE<- NERE%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))

PECI<- PECI%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))

SCBA<- SCBA%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))

TACA<- TACA%>% 
  mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
          expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
          retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
          persistant= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))


############################################################
### copy only; did not get for loop to run
species <- list('BRTE', 'IMCY', 'MISI', 'MIVI', 'NERE', 'PECI', 'SCBA', 'TACA')
species

for (i in species) {
#create new column for future agreement in 11 or more models
  i <- i %>% 
    mutate (future85= ifelse(((Future_Models_Sum>=11)), 1, 0),
            expander= ifelse(((sppRichCtyBias_current== 0 & Future_Models_Sum>=11)), 1, 0), 
            retractor= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum<11)), 1, 0),
            persistent= ifelse(((sppRichCtyBias_current== 1 & Future_Models_Sum>=11)), 1, 0))
}
############################################################


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
                        expander_BRTE = expander, retractor_BRTE = retractor, persistent_BRTE = persistent)

IMCYkeep <- IMCYkeep %>% rename(current_IMCY = sppRichCtyBias_current, future85_IMCY = future85, 
                                expander_IMCY = expander, retractor_IMCY = retractor, persistent_IMCY = persistent)

MISIkeep <- MISIkeep %>% rename(current_MISI = sppRichCtyBias_current, future85_MISI = future85, 
                                expander_MISI = expander, retractor_MISI = retractor, persistent_MISI = persistent)

MIVIkeep <- MIVIkeep %>% rename(current_MIVI = sppRichCtyBias_current, future85_MIVI = future85, 
                                expander_MIVI = expander, retractor_MIVI = retractor, persistent_MIVI = persistent)

NEREkeep <- NEREkeep %>% rename(current_NERE = sppRichCtyBias_current, future85_NERE = future85, 
                                expander_NERE = expander, retractor_NERE = retractor, persistent_NERE = persistent)

PECIkeep <- PECIkeep %>% rename(current_PECI = sppRichCtyBias_current, future85_PECI = future85, 
                                expander_PECI = expander, retractor_PECI = retractor, persistent_PECI = persistent)

SCBAkeep <- SCBAkeep %>% rename(current_SCBA = sppRichCtyBias_current, future85_SCBA = future85, 
                                expander_SCBA = expander, retractor_SCBA = retractor, persistent_SCBA = persistent)

TACAkeep <- TACAkeep %>% rename(current_TACA = sppRichCtyBias_current, future85_TACA = future85, 
                                expander_TACA = expander, retractor_TACA = retractor, persistent_TACA = persistent)




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

grasses <- grasses %>%
  mutate(totretractor = retractor_BRTE+retractor_IMCY+retractor_MISI+retractor_MIVI+retractor_NERE+retractor_PECI+retractor_SCBA+retractor_TACA)

write.table(grasses, file = "grasses.csv", sep = ",", row.names = FALSE)


######################################
#join grasses to counties

#first check GEOIDs
#is.numeric(NC3$GEOID) #FALSE
#is.numeric(grasses$GEOID) #TRUE

#is.character(NC3$GEOID) #TRUE

#grassesnum <- grasses

#convert GEOID to numeric
#grassesnum$GEOID <- as.numeric(grassesnum$GEOID)

#is.numeric(grassesnum$GEOID)





#add leading zeros to GEOIDs
## identify GEOIDs that need leading zeros added
grasses$digits <- nchar(grasses$GEOID)
#table(final.table$digits)
grasses$digits

## add leading zeros where needed, otherwise replicate GEOID
grasses$GEOID[grasses$digits == 4] <- paste(0, grasses$GEOID[grasses$digits == 4], sep = "")

table(nchar(grasses$GEOID)) #verify that all FIPS are 5 digits




#subset to NC
GEOIDs <- NC3$GEOID
GEOIDs

grassesNC <- grasses %>%
  filter(GEOID %in% GEOIDs)
#460 observations


#grassesCO <- grasses %>%
  #dplyr::filter(GEOID %>% COGEOIDs)


#grassesCO <- grasses %>%
  #filter(state == "CO")


#countygrasses <- left_join(grassesNC, NC2keep, by = "GEOID")
#396 observations

write.table(grassesNC, file = "countygrasses.csv", sep = ",", row.names = FALSE)

head(grassesNC)
#countygrassesslim <- grassesNC %>% select(1, 42:45)

#colnames(countygrassesslim)[colnames(countygrassesslim) == 'NAME'] <- 'CountyName'
#write.table(countygrassesslim, file = "countygrassesslim.csv", sep = ",", row.names = FALSE)


#statesinNC <- unique(countygrassesslim$State)

##################################
head(countieskeep)
#head(countygrassesslim)

is.numeric(countieskeep$GEOID) #FALSE
#is.numeric(countygrassesslim$GEOID) #FALSE

is.factor(countieskeep$GEOID) #FALSE
#is.factor(countygrassesslim$GEOID) #FALSE

is.character(countieskeep$GEOID) #TRUE
#is.character(countygrassesslim$GEOID) #TRUE
is.character(grassesNC$GEOID) #TRUE


#now need to map counties with grass info
head(cty@data)

#NC <- cty[cty@data$State == "CO"|cty@data$State == "KS"|cty@data$State == "NE"|cty@data$State == "WY"|cty@data$State == "SD"|cty@data$State == "ND"|cty@data$State == "MT",]

NC <- cty@data %>%
  filter(GEOID %in% GEOIDs)

#plot(NC)



#out_file <- NC %>%
  #left_join (., countygrassesslim, by = c('GEOID'))


out_file2 <- read_sf('data/US_counties/WGS84_clip.shp') %>%
  left_join(., grassesNC, by = c('GEOID')) %>%
  filter(GEOID %in% GEOIDs) %>%
  mutate(change = totexpander - totretractor)




head(out_file2)
str(out_file2)

### individual species
#expansion
color_expander <- brewer.pal(n=3, name = "Reds") #3 is the minimum
color_expander

species
head(out_file2)

out_file2$expander_BRTE = factor(out_file2$expander_BRTE, levels = c(0, 1))
out_file2$expander_IMCY = factor(out_file2$expander_IMCY, levels = c(0, 1))
out_file2$expander_MISI = factor(out_file2$expander_MISI, levels = c(0, 1))
out_file2$expander_MIVI = factor(out_file2$expander_MIVI, levels = c(0, 1))
out_file2$expander_NERE = factor(out_file2$expander_NERE, levels = c(0, 1))
out_file2$expander_PECI = factor(out_file2$expander_PECI, levels = c(0, 1))
out_file2$expander_SCBA = factor(out_file2$expander_SCBA, levels = c(0, 1))
out_file2$expander_TACA = factor(out_file2$expander_TACA, levels = c(0, 1))


#check
summary(grassesNC$expander_BRTE)
summary(grassesNC$expander_IMCY)
summary(grassesNC$expander_MISI)
summary(grassesNC$expander_MIVI) #expansion
summary(grassesNC$expander_NERE)
summary(grassesNC$expander_PECI)
summary(grassesNC$expander_SCBA) #expansion
summary(grassesNC$expander_TACA) #expansion


#change this line 
pal <- colorRampPalette(color_expander)

plot(out_file2[23], pal = pal) #no expansion
plot(out_file2[28], pal = pal) #no expansion
plot(out_file2[33], pal = pal) #no expansion
plot(out_file2[38], pal = pal)
plot(out_file2[43], pal = pal) #no expansion
plot(out_file2[48], pal = pal) #no expansion
plot(out_file2[53], pal = pal)
plot(out_file2[58], pal = pal)








#persistence
###
color_persistent <- brewer.pal(n=3, name = "Reds") #3 is the minimum
color_persistent

species
head(out_file2)
str(out_file2)

out_file2$persistent_BRTE = factor(out_file2$persistent_BRTE, levels = c(0, 1))
out_file2$persistent_IMCY = factor(out_file2$persistent_IMCY, levels = c(0, 1))
out_file2$persistent_MISI = factor(out_file2$persistent_MISI, levels = c(0, 1))
out_file2$persistent_MIVI = factor(out_file2$persistent_MIVI, levels = c(0, 1))
out_file2$persistent_NERE = factor(out_file2$persistent_NERE, levels = c(0, 1))
out_file2$persistent_PECI = factor(out_file2$persistent_PECI, levels = c(0, 1))
out_file2$persistent_SCBA = factor(out_file2$persistent_SCBA, levels = c(0, 1))
out_file2$persistent_TACA = factor(out_file2$persistent_TACA, levels = c(0, 1))


#check
summary(grassesNC$persistent_BRTE) #persistent
summary(grassesNC$persistent_IMCY) #not persistent
summary(grassesNC$persistent_MISI) #not persistent
summary(grassesNC$persistent_MIVI) #persistent
summary(grassesNC$persistent_NERE) #not persistent
summary(grassesNC$persistent_PECI) #not persistent
summary(grassesNC$persistent_SCBA) #not persistent
summary(grassesNC$persistent_TACA) #persistent


#change this line 
pal <- colorRampPalette(color_persistant)

plot(out_file2[25], pal = pal) #persistent
plot(out_file2[30], pal = pal) 
plot(out_file2[35], pal = pal) 
plot(out_file2[40], pal = pal) #persistent
plot(out_file2[45], pal = pal) 
plot(out_file2[50], pal = pal) 
plot(out_file2[55], pal = pal)
plot(out_file2[60], pal = pal) #persistent







### species totals
color_status <- brewer.pal(n = 5, name = "Purples")
color_status



#change this line 
pal <- colorRampPalette(color_status, levels = lev)

out_file2$totfuture = factor(out_file2$totfuture, levels = c(0, 1, 2, 3, 4))
out_file2$totcurrent = factor(out_file2$totcurrent, levels = c(0, 1, 2, 3, 4))

plot(out_file2[22], pal = pal)
plot(out_file2[21], pal = pal)
###


###
color_expander <- brewer.pal(n=5, name = "Reds")
color_expander

out_file2$totexpander = factor(out_file2$totexpander, levels = c(0, 1, 2, 3))


#change this line 
pal <- colorRampPalette(color_expander)

plot(out_file2[23], pal = pal)
###


###
color_retractor <- brewer.pal(n=5, name = "Blues")
color_retractor

out_file2$totretractor <- factor(out_file2$totretractor, levels = c(0, 1, 2, 3))

#change this line 
pal <- colorRampPalette(color_retractor)

plot(out_file2[24], pal = pal)
###



###
color_change <- brewer.pal(n=5, name = "RdBu")
color_change

out_file2$change <- factor(out_file2$change, levels = c(2, 1, 0, -1, -2))

#change this line 
pal <- colorRampPalette(color_change)

plot(out_file2[25], pal = pal)
###



summary(out_file2$change)
summary(out_file2$totexpander)
summary(out_file2$totretractor)

#End script here
#########################








#writeOGR(outfile2, 'data/countygrasses.shp', overwrite=TRUE)

td <- file.path("/Users/rana7082/Documents/research/NC_RISCC/data/")
writeOGR(out_file2, td, "countygrasses", driver="ESRI Shapefile")
#writeOGR(cities, td, "cities", driver="ESRI Shapefile")


spdf_fortified <- tidy(NC, region = "Shape_Area")

library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = INTPTLON, y = INTPTLAT, group = group), fill="#69b3a2", color="white") +
  theme_void() 














######################################################################
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
