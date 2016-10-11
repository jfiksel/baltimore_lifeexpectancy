library(sp)
library(leaflet)
library(maptools)
library(ggplot2)
library(plyr)
library(ggmap)
library(acs)
library(tigris)

csa <- readRDS(list.files(path=file.path("data", "raw_data"),
                          pattern="csa_shapes.rds", full.names=TRUE))
blocks <- readRDS(list.files(path=file.path("data", "raw_data"),
                             pattern="census_blocks.rds", full.names=TRUE))

well_being <- readRDS(list.files(file.path("data", "processed_data"),
                      pattern="well_being2014.rds", full.names=TRUE))
census2010 <- readRDS(list.files(file.path("data", "processed_data"),
                                 pattern="census2010processed.rds", full.names=TRUE))
property_taxes <- readRDS(list.files(file.path("data", "processed_data"),
                                     pattern="property_taxes.rds", full.names=TRUE))
victim_crime2015 <- readRDS(list.files(file.path("data", "processed_data"),
                                       pattern="victim_crime2015.rds", full.names=TRUE))
victim_crime2014 <- readRDS(list.files(file.path("data", "processed_data"),
                                       pattern="victim_crime2014.rds", full.names=TRUE))
education_df <- readRDS(list.files(file.path("data", "processed_data"),
                                   pattern="acs_education.rds", full.names = TRUE))
income_df <- readRDS(list.files(file.path("data", "processed_data"),
                                   pattern="acs_income.rds", full.names = TRUE))
block_groups <- readRDS(list.files(file.path("data", "processed_data"),
                                   pattern="block_groups.rds", full.names = TRUE))
liquor.stores <- readRDS(list.files(file.path("data", "processed_data"),
                                   pattern="liquor_stores.rds", full.names = TRUE))
housing_market <- readRDS(list.files(file.path("data", "processed_data"),
                                    pattern="housing_market.rds", full.names = TRUE))

key <- na.omit(match(csa@data$Community, well_being$csa))
### Remove jail
csa <- csa[-51,]

### CSA coordinates to latitude and longitude
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
csa <- spTransform(csa,  llprj)


### Plot CSA life expectancy 
csa@data$id <- csa@data$Community
csa@data$life_expectancy <- well_being$life_exepctancy[key]
csa.points <- fortify(csa, region="id")
csa.df <- join(csa.points, csa@data, by="id")
csa.df$life_expectancy <- as.numeric(csa.df$life_expectancy)

myggmap <- get_map(location="Baltimore", zoom=12)
ggmap(myggmap)+ 
  #geom_polygon(data=csa.df, aes(x=long, y=lat, group=group)) +
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black") 


### Plot income taxes by household

### First neighborhood to CSA
csaname <- csa@data$Community
neighborhood <- csa@data$Neigh
neighborhood <- sapply(neighborhood, function(x) strsplit(as.character(x), ", "))
times <- sapply(neighborhood, length)
neighborhood <- toupper(unlist(neighborhood))
csakey <-rep(csaname, times=times)
names(csakey) <- neighborhood

property_taxes <- property_taxes[property_taxes$neighborhood %in% names(csakey),]
property_taxes$csa <- csakey[property_taxes$neighborhood]
### Now plot
ggmap(myggmap)+
  geom_point(data=property_taxes[property_taxes$citytax <=20000,],
             aes(x=longitude, y=latitude,colour=log(citytax)), alpha=.3) +
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black") +
  scale_colour_gradientn("City taxes", colors=c('red', 'yellow', 'lightgreen', 'darkgreen')) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Property Taxes")

### Plot shootings
victim_crime <- rbind(victim_crime2014, victim_crime2015)
ggmap(myggmap)+
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black")  +
  geom_polygon(data=csa.df, aes(x=long, y=lat, group=group, fill=life_expectancy), alpha=.4) +
  geom_point(data=victim_crime[victim_crime$description=="SHOOTING",], aes(x=longitude, y=latitude), alpha=.5) +
  scale_fill_gradientn("Life expectancy", colors=c('red', 'yellow', 'green')) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Life Expectancy and Shootings 2015 & 2014")

### Plot property crime
### Note for property crime, definition is
### larceny, auto theft, burglary, arson
### c(1, 3, 6, 7, 8)
property.crime <- unique(victim_crime2015$description)[c(1, 3, 6, 7, 8)]
ggmap(myggmap)+
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black")  +
  geom_point(data=victim_crime2015[victim_crime2015$description %in% property.crime,], aes(x=longitude, y=latitude, color=description), alpha=.5) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Property Crime 2015")

### Just burglary
ggmap(myggmap)+
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black")  +
  geom_polygon(data=csa.df, aes(x=long, y=lat, group=group, fill=life_expectancy), alpha=.4) +
  geom_point(data=victim_crime2015[victim_crime2015$description=="BURGLARY",], aes(x=longitude, y=latitude), alpha=.2) +
  scale_fill_gradientn("Life expectancy", colors=c('red', 'yellow', 'green')) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Life Expectancy and Burglary 2015")


### Income by block group

bmore_merged <- geo_join(block_groups, income_df, "GEOID", "GEOID")

bmore_merged@data$id <- bmore_merged@data$GEOID
bmore_merged.points <- fortify(bmore_merged, region="id")
bmore_merged.df <- join(bmore_merged.points, bmore_merged@data, by="id")

ggmap(myggmap)+
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=bmore_merged.df, aes(x=long, y=lat, group=group, fill=hhincome), alpha=0.5)+
  scale_fill_gradientn("Average Household Income", colors=c('red', 'yellow', 'lightgreen', 'darkgreen')) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Block Group Household Income 2014")

### Foreclosures and median house price sale by block group
bmore_merged2 <- geo_join(block_groups, housing_market, "GEOID", "blockgroup")
bmore_merged2@data$id <- bmore_merged2@data$GEOID
bmore_merged.points <- fortify(bmore_merged2, region="id")
bmore_merged2.df <- join(bmore_merged.points, bmore_merged2@data, by="id")
bmore_merged2.df$foreclosurepct[bmore_merged2.df$foreclosurepct > 1 & !is.na(bmore_merged2.df$foreclosurepct)]

ggmap(myggmap)+
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=bmore_merged2.df, aes(x=long, y=lat, group=group, fill=mediansalesprice), alpha=0.5)+
  scale_fill_gradientn("Median Sales Price", colors=c('red', 'yellow', 'lightgreen', 'darkgreen')) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Block Group Median House Sale Price")

bmore_merged2.df <- subset(bmore_merged2.df, foreclosurepct < .2)
ggmap(myggmap)+
  geom_path(data=csa.df, aes(x=long, y=lat, group=group), color="black") +
  geom_polygon(data=bmore_merged2.df, aes(x=long, y=lat, group=group, fill=foreclosurepct), alpha=0.5)+
  scale_fill_gradientn("Foreclosure Percentage", colors=c('darkgreen', 'lightgreen', 'yellow', 'red')) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Baltimore Block Group Foreclosure Percentage")

