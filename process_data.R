library(jsonlite)
library(plyr)
library(dplyr)
library(KernSmooth)
library(rgeos)
library(fields)
library(sp)
library(tigris)

files_dir <- file.path("data", "raw_data")
file_paths <- list.files(path=files_dir, pattern=".rds")
file_names <- gsub(".rds", "", file_paths)

processed_path <- file.path("data", "processed_data")
if(!dir.exists(processed_path)){
  dir.create(processed_path)
}

### Vacant buildings(point -> density)
### Shootings (point -> density)
### Burglary (point -> density)
### Fast food (point -> density)
### liquor stores (point -> density)
### Average household income (block group)
### Race (block group, %)
### Sex (block group, %)
### Education(block group)
### Single mothers (block group, % of total families with children)



### Vacant buildings
vacant_buildings <- readRDS(file.path(files_dir, "vacant_buildings.rds"))
coords <- vacant_buildings$location$coordinates
vacant_coords <- data.frame(longitude=unlist(sapply(coords, function(coord) coord[1])),
                            latitude=unlist(sapply(coords, function(coord) coord[2])))
saveRDS(vacant_coords, file.path(processed_path, "vacant_coords.rds"))

### Shootings and burglaries 2014, 2015 & 2016 from victim based crime
#### Victim based crimes
victim_crime <- readRDS(file.path(files_dir, "victim_crime.rds"))
coordinates <- victim_crime$location_1$coordinates
victim_crime$location_1 <- NULL
isnull <- sapply(coordinates, function(x) is.null(x))
victim_crime <- victim_crime[!isnull,]
victim_crime$longitude <- unlist(sapply(coordinates, function(x) x[1]))
victim_crime$latitude <- unlist(sapply(coordinates, function(x) x[2]))
victim_crime <- victim_crime[,-c(1,2,4,10:12)]
victim_crime$longitude <- as.numeric(victim_crime$longitude)
victim_crime$latitude <- as.numeric(victim_crime$latitude)
year <- as.integer(substr(victim_crime$crimedate,1,4))
victim_crime$crimedate <- NULL
victim_crime$year <- year
victim_crime$description <- tolower(victim_crime$description)
shooting_coords <- victim_crime %>% 
  filter(year==2016|year==2015|year==2014 & description=="shooting") %>%
  select(longitude, latitude, year)
burglary_coords <- victim_crime %>% 
  filter(year==2016|year==2015|year==2014 & description=="burglary") %>%
  select(longitude, latitude, year)
saveRDS(shooting_coords, file.path(processed_path, "shooting_coords.rds"))
saveRDS(burglary_coords, file.path(processed_path, "burglary_coords.rds"))
saveRDS(victim_crime[victim_crime$year==2011,], file.path(processed_path, "victim_crime2011.rds"))
saveRDS(victim_crime[victim_crime$year==2012,], file.path(processed_path, "victim_crime2012.rds"))
saveRDS(victim_crime[victim_crime$year==2013,], file.path(processed_path, "victim_crime2013.rds"))
saveRDS(victim_crime[victim_crime$year==2014,], file.path(processed_path, "victim_crime2014.rds"))
saveRDS(victim_crime[victim_crime$year==2015,], file.path(processed_path, "victim_crime2015.rds"))
saveRDS(victim_crime[victim_crime$year==2016,], file.path(processed_path, "victim_crime2016.rds"))

### Restaurants
restaurants <- readRDS(file.path(files_dir, "restaurants.rds"))
name <- tolower(restaurants$name)
name <- gsub(" |'", "", name)
name[grepl("mcdonalds", name)] <- "mcdonalds"
name[grepl("burgerking", name)] <- "burgerking"
name[grepl("kentucky|kfc", name)] <- "kfc"
name[grepl("tacobell", name)] <- "tacobell"
name[grepl("popeyesfamous", name)] <- "popeyes"
name[grepl("wendys", name)] <- "wendys"
restaurants$name <- name
fastfood <- restaurants %>% filter(name=="mcdonalds"|
                                     name=="burgerking"|
                                     name=="kfc"|
                                     name=="tacobell"|
                                     name=="popeyes"|
                                     name=="wendys")
### Get fast food coords
colnames(fastfood)[2:4]<- c('city', 'address', 'state')
google_api <- "AIzaSyDzHDDwR_AjIzMkk5OT472e2yLtqNgZz0E"
full_address <- paste(fastfood$address, fastfood$city, fastfood$state, sep=" ")
latitude <- sapply(full_address, function(place){
  place <- gsub(" ", "+", place)
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", place, "&key=", google_api)
  geoloc <- fromJSON(url)
  geoloc$results$geometry$location[1][1,1]
})

longitude <-  sapply(full_address, function(place){
  place <- gsub(" ", "+", place)
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", place, "&key=", google_api)
  geoloc <- fromJSON(url)
  geoloc$results$geometry$location[2][1,1]
})
fastfood_coords <- data.frame(longitude=unname(longitude),
                              latitude=unname(latitude))
saveRDS(fastfood_coords, file.path(processed_path, "fastfoods.rds"))

### Liquor stores
liquor_stores <- readRDS(file.path(files_dir, "liquor_stores.rds"))
liquor_stores <- subset(liquor_stores, licensestatus=="Renewed")
liquor_stores <- data.frame(longitude=liquor_stores$location_1$longitude,
                            latitude=liquor_stores$location_1$latitude)
liquor_coords <- subset(liquor_stores, !duplicated(liquor_stores))
liquor_coords <- as.data.frame(liquor_coords)
liquor_coords <- sapply(liquor_coords, function(col) as.numeric(as.character(col)))
liquor_coords <- as.data.frame(liquor_coords)
saveRDS(liquor_coords, file.path(processed_path, "liquor_stores.rds"))

### Household income
income_data <- readRDS(file.path(files_dir, "acs_income.rds"))                           

income_df <- data.frame(GEOID=paste0(as.character(income_data@geography$state), 
                                    as.character(income_data@geography$county),
                                    income_data@geography$tract,
                                    income_data@geography$blockgroup), 
                        hhincome=income_data@estimate[,1])
saveRDS(income_df, file.path(processed_path, "acs_income.rds"))

### Race by block group
race <- readRDS(file.path(files_dir, "acs_race.rds"))
race_df <- data.frame(GEOID=paste0(as.character(race@geography$state), 
                                  as.character(race@geography$county),
                                  race@geography$tract,
                                  race@geography$blockgroup),
                      pctwhite=race@estimate[,2]/race@estimate[,1],
                      pctblack=race@estimate[,3]/race@estimate[,1])
saveRDS(race_df, file.path(processed_path, "acs_race.rds"))

### Sex by block group
sex <- readRDS(file.path(files_dir, "acs_sexbyage.rds"))
sex_df <- data.frame(GEOID=paste0(as.character(sex@geography$state), 
                          as.character(sex@geography$county),
                          sex@geography$tract,
                          sex@geography$blockgroup),
                     pctmale=sex@estimate[,2]/sex@estimate[,1],
                     pctfemale=sex@estimate[,26]/sex@estimate[,1],
                     totalpop=sex@estimate[,1])
saveRDS(sex_df, file.path(processed_path, "acs_sex.rds"))
### Education by block group
education_data <- readRDS(file.path(files_dir, "acs_education.rds"))
point.estimates <- education_data@estimate
total <- point.estimates[,1]
less.highschool <- rowSums(point.estimates[,c(3:10, 20:27)])
highschool <- rowSums(point.estimates[,c(11,28)])
somecollege <- rowSums(point.estimates[,c(12,13,29:30)])
college <- rowSums(point.estimates[,c(14,15,31,32)])
graduate <- rowSums(point.estimates[,c(16:18, 33:35)])
education_df <- data.frame(GEOID=paste0(as.character(income_data@geography$state), 
                                       as.character(income_data@geography$county),
                                       income_data@geography$tract,
                                       income_data@geography$blockgroup),
                           pct.lesshighschool=less.highschool/total,
                           pct.highschool=highschool/total,
                           pct.somecollege=somecollege/total,
                           pct.college=college/total,
                           pct.graduate=graduate/total)
#education_df <- education_df[!is.na(rowSums(education_df[,-1])),]
saveRDS(education_df, file.path(processed_path, "acs_education.rds"))

### Percentage of parents that are single moms
sexbyfamtype <- readRDS(file.path(files_dir, "acs_childrenbyfamtype.rds"))
singlemom_df <- data.frame(GEOID=paste0(as.character(sexbyfamtype@geography$state), 
                                       as.character(sexbyfamtype@geography$county),
                                       sexbyfamtype@geography$tract,
                                       sexbyfamtype@geography$blockgroup),
                           pctsinglemom=sexbyfamtype@estimate[,15]/sex@estimate[,1])
singlemom_df$pctsinglemom[is.na(singlemom_df$pctsinglemom)] <- 0
saveRDS(singlemom_df, file.path(processed_path, "acs_singlemom.rds"))

### Block group shapes
block.groups <- readRDS(file.path(files_dir, "block_groups.rds"))
k <- which(substr(block.groups@data$TRACTCE, 1, 1)=="0")
block.groups@data[k,]$GEOID <- paste0(substr(block.groups@data[k,]$GEOID, 1, 4), substr(block.groups@data[k,]$GEOID, 6, 12))
block.groups@data$GEOID <- factor(block.groups@data$GEOID)
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
block.groups <- spTransform(block.groups, llprj)
saveRDS(block.groups, file.path(processed_path, "block_groups.rds"))

### Combine data to block groups
blockgroup_data <- join_all(list(income_df, race_df, sex_df, education_df, singlemom_df))
blockgroup_merged <- geo_join(block.groups, blockgroup_data, "GEOID", "GEOID")
blockgroup_datamissing <- is.na(blockgroup_merged@data)
notmissing <- rowSums(blockgroup_datamissing)==0
blockgroup_merged <- blockgroup_merged[notmissing,]

### CSA Shape File
csa <- readRDS(list.files(path=file.path("data", "raw_data"),
                          pattern="csa_shapes.rds", full.names=TRUE))
### Remove jail
csa <- csa[-51,]
csa@data <- droplevels(csa@data)
### CSA coordinates to latitude and longitude
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
csa <- spTransform(csa,  llprj)
saveRDS(csa, file.path(processed_path, "csa_shapes.rds"))

### Blocks
blocks <- readRDS(list.files(path=file.path("data", "raw_data"),
                             pattern="census_blocks.rds", full.names=TRUE))
llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
blocks <- spTransform(blocks,  llprj)


### Assign block group data to blocks
k1 <- which(substr(blocks@data$TRACTCE10, 1, 1)=="0")
blocks@data[k1,]$GEOID10 <- paste0(substr(blocks@data[k1,]$GEOID10, 1, 4), substr(blocks@data[k1,]$GEOID10, 6, 12))
k2 <- which(substr(blocks@data$TRACTCE10, 1, 1)!="0")
blocks@data[k2,]$GEOID10 <- substr(blocks@data[k2,]$GEOID10, 1, nchar(blocks@data[k2,]$GEOID10)-3)
blocks@data$GEOID10 <- factor(blocks@data$GEOID10)
match.blockgroup <- match(blocks@data$GEOID10, blockgroup_merged@data$GEOID)
blocks@data <- cbind(blocks@data,blockgroup_merged@data[match.blockgroup,14:ncol(blockgroup_merged@data),])
which.keep <- rowSums(is.na(blocks@data[,16:ncol(blocks@data)]))==0
blocks <- blocks[which.keep,]

### CSA for each block
which.csa <- over(blocks, csa)
blocks@data$csa <- which.csa$Community
blocks <- blocks[!is.na(blocks@data$csa),]

### Smoothing of point data
### First get borders of latitude and longitude for blockss
min.longitude <- min(sapply(1:length(blocks), function(i) min(blocks@polygons[i][[1]]@Polygons[[1]]@coords[,1])))
max.longitude <- max(sapply(1:length(blocks), function(i) max(blocks@polygons[i][[1]]@Polygons[[1]]@coords[,1])))
min.latitude <- min(sapply(1:length(blocks), function(i) min(blocks@polygons[i][[1]]@Polygons[[1]]@coords[,2])))
max.latitude <- max(sapply(1:length(blocks), function(i) max(blocks@polygons[i][[1]]@Polygons[[1]]@coords[,2])))

### Centers of blocks
blockcenters <- gCentroid(blocks, byid=TRUE)

### KernSmooth point data
### vacant_coords, shooting_coords, burglary_coords,fastfood_coords,liquor_coords
### Vaccant buildies 
vacant_dens <- bkde2D(as.matrix(vacant_coords), bandwidth=c(.004, .004), 
                        range.x=list(c(min.longitude, max.longitude), 
                                     c(min.latitude, max.latitude)))
obj <- list(x=vacant_dens$x1, y=vacant_dens$x2, z=vacant_dens$fhat)
vacant_pred <- interp.surface(obj, blockcenters@coords)
blocks@data$vacant_density <- vacant_pred
### Example of plotting
#plotdf <- data.frame(long=blockcenters@coords[,1], lat=blockcenters@coords[,2], pred=vacant_pred)
#plotdf <- subset(plotdf, !is.na(pred))
#ggplot()+ geom_point(data=plotdf, aes(x=long, y=lat, color=pred))+
#  scale_color_gradientn("Vacant building density", colors=c('darkgreen', 'lightgreen', 'yellow', 'red'))
### Shootings
shooting_dens <- bkde2D(as.matrix(shooting_coords), bandwidth=c(.004, .004), 
               range.x=list(c(min.longitude, max.longitude), 
                            c(min.latitude, max.latitude)))
obj <- list(x=shooting_dens$x1, y=shooting_dens$x2, z=shooting_dens$fhat)
shooting_pred <- interp.surface(obj, blockcenters@coords)
blocks@data$shooting_density <- shooting_pred

### Burglaries 
burglary_dens <- bkde2D(as.matrix(burglary_coords), bandwidth=c(.004, .004), 
                        range.x=list(c(min.longitude, max.longitude), 
                                     c(min.latitude, max.latitude)))
obj <- list(x=burglary_dens$x1, y=burglary_dens$x2, z=burglary_dens$fhat)
burglary_pred <- interp.surface(obj, blockcenters@coords)
blocks@data$burglary_density <- burglary_pred

### Fast food
fastfood_dens <- bkde2D(as.matrix(fastfood_coords), bandwidth=c(.004, .004), 
                        range.x=list(c(min.longitude, max.longitude), 
                                     c(min.latitude, max.latitude)))
obj <- list(x=fastfood_dens$x1, y=fastfood_dens$x2, z=fastfood_dens$fhat)
fastfood_pred <- interp.surface(obj, blockcenters@coords)
blocks@data$fastfood_density <- fastfood_pred

### Liquor stores
liquor_dens <- bkde2D(as.matrix(liquor_coords), bandwidth=c(.004, .004), 
                        range.x=list(c(min.longitude, max.longitude), 
                                     c(min.latitude, max.latitude)))
obj <- list(x=liquor_dens$x1, y=liquor_dens$x2, z=liquor_dens$fhat)
liquor_pred <- interp.surface(obj, blockcenters@coords)
blocks@data$liquor_density <- liquor_pred
test.na <- is.na(blocks@data[,16:ncol(blocks@data)])
keep <- rowSums(test.na)==0
blocks <- blocks[keep,]
saveRDS(blocks, file.path(processed_path, "block_final.rds"))
### Assign average of point variables to CSA

csa.summarize <- blocks@data %>% group_by(csa) %>%
  summarize(vacant_density=mean(vacant_density),
            shooting_density=mean(shooting_density),
            burglary_density=mean(burglary_density),
            fastfood_density=mean(fastfood_density),
            liquor_density=mean(liquor_density))
order.csa <- order(csa.summarize$csa)
names(order.csa) <- csa.summarize$csa
csa.summarize <- csa.summarize[order.csa[csa@data$Community],]
csa@data <- cbind(csa@data, csa.summarize[,2:ncol(csa.summarize)])

### Block group to CSA (weighted average)
### First assign block group to CSA based on number of blocks within that block group in each CSA
blocks@data$blockgroup <- blocks@data$GEOID10
which.csa <- blocks@data %>% group_by(blockgroup, csa) %>% 
  summarize(count=n()) %>%
  group_by(blockgroup) %>%
  filter(count==max(count))
which.csa <- subset(which.csa, !duplicated(blockgroup))
csa.vector <- which.csa$csa
names(csa.vector) <- which.csa$blockgroup
blockgroup_merged@data$csa <-unname(csa.vector[blockgroup_merged@data$GEOID])
blockgroup_merged <- blockgroup_merged[!is.na(blockgroup_merged@data$csa),]
saveRDS(blockgroup_merged, file.path(processed_path, "blockgroup_final.rds"))

csa.summarize <- blockgroup_merged@data %>% group_by(csa) %>%
  mutate(weight=totalpop/sum(totalpop)) %>%
  summarize(hhincome=weighted.mean(hhincome, weight),
            pctblack=weighted.mean(pctblack, weight),
            pctmale=weighted.mean(pctmale, weight),
            totalpop=weighted.mean(totalpop, weight),
            pct.lesshighschool=weighted.mean(pct.lesshighschool, weight),
            pct.highschool=weighted.mean(pct.highschool, weight),
            pct.somecollege=weighted.mean(pct.somecollege, weight),
            pct.college=weighted.mean(pct.college, weight),
            pct.graduate=weighted.mean(pct.graduate, weight),
            pct.singlemom=weighted.mean(pctsinglemom, weight))

order.csa <- order(csa.summarize$csa)
names(order.csa) <- csa.summarize$csa
csa.summarize <- csa.summarize[order.csa[csa@data$Community],]
csa@data <- cbind(csa@data, csa.summarize[,2:ncol(csa.summarize)])
csa@data <- csa@data[,-c(2,3)]
colnames(csa@data)[1] <- "community"           
### Separate well being into different data frames by year
well_being <- readRDS(file.path(files_dir, "well_being.rds"))
colnames(well_being)[6] <- "csa"
cnames.old <- unique(gsub("10$|11$|12$|13$|14$|2010$","", colnames(well_being)))
cnames.old <- gsub("_", "", cnames.old)
cnames.new <- c('pct_satisfact_birthwt', "csa", "pct_chldrn_high_lead",
                'fast_food_density', 'chldrn_tested_lead', 'life_exepctancy',
                'liquor_outlet_density', 'mortality_1to14', 'mortality_less1',
                'mortality_15to24', 'mort_25to44','mortality_45to64',
                'mortality_65to84', 'mortality_more85', 
                'pct_births_early_care', 'pct_receive_tanf',
                'teen_birth_rate', 'pct_births_delivered_trm')
names(cnames.new) <- cnames.old
well_being2010 <- well_being[,grep("10$|csa", colnames(well_being))]
colnames(well_being2010) <- cnames.new[gsub("10$|_","" ,colnames(well_being2010))]
saveRDS(well_being2010, file.path(processed_path, "well_being2010.rds"))

well_being2011 <- well_being[,grep("11$|csa", colnames(well_being))]
colnames(well_being2011) <- cnames.new[gsub("11$|_","" ,colnames(well_being2011))]
saveRDS(well_being2011, file.path(processed_path, "well_being2011.rds"))

well_being2012 <- well_being[,grep("12$|csa", colnames(well_being))]
colnames(well_being2012) <- cnames.new[gsub("12$|_","" ,colnames(well_being2012))]
saveRDS(well_being2012, file.path(processed_path, "well_being2012.rds"))

well_being2013 <- well_being[,grep("13$|csa", colnames(well_being))]
colnames(well_being2013) <- cnames.new[gsub("13$|_","" ,colnames(well_being2013))]
saveRDS(well_being2013, file.path(processed_path, "well_being2013.rds"))

well_being2014 <- well_being[,grep("14$|csa", colnames(well_being))]
colnames(well_being2014) <- cnames.new[gsub("14$|_","" ,colnames(well_being2014))]
saveRDS(well_being2014, file.path(processed_path, "well_being2014.rds"))

### Life expectancy to CSA
well_being <- well_being2014
key <- na.omit(match(csa@data$community, well_being$csa))
csa@data$life_expectancy <- as.numeric(well_being$life_exepctancy[key])
saveRDS(csa, file.path(processed_path, "final_csa.rds"))

### Property Taxes
property_taxes <- readRDS(file.path(files_dir, "property_taxes.rds"))
property_taxes <- property_taxes[property_taxes$location$coordinates!="NULL",]
coordinates <-property_taxes$location$coordinates
property_taxes <- property_taxes[,c(2,7, 12, 15)]
property_taxes$longitude <- sapply(coordinates, function(x) x[1])
property_taxes$latitude <- sapply(coordinates, function(x) x[2])
property_taxes$citytax <- as.numeric(property_taxes$citytax)
property_taxes <- property_taxes[!is.na(property_taxes$citytax),]
property_taxes$rescode <- gsub(" ", "", property_taxes$rescode)
saveRDS(property_taxes, file.path(processed_path, "property_taxes.rds"))





