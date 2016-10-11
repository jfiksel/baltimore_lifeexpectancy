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

### CSA life expectancy 
csa@data$id <- csa@data$Community
csa@data$life_expectancy <- well_being$life_exepctancy[key]
csa.points <- fortify(csa, region="id")
csa.df <- join(csa.points, csa@data, by="id")

### Look at variation and mean of property_taxes within CSA

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

mean.citytax <- property_taxes %>% 
  group_by(csa) %>%
  summarize(meantax=mean(log(citytax+1)))

csa@data$life_expectancy <- as.numeric(csa@data$life_expectancy)
cor(csa@data$life_expectancy, mean.citytax$meantax)
plot(csa@data$life_expectancy, mean.citytax$meantax, 
     ylab="Mean of Log City Taxes in CSA", xlab="Life Expectancy")

var.citytax <- property_taxes %>% 
  group_by(csa) %>%
  summarize(var=var(log(citytax+1)))

cor(csa@data$life_expectancy, var.citytax$var)
plot(csa@data$life_expectancy, var.citytax$var, 
     ylab="Variance of Log City Taxes in CSA", xlab="Life Expectancy")

plot(mean.citytax$meantax, var.citytax$var)
