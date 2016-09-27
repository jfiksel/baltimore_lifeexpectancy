library(jsonlite)

files_dir <- file.path("data", "raw_data")
file_paths <- list.files(path=files_dir)
file_names <- gsub(".rds", "", file_paths)
for(i in seq_along(file_paths)){
  name <- as.character(file_names[i])
  file <- file.path(files_dir, file_paths[i])
  df <- readRDS(file)
  assign(name, df)
}

processed_path <- file.path("data", "processed_data")
if(!dir.exists(processed_path)){
  dir.create(processed_path)
}
### Cleaning calls911
date <- calls911$calldatetime
calls911$calldatetime <- unname(sapply(date, function(x) substr(x, 1, 10)))
location <- calls911$location
comma_pos <- sapply(location, function(x) regexpr(",", x)[1])
calls911$latitude <- as.numeric(sapply(location, function(x) substr(x, 2, 11)))
calls911$longitude <- as.numeric(sapply(location, function(x) substr(x, 13, (nchar(x)-1))))
calls911$location <- NULL
saveRDS(calls911, file.path(processed_path, "calls911processed.rds"))

### Get latitude and longtitude of grocery stores
colnames(grocery_stores)[c(2,3,4)] <- c('city', 'address', 'state')
google_api <- "AIzaSyDzHDDwR_AjIzMkk5OT472e2yLtqNgZz0E"
full_address <- paste(grocery_stores$address, grocery_stores$city, grocery_stores$state, sep=" ")
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

grocery_stores$latitude <- unname(latitude)
grocery_stores$longitude <- unname(longitude)

saveRDS(grocery_stores, file.path(processed_path, "grocery_storesprocessed.rds"))

### Rename census data
colnames(census2010) <- c("age6_18", "age19_24", "age25_44", "age45_64", "age65_over",
                          "csa2010", "pct_households_w.children", "total_female",
                          "pct_earn_less25k", "pct_earn_25k_40k", "pct_earn_40k_60k",
                          "pct_earn_60k_75k", "pct_earn_more75k", "total_householdsize",
                          "avg_householdsize", "total_males", "median_household_income",
                          "pct_2ormore_races", "pct_black", "pct_asian", "pct_hispanic",
                          "perna", "pct_otherrace", "pct_islander","pct_white",
                          "racial_diversity_indx", "total_population")
saveRDS(census2010, file.path(processed_path, "census2010processed.rds"))

### Separate well being into different data frames by year
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

### Separate housing development into different data frames by year
cnames.old <- unique(gsub("10|11|12|13|14","", colnames(housing_devel)))
cnames.new <- c('affordability_mortgage', 'affordability_rent',
                'pct_vacant_bmore', 'pct_sales_forcash', 'construction_permit_per1000',
                'demolition_permit_per1000', 'median_days_on_market',
                'pct_mortgage_foreclosure', 'hcvhouse', 
                'homestead_tax_credits_per1000', 'csa',
                'pct_no_receive_mail', 'pct_housing_owner_occupied',
                'homeowners_tax_credits_per1000', 'pct_residential_sales_foreclosure',
                'pct_w_rehab_permits', 'median_home_price', 'number_homes_sold', 'total_res_properties',
                'pct_vacant_peroperties', 'pct_housing_violations')
names(cnames.new) <- cnames.old

housing_devel2010 <- housing_devel[,grep("10|neighborhood", colnames(housing_devel))]
colnames(housing_devel2010) <- cnames.new[gsub("10", "", colnames(housing_devel2010))]
saveRDS(housing_devel2010, file.path(processed_path, "housing_devel2010.rds"))

housing_devel2011 <- housing_devel[,grep("11|neighborhood", colnames(housing_devel))]
colnames(housing_devel2011) <- cnames.new[gsub("11", "", colnames(housing_devel2011))]
saveRDS(housing_devel2011, file.path(processed_path, "housing_devel2011.rds"))

housing_devel2012 <- housing_devel[,grep("12|neighborhood", colnames(housing_devel))]
colnames(housing_devel2012) <- cnames.new[gsub("12", "", colnames(housing_devel2012))]
saveRDS(housing_devel2012, file.path(processed_path, "housing_devel2012.rds"))

housing_devel2013 <- housing_devel[,grep("13|neighborhood", colnames(housing_devel))]
colnames(housing_devel2013) <- cnames.new[gsub("13", "", colnames(housing_devel2013))]
saveRDS(housing_devel2013, file.path(processed_path, "housing_devel2013.rds"))

housing_devel2014 <- housing_devel[,grep("14|neighborhood", colnames(housing_devel))]
colnames(housing_devel2014) <- cnames.new[gsub("14", "", colnames(housing_devel2014))]
saveRDS(housing_devel2014, file.path(processed_path, "housing_devel2014.rds"))
