library(jsonlite)
if(!dir.exists("data")){
  dir.create("data")
}
if(!dir.exists(file.path("data", "raw_data"))){
  dir.create(file.path("data", "raw_data"))
}
well_being <- fromJSON("https://data.baltimorecity.gov/resource/ivtw-hiv6.json")
dest <- file.path("data", "raw_data", "well_being.rds")
saveRDS(well_being, dest)

housing_devel <- fromJSON("https://data.baltimorecity.gov/resource/wpgb-3mej.json")
dest <- file.path("data", "raw_data", "housing_devel.rds")
saveRDS(housing_devel, dest)

vacant_buildings <- fromJSON("https://data.baltimorecity.gov/resource/rw5h-nvv4.json")
dest <- file.path("data", "raw_data", "vacant_buildings.rds")
saveRDS(vacant_buildings, dest)

victim_crime <- fromJSON("https://data.baltimorecity.gov/resource/4ih5-d5d5.json")
dest <- file.path("data", "raw_data", "victim_crime.rds")
saveRDS(victim_crime, dest)

property_taxes <- fromJSON("https://data.baltimorecity.gov/resource/6act-qzuy.json")
dest <- file.path("data", "raw_data", "property_taxes.rds")
saveRDS(property_taxes, dest)

liquor_licenses <- fromJSON("https://data.baltimorecity.gov/resource/g2jf-x8pp.json")
dest <- file.path("data", "raw_data", "liquor_licenses.rds")
saveRDS(liquor_licenses, dest)
census2010 <- fromJSON("https://data.baltimorecity.gov/resource/ygvc-86i7.json")
dest <- file.path("data", "raw_data", "census2010.rds")
saveRDS(census2010, dest)

grocery_stores <- fromJSON("https://data.baltimorecity.gov/resource/8gms-s9we.json")
dest <- file.path("data", "raw_data", "grocery_stores.rds")
saveRDS(grocery_stores, dest)

calls911 <- fromJSON("https://data.baltimorecity.gov/resource/m8g9-abgb.json")
dest <- file.path("data", "raw_data", "calls911.rds")
saveRDS(calls911, dest)



