library(jsonlite)
library(tigris)
library(rgdal)
library(downloader)
library(acs)
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

liquor_stores <- fromJSON("https://data.baltimorecity.gov/resource/hew9-k3x4.json")
dest <- file.path("data", "raw_data", "liquor_stores.rds")
saveRDS(liquor_stores, dest)

census2010 <- fromJSON("https://data.baltimorecity.gov/resource/ygvc-86i7.json")
dest <- file.path("data", "raw_data", "census2010.rds")
saveRDS(census2010, dest)

grocery_stores <- fromJSON("https://data.baltimorecity.gov/resource/8gms-s9we.json")
dest <- file.path("data", "raw_data", "grocery_stores.rds")
saveRDS(grocery_stores, dest)

calls911 <- fromJSON("https://data.baltimorecity.gov/resource/m8g9-abgb.json")
dest <- file.path("data", "raw_data", "calls911.rds")
saveRDS(calls911, dest)

housing_market <- fromJSON("https://data.baltimorecity.gov/resource/7p9s-x9xv.json")
dest <- file.path('data', 'raw_data', 'housing_market.rds')
saveRDS(housing_market, dest)

### CSA shape file

url <- 'http://bniajfi.org/wp-content/uploads/2014/04/csa_2010_boundaries.zip'
download(url, dest=file.path('data', 'raw_data', 'shapes.zip'))
unzip(file.path('data', 'raw_data', 'shapes.zip'),
      exdir=file.path('data', 'raw_data'))
csa_shapes <- readOGR(file.path("data", "raw_data"), "CSA_NSA_Tracts")
dest <- file.path("data", "raw_data", "csa_shapes.rds")
saveRDS(csa_shapes, dest)

### Neighborhood shape file
url <- 'http://gis.baltimore.opendata.arcgis.com/datasets/1ca93e68f11541d4b59a63243725c4b7_0.zip'
download.file(url, dest=file.path('data', 'raw_data', 'neigh_shapes.zip'))
unzip(file.path('data', 'raw_data', 'neigh_shapes.zip'),
      exdir=file.path('data', 'raw_data'))
neighborhood_shapes <- readOGR(file.path("data", "raw_data"), "Neighborhoods")
dest <- file.path("data", "raw_data", "neighborhood_shapes.rds")
saveRDS(neighborhood_shapes, dest)
### Baltimore 
### lookup_code("Maryland", "Baltimore City")
###  "The code for Maryland is '24' and the code for Baltimore city is '510'."
### If error message occurs in line below, use:
###    options(tigris_use_cache = FALSE)
### Code can take a while to run
block_defs <- blocks(state = 24, county = 510)
dest <- file.path("data", "raw_data", "census_blocks.rds")
saveRDS(block_defs, dest)

### ACS data
### Average household income
acs_api <- "9de0607b39f202d656f833c9ed107b4d7e62ac0d"
api.key.install(acs_api)
income_data <- acs.fetch(endyear=2014,
                         geography=geo.make(state="MD",
                                            county=510,
                                            tract="*",
                                            block.group="*"),
                         table.number="B19013")
dest <- file.path("data", "raw_data", "acs_income.rds")
saveRDS(income_data, dest)
education_data <- acs.fetch(endyear=2014,
                         geography=geo.make(state="MD",
                                            county=510,
                                            tract="*",
                                            block.group="*"),
                         table.number="B15002")
dest <- file.path("data", "raw_data", "acs_education.rds")
saveRDS(education_data, dest)

### block groups
block.groups <- block_groups(state="MD", county=510)
dest <- file.path("data", "raw_data", "block_groups.rds")
saveRDS(block.groups, dest)




