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

google_api <- "AIzaSyDzHDDwR_AjIzMkk5OT472e2yLtqNgZz0E"
address <- paste(grocery_stores$location_1_location, grocery_stores$location_1_city, grocery_stores$location_1_state, sep=" ")
latitude <- sapply(address, function(place){
  place <- gsub(" ", "+", place)
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", place, "&key=", google_api)
  geoloc <- fromJSON(url)
  geoloc$results$geometry$location[1][1,1]
})

longitude <-  sapply(address, function(place){
  place <- gsub(" ", "+", place)
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", place, "&key=", google_api)
  geoloc <- fromJSON(url)
  geoloc$results$geometry$location[2][1,1]
})

grocery_stores$latitude <- unname(latitude)
grocery_stores$longitude <- unname(longitude)
saveRDS(grocery_stores, file.path(processed_path, "grocery_storesprocessed.rds"))
