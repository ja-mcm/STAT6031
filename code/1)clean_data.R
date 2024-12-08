library(data.table)
library(jsonlite)
library(ggplot2)
library(dplyr)

#### GOALS OF FILE ######
# 1) Get rid of "bad" rows of data (which will screw up our model)
# 2) Get JSON data into a usable format 
#########################



# Read in data
# Make sure git repo is pulled onto your desktop, and that your working directory is in "STAT6031" (ie. the main folder)
raw_data <- fread("data/Listings_New_Orleans.csv")
bnb_data <- raw_data


### 1) Find bad data
# -----------------------------------------
# Remove listings with no price data
bnb_data <- bnb_data |> mutate(price = as.numeric(gsub("[$,]", "", as.character(price))))
bnb_data <- bnb_data[nchar(price)>1]  # (!!!) drops roughly 1000 records (!!!)

# Note: there are some seriously CRAZY prices - these are real listings but they make no sense
bnb_data[order(-price),.(id, name, beds, bathrooms, accommodates, price)]
# - $28,000 per night for a 


### 2) Add NEW variable - 
# Top 10 destination points from TripAdvisor
# This is meant to model how good the location of the BNB is....
# Represented as a count - so "6" means that the listing is within 1 km of 6 different attractions
# Probably want to model this as a log(count), since there are diminishing returns 1->2->3....->6
# -------------------------------------------
### Add top 10 destination points from TripAdvisor
dest_points <- data.table("Rank" = seq(1:10),
                          "Location" = c("WW2 Museum", "French Quarter", "Frenchman Street", "Garden District", "Jackson Square", "Preservation Hall", "Mardi Gras World", "St. Louis Cemetary", "New Orleans City Park", "St. Louis Cathedral"),
                          "Lat" = c(29.943208928864262, 29.959424483411674, 29.964318181205662, 29.928845784748955, 29.95756024289618, 29.958610251707462, 29.941144698995455, 29.959608239041174, 29.993454400417903, 29.95815187771942),
                          "Long" = c(-90.07057952646183, -90.06491814669677, -90.05773360204412, -90.08380077303872,-90.06294615548948, -90.06534875344381, -90.0621223634231, -90.07117833809907, -90.09813780389588, -90.06370009040566))


### Find everything within 1 km of each point
# Non equi-join (https://www.r-bloggers.com/2021/02/the-unequalled-joy-of-non-equi-joins/)

# Get locations within 1 km of key points (Latitude only)
near_id <- data.table()

for (i in 1:nrow(dest_points)) {
  setkey(bnb_data, latitude)
  setkey(dest_points, Lat)
  close_lat <- dest_points[i][bnb_data, roll = .01, nomatch = NULL]
  
  
  # Now filter to within 1km - longitude
  setkey(close_lat, longitude)
  setkey(dest_points, Long)
  close_lat_long <- dest_points[i][close_lat, roll = 0.01, nomatch = NULL][,.(id)]
  
  near_id <- rbind(near_id, close_lat_long)
  rm(close_lat)
  rm(close_lat_long)
}

### Count up how many points of interest are within 1 km of a given rental
near_id <- as.data.table(near_id)
near_id <- near_id[,near_top_10 := .N, by=id]

# Add this new derived variable back into the main dataset
bnb_data <- near_id[bnb_data,on="id"]
bnb_data <- bnb_data[is.na(near_top_10), near_top_10:=0] |> unique()


### 3) Parse Amenities JSON
# -------------------------------
# Fix up quotes, so we can parse this JSON
bnb_data[,amenities:=gsub('""','"' , amenities, fixed=TRUE)]
bnb_data[,amenities_list:=lapply(bnb_data$amenities, function(x) {parse_json(x)})]

# The parsed JSON now lives in a list object, which holds all the individual features for each property
# bnb_data$amenities_list[1:3]

# Collapse list items into a table, for easier review of what these look like...
all_amenities <- unlist(bnb_data[, amenities_list])  |> data.table()
all_amenities <- all_amenities[,.N,by=V1][order(-N)]


# over 2,000 possible items - we'll have to reduce this....

# 1) Can we combine similar entries?
# Coffee has 38 distinct entries...
# all_amenities[V1 %like% "Coffee"]

# 2) Ignore "sparse" features
# Of the 2000+ amenities, only 165 apply to at least 1% of the listings (ie. count of 70 or more)

# 3) Can we identify "premium" features, and disregard the rest?
# Pool
# Stainless steel appliances
# etc
# Which amenities are strongly associated with higher prices?

### Potential dummy variables for "significant" amenities
bnb_data[amenities_list %like% "Long term stays allowed",has_long_term:=1]
bnb_data[amenities_list %like% "Backyard",has_backyard:=1]
bnb_data[amenities_list %like% "Patio",has_patio:=1]
bnb_data[amenities_list %like% "Pool",has_pool:=1]
bnb_data[amenities_list %like% "Stainless",has_stainless:=1]
bnb_data[amenities_list %like% "Lake",has_lake:=1]
bnb_data[amenities_list %like% "Private",has_privacy:=1]
bnb_data[amenities_list %like% "Window guards",has_window_guards:=1]
bnb_data[amenities_list %like% "Indoor fireplace",has_indoor_fireplace:=1]

cols <- names(bnb_data)[names(bnb_data) %like% "has" & !names(bnb_data) %in% c("host_has_profile_pic", "has_availability")]
bnb_data[ , (cols) := lapply(.SD, nafill, fill=0), .SDcols = cols]


### Remove "unrealized" listings - these have anomalous prices AND zero availability
# Or, not available within the next 365 days.....
# These are likely to have never had an actual stay and therefore their pricing is not relevant to our model
high_low <- bnb_data[,quantile(price, probs=c(.025, 0.975))]
bnb_data <- bnb_data[!(((price>high_low[2] | price<high_low[1]) & availability_30 == 0) | availability_365 == 0)]

## Misc cleanup
rm(high_low)
rm(near_id)
rm(i)
