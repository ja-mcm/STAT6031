library(data.table)
library(jsonlite)
library(ggplot2)

# Read in sample data
bnb_data <- fread("data/Listings_New_Orleans.csv")


### 1) Parse Amenities JSON
# -------------------------------
# Fix up quotes, so we can parse this JSON
bnb_data[,amenities:=gsub('""','"' , amenities, fixed=TRUE)]
bnb_data[,amenities_list:=lapply(bnb_data$amenities, function(x) {parse_json(x)})]

# The parsed JSON now lives in a list object, which holds all the individual features for each property
bnb_data$amenities_list[1:3]

# Collapse list items into a table, for easier review of what these look like...
all_amenities <- unlist(bnb_data[, amenities_list])  |> data.table()
all_amenities <- all_amenities[,.N,by=V1][order(-N)]


# over 2,000 possible items - we'll have to reduce this....

# 1) Can we combine similar entries?
# Coffee has 38 distinct entries...
all_amenities[V1 %like% "Coffee"]

# 2) Ignore "sparse" features
# Of the 2000+ amenities, only 165 apply to at least 1% of the listings (ie. count of 70 or more)

# 3) Can we identify "premium" features, and disregard the rest?
  # Pool
  # Stainless steel appliances
  # etc
# Which amenities are strongly associated with higher prices?




### 2) Find bad data
# -----------------------------------------
# Remove listings with no price data
bnb_data[,price:=as.numeric(substring(price,2))] ### Note: drops decimal places, but prices never have cent component
bnb_data <- bnb_data[nchar(price)>1]  # (!!!) drops roughly 1000 records (!!!)


### Find potential duplicates
# based on repeated lat/long (497)
potential_dupes <- bnb_data[duplicated(bnb_data, by = c("latitude", "longitude")) == "TRUE"][order(latitude,longitude)]

# based on repeated descriptions (199)
potential_dupes2 <- bnb_data[duplicated(bnb_data, by = c("name")) == "TRUE"][order(latitude,longitude)]



### 3) Explore
# -------------------------------------------
# Inspect distribution of prices
# Very right-skewed
ggplot(bnb_data,aes(price)) + geom_histogram()

# Log transform isnt perfect either...
ggplot(bnb_data,aes(log(price))) + geom_histogram()


# Note the anomalous entries @ $999 and $500 price points. What's up with these?
# We'll need to identify why so that our price prediction accuracy doesn't get tanked.




### 4) Spatial Analysis
# -------------------------------------------
### Add 3 sample destination points from TripAdvisor
dest_points <- data.table("Rank" = seq(1:3),
                          "Location" = c("WW2 Museum", "French Quarter", "Frenchman Street"),
                          "Lat" = c(29.943208928864262, 29.959424483411674, 29.964318181205662),
                          "Long" = c(-90.07057952646183, -90.06491814669677, -90.05773360204412))


### Find everything within 1 km of Point #1
# Non equi-join (https://www.r-bloggers.com/2021/02/the-unequalled-joy-of-non-equi-joins/)

# Get locations within 1 km of key points (Latitude only)
setkey(bnb_data, latitude)
setkey(dest_points, Lat)
close_lat <- dest_points[bnb_data, roll = 0.01][!is.na(Location)]

# Now filter to within 1km - longitude
setkey(close_lat, longitude)
setkey(dest_points, Long)
close_lat_long <- dest_points[close_lat, roll = 0.01][!is.na(Location)][,.(id, 1)]

# Add this new derived variable back into the main dataset
bnb_data <- close_lat_long[bnb_data,on="id"]
bnb_data[is.na(V2), V2:=0]

setnames(bnb_data, "V2", "near_attraction")

### TODO: Count up how many points of interest are within 1 km of a given rental
# Currently, this is a binary

# close_pts <- dest_points[1][bnb_data, 
#             .(id, Location ,Lat, Long, latitude, longitude, MAX_LON, MIN_LON),
#             on=.(Lat>=MIN_LAT, Lat<=MAX_LAT, Long>=MAX_LON), nomatch=0L]




### FUTURE: Remove unusable columns
# -------------------------------------------
ncol(bnb_data) 
## 76 columns

# Drop all URLs - we don't plan on using these....
bnb_data[,grep("^.*url$", colnames(bnb_data)):= NULL]

# Drop all the redundant max/min nights variables - keep only the average
bnb_data[,grep("^.*nights$", colnames(bnb_data)):= NULL]

# Drop columns with only NAs
bnb_data <- bnb_data[, .SD, .SDcols = \(x) !all(is.na(x))]

# Drop any invariant columns (ie. only 1 value)
bnb_data <- bnb_data[, !sapply(bnb_data, FUN = function(x) length(unique(x))==1), with = FALSE]

# Drop other misc columns that have no clear use
bnb_data[,c("host_verifications", "neighbourhood"):=NULL]


ncol(bnb_data)
## 58 columns
