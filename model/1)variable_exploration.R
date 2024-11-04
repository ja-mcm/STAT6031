library(data.table)
library(jsonlite)
library(rlist)

bnb_data <- fread("data/Listings_New_Orleans.csv")

### Fix up quotes, so we can parse this JSON
bnb_data[,amenities:=gsub('""','"' , amenities, fixed=TRUE)]
bnb_data[,amenities_list:=lapply(bnb_data$amenities, function(x) {parse_json(x)})]

# this lives in a list object, which holds all the features for each individual property
bnb_data$amenities_list[1:3]
bnb_data[amenities_list %like% "Wifi",.N] # 6609 of 7118 have Wifi listed

# Amenities appear to be from a drop-down
# (while there are multiple variations on this item, none of them are misspelled)
# <SLOW RUNNING CODE>
# bnb_data[amenities_list %like% "Coffee",amenities_list] |> rlist::list.search(, expr = grepl("Coffee",.))
