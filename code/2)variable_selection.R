library(data.table)
library(ggplot2)
library(dplyr)

#### GOALS OF FILE ######
# 1) Remove all columns that are clearly not valid predictors of PRICE
# 2) Conform data to format usable for a regression 
#########################

### BE SURE TO RUN 
# - 1)clean_data.R to load a cleaned dataset

# Select useful variables

#colnames(bnb_data)
filtered_data <- bnb_data %>% 
  dplyr::select(-listing_url, -scrape_id, -last_scraped, -source, -name, -description,
                -picture_url, -host_id, -host_url, -host_name, -host_about, 
                -host_thumbnail_url, -host_picture_url, -bathrooms_text, -minimum_nights,
                - maximum_nights, -minimum_minimum_nights, -maximum_minimum_nights,
                - minimum_maximum_nights, -maximum_maximum_nights, -calendar_updated,
                - has_availability, -calendar_last_scraped, -first_review, -license,
                -neighborhood_overview, -host_verifications, -neighbourhood, 
                -neighbourhood_group_cleansed, -property_type, -amenities,
                -host_listings_count, - host_neighbourhood, -last_review)

# Do we keep host_neighborhood?

# Replace "N/A" and empty with NA in the entire dataset
filtered_data <- filtered_data %>%
  mutate(across(where(is.character), ~ na_if(., "N/A"))) %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# Look at $ of NA per column
na_counts <- colSums(is.na(filtered_data))

# Here, review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
# review_scores_checkin, review_scores_communication, review_scores_location,                          review_scores_value 
# review_scores_value and reviews_per_month all have 879 missing values, look at
# whether they happen in the exact same rows

columns_of_interest <- c(
  "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness",
  "review_scores_checkin", "review_scores_communication", "review_scores_location",
  "review_scores_value", "reviews_per_month")

# All NAs happen in the exact same rows. Remove them?

## Convert T/F to 1/0 --> true = 1, false = 0
filtered_data <- filtered_data %>%
  mutate(across(c(host_is_superhost, host_has_profile_pic, host_identity_verified, instant_bookable),
                ~ if_else(as.character(.) == "t", 1, if_else(as.character(.) == "f", 0, NA_real_))))

# Modify host_since column to host for x years, easier to model
filtered_data <- filtered_data %>%
  mutate(
    # Remove "host since " and extract the year part
    host_since_year = gsub("host since ", "", host_since),
    host_since_year = as.Date(host_since_year),            # Convert to Date
    host_years = ifelse(!is.na(host_since_year),            # Check if the date is valid
                        as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(host_since_year, "%Y")),
                        NA)) %>%
  dplyr::select(-host_since, -host_since_year)

# Code host_location column as 1 if New Orleans, 0 otherwise
filtered_data <- filtered_data %>%
  mutate(host_location = if_else(host_location == "New Orleans, LA", 1, 0))

final_data <- filtered_data


# We could probably remove the price outliers with no reviews since they are probably fake
rm(na_counts)
rm(columns_of_interest)

# Remove % symbol and convert to numeric
final_data$host_response_rate <- as.numeric(gsub("%", "", final_data$host_response_rate))
final_data$host_response_rate <- as.numeric(final_data$host_response_rate)

final_data$host_acceptance_rate <- as.numeric(gsub("%", "", final_data$host_acceptance_rate))
final_data$host_acceptance_rate <- as.numeric(final_data$host_acceptance_rate)


## Remove calculated fields - these are duplicative
cols <- names(final_data)[names(final_data) %like% "calculated"]
final_data[ , (cols) := NULL]


### Do we want to:
# 1) Standardize
# 2) Center
# 3) Remove NAs/impute NAs?