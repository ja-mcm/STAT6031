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
filtered_data <- bnb_data %>% 
  dplyr::select(host_since, host_location, host_listings_count, host_response_time,
                host_response_rate, host_acceptance_rate, host_is_superhost,
                host_total_listings_count, host_has_profile_pic, host_identity_verified,
                latitude, longitude, room_type, accommodates, bathrooms, beds, price, 
                #minimum nights, maximum nights, minimum nights avg ntm, maximum nights avg ntm,
                number_of_reviews, review_scores_rating, review_scores_accuracy,
                review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
                review_scores_location, review_scores_value,near_top_10)


# Replace empty values or N/A values by NA and count how many missing values per column
# Replace "N/A" with NA in the entire dataset
filtered_data <- filtered_data %>%
  mutate(across(c(host_response_time, host_response_rate, host_acceptance_rate), ~ na_if(., "N/A")))

# Replace empty with NA in the entire dataset
filtered_data <- filtered_data %>%
  # Apply `na_if()` to character columns only
  mutate(across(c(host_location, host_is_superhost, host_acceptance_rate), ~ na_if(., "")))

# Look at $ of NA per column
na_counts <- colSums(is.na(filtered_data))
na_counts

## Convert T/F to 1/0 --> true = 1, false = 0
filtered_data <- filtered_data %>%
  mutate(across(c(host_is_superhost, host_has_profile_pic, host_identity_verified),
                ~ if_else(as.character(.) == "t", 1, if_else(as.character(.) == "f", 0, NA_real_))))


##### I THINK WE CAN DROP THIS, SINCE LM FUNCTION WILL CREATE THE DUMMY VARS FOR US AUTOMAGICALLY
#### >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Code categorical variables with different levels as dummy variables
# Room Type
# table(filtered_data$room_type)
# 
# # Create dummy varaible to code room_type column
# dummies_roomtype <- model.matrix(~ room_type - 1, data = filtered_data)
# 
# # Convert the matrix to a data frame and add it to the original dataset
# filtered_data <- cbind(filtered_data, dummies_roomtype)
# 
# # Host Response Type
# table(filtered_data$host_response_time)
# 
# # Create dummy varaible to code host_response_time column
# filtered_data$host_response_time[is.na(filtered_data$host_response_time)] <- "unknown"
# 
# # Create dummy variables including the "unknown" category
# dummies_responsetime <- model.matrix(~ host_response_time - 1, data = filtered_data)
# 
# # Add the dummy variables back to the data
# filtered_data <- cbind(filtered_data, dummies_responsetime)
# 
# colnames(filtered_data) <- gsub(" ", "_", colnames(filtered_data))
# colnames(filtered_data) <- gsub("/", "_", colnames(filtered_data))
# 
# filtered_data <- filtered_data %>%
#   rename(entire_unit = room_typeEntire_home_apt,
#          hotel = room_typeHotel_room,
#          private_room = room_typePrivate_room,
#          shared_room = room_typeShared_room)
# 
# filtered_data <- filtered_data %>%
#   rename(resp_time_days = host_response_timea_few_days_or_more,
#          resp_time_unknown = host_response_timeunknown,
#          resp_time_day = host_response_timewithin_a_day,
#          resp_time_hours = host_response_timewithin_a_few_hours,
#          resp_time_one_hour = host_response_timewithin_an_hour)


# Modify host_since column to host for x years, easier to model
filtered_data <- filtered_data %>%
  mutate(
    # Remove "host since " and extract the year part
    host_since_year = gsub("host since ", "", host_since),
    host_since_year = as.Date(host_since_year),            # Convert to Date
    host_years = ifelse(!is.na(host_since_year),            # Check if the date is valid
                        as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(host_since_year, "%Y")),
                        NA)                               # If not valid, set as NA
  )

# Create final dataset
final_data <- filtered_data %>%
  select(-host_since, -room_type, -host_since_year, -host_response_time)

