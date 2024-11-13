library(data.table)
library(ggplot2)
library(reshape2)

final_data <- final_data %>%
  dplyr::select(-id, -maximum_nights_avg_ntm)

#### GOALS OF FILE ######
# 1) Explore relationships between variables
# 2) Provide diagnostics necessary to build appropriate model
#########################

### BE SURE TO RUN 
# - 1)clean_data.R to load a cleaned dataset
# - 2)variable_selection.R to reduce & conform columns

### Explore Price
# -------------------------------------------
# Inspect distribution of prices
# Very right-skewed
ggplot(final_data,aes(price)) + geom_histogram() + labs(title = "Distribution of AirBNB Prices - New Orleans")

# Log transform isn't perfect either...
ggplot(final_data,aes(log(price))) + geom_histogram() + labs(title = "Distribution of log(AirBNB Prices) - New Orleans")

# Be on the lookout for anomalous entries
# We'll need to identify these quickly so that our price prediction accuracy doesn't get tanked.

### Correlation Heat Map Attempt
set.seed(123)

numeric_data <- final_data %>%
  dplyr::select(-neighbourhood_cleansed, -room_type, -amenities_list,
                -host_response_time, -id, -calculated_host_listings_count_entire_homes,
                -calculated_host_listings_count_private_rooms, 
                -calculated_host_listings_count_shared_rooms,
                -calculated_host_listings_count)

numeric_data <- as_tibble(numeric_data)

correlation_matrix <- cor(numeric_data)

correlation_matrix_dt <- as.data.table(as.table(correlation_matrix))

# Melt the correlation matrix for ggplot2
cor_melted <- melt(correlation_matrix_dt, na.rm = TRUE)

# Create the heatmap
ggplot(cor_melted, aes(V1, V2, fill = value)) +
  geom_tile(color = "white") +  # Add white borders to the squares
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +  # Gradient fill for correlation values
  geom_text(aes(label = round(value, 2)), color = "black") +  # Add correlation values in squares
  theme_minimal() +  # Use a minimal theme
  labs(title = "Correlation Matrix Heatmap", 
       x = "", 
       y = "") +  # Remove axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis labels


## Report % of binary variables
report_binary_percentages <- function(data) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop through each column in the dataset
  for (col in names(data)) {
    # Check if the column is binary (contains only 0s, 1s, or NA values)
    unique_values <- unique(na.omit(data[[col]]))
    if (all(unique_values %in% c(0, 1))) {
      # Calculate the percentages of 0s and 1s, ignoring NAs
      percent_1 <- mean(data[[col]] == 1, na.rm = TRUE) * 100
      percent_0 <- mean(data[[col]] == 0, na.rm = TRUE) * 100
      
      # Store the results in a list
      results[[col]] <- c(`% of 1s` = percent_1, `% of 0s` = percent_0)
    }
  }
  
  # Convert the list to a data frame for easier viewing
  results_df <- as.data.frame(do.call(rbind, results))
  return(results_df)
}

report_binary_percentages(final_data)

## Look at density plots and boxplots of non-binary variables

create_plots <- function(data) {
  # Loop through each column in the dataset
  for (col in names(data)) {
    # Check if the column is numeric and not binary (contains values other than 0 and 1)
    unique_values <- unique(na.omit(data[[col]]))
    if (is.numeric(data[[col]]) && length(unique_values) > 2) {
      # Create boxplot
      boxplot(data[[col]], main = paste("Boxplot"), xlab = col, col = "lightblue")
      
      # Create density plot
      plot(density(data[[col]], na.rm = TRUE), main = paste("Density Plot"), xlab = col, col = "lightgreen")
    } else {
      message(paste("Skipping binary or non-numeric column:", col))
    }
  }
}

par(mfrow = c(1,2))
create_plots(final_data)

## Report summary statistics
summary_stats <- function(data) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop through each column in the dataset
  for (col in names(data)) {
    # Check if the column is numeric and not binary
    unique_values <- unique(na.omit(data[[col]]))
    if (is.numeric(data[[col]]) && length(unique_values) > 2) {
      # Calculate summary statistics
      summary_stats <- c(
        Mean = round(mean(data[[col]], na.rm = TRUE), 3),
        Median = round(median(data[[col]], na.rm = TRUE), 3),
        SD = round(sd(data[[col]], na.rm = TRUE), 3),
        Min = round(min(data[[col]], na.rm = TRUE), 3),
        Max = round(max(data[[col]], na.rm = TRUE), 3),
        NAs = sum(is.na(data[[col]]))
      )
      
      # Store the results in a list
      results[[col]] <- summary_stats
    } else {
      message(paste("Skipping binary or non-numeric column:", col))
    }
  }
  
  # Convert the list to a data frame for easier viewing
  results_df <- as.data.frame(do.call(rbind, results))
  return(results_df)
}

nonbinary_summary <- summary_stats(final_data)
print(nonbinary_summary)
