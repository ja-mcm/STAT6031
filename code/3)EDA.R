library(data.table)
library(ggplot2)
library(reshape2)

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

#pairs(x ~ ., data = data, main = "Scatterplot Matrix")

# Correlation heat map
set.seed(123)

numeric_data <- final_data %>%
  dplyr::select(-neighbourhood_cleansed, -room_type, -amenities_list, -host_response_time, -id)

numeric_data <- as_tibble(numeric_data)

numeric_data <- as.data.table(numeric_data)

correlation_matrix <- cor(numeric_data)

correlation_matrix_dt <- as.data.table(as.table(correlation_matrix))

# Melt the data.table for ggplot2
cor_melted <- melt(correlation_matrix_dt, id.vars = NULL)
print(cor_melted)

# Melt the correlation matrix for ggplot2
cor_melted <- melt(correlation_matrix)

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

