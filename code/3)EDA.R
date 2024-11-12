library(data.table)
library(ggplot2)


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

# Be on the lookout for anomalous entries (we removed the ones @ $999)
# We'll need to identify these quickly so that our price prediction accuracy doesn't get tanked.



