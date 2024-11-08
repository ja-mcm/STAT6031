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



### Spatial Analysis
# -------------------------------------------
# The more things you are close to, the better (may be non-linear - consider using log())
ggplot(final_data,aes(longitude, latitude, colour = log(near_top_10))) + 
  geom_point(cex = 0.3) +
  coord_cartesian(xlim=c(-90.14,-90.0), ylim = c(29.9, 30.0)) + theme_void() + 
  labs(title = "  New Orleans - Popular Destinations", subtitle = "") + 
  theme(legend.position="none", plot.title = element_text(size=18))


ggplot(final_data[beds < 5 & price < 2000],aes(longitude, latitude, colour = log(price), size=price)) + 
  geom_point(cex = 0.3) +
  coord_cartesian(xlim=c(-90.14,-90.03), ylim = c(29.9, 30.0)) + theme_void() + 
  scale_colour_gradient(low = "grey80",high = "blue") +
  labs(title = "  New Orleans - AirBnB Prices", subtitle = "") +
  theme(plot.title = element_text(size=18))

