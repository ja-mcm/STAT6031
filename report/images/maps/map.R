library(osmdata)

### BE SURE TO RUN 
# - 1)clean_data.R to load a cleaned dataset
# - 2)variable_selection.R to reduce & conform columns

### Don't run this every time....no need to hammer the osm servers for the data
# Just run to generate maps for presentation

big_streets <- getbb("New Orleans")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()


med_streets <- getbb("New Orleans")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- getbb("New Orleans")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street","unclassified","service", "footway")) %>%
  osmdata_sf()



plot_NO <- ggplot() +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  theme_void() + # get rid of background color, grid lines, etc.
  theme(plot.title = element_text(size = 20, face="bold", hjust=.5),
        plot.subtitle = element_text(size = 8, hjust=.5, margin=margin(2, 0, 5, 0)))


### Spatial Analysis
# -------------------------------------------
# The more things you are close to, the better (may be non-linear - consider using log())

#### TOP Tripadvisor spots
plot_NO + 
  coord_sf(xlim = c(-90.14,-90.0), 
           ylim = c(29.9, 30.0),
           expand = FALSE)  +
  labs(title = "NEW ORLEANS", subtitle = "Listings within 1km of a Top 10 Attraction (Trip Advisor)") +
  geom_point(data=filtered_data,aes(x=longitude, y=latitude, colour = log(near_top_10)), cex=0.7) +
  theme(legend.position="none")


### Prices (middle 95th percentile)
filtered_data[,quantile(price, probs=c(.025, 0.975))]

plot_NO + 
  coord_sf(xlim = c(-90.14,-90.0), 
           ylim = c(29.9, 30.0),
           expand = FALSE)  +
  labs(title = "NEW ORLEANS", subtitle = "AirBNB Prices") +
  geom_point(data=filtered_data[price>48 & price<712],aes(x=longitude, y=latitude, colour = log(price), size=price), cex=0.3) +
  scale_colour_gradient(low = "grey80",high = "red") +
  theme(legend.position="none")




### PREDICTION ERROR
plot_NO + 
  coord_sf(xlim = c(-90.14,-90.0), 
           ylim = c(29.9, 30.0),
           expand = FALSE)  +
  labs(title = "NEW ORLEANS", subtitle = "Model Errors (TRAINING DATA)") +
  geom_point(data=data_orig_train,aes(x=longitude, y=latitude, colour = log(LASSO_ERROR), size=LASSO_ERROR), cex=0.6) +
  scale_colour_gradient2(low = "red",mid = "grey", high = "green") +
  theme(legend.position="none")

### PREDICTION ERROR
plot_NO + 
  coord_sf(xlim = c(-90.14,-90.0), 
           ylim = c(29.9, 30.0),
           expand = FALSE)  +
  labs(title = "NEW ORLEANS", subtitle = "Model Errors (TEST DATA)") +
  geom_point(data=data_orig_test,aes(x=longitude, y=latitude, colour = log(LASSO_ERROR), size=LASSO_ERROR), cex=0.6) +
  scale_colour_gradient2(low = "red",mid = "grey", high = "green") +
theme(legend.position="none")
