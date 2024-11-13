# Modeling

### Notes
- We agreed an interaction variable between # of reviews and review score made sense (ie. more good scores counts for more than only a few good scores...)
- Variables that might need log-transforms:
     - Price
     - Near_Top_10
     - Host_Listings
     - basically, anything we suspect has "diminishing returns" as X increases....


# Variable Selection

#### Notes
- We dropped highly correlated columns related to host listings. See Correlation Plot
- There were also a variety of columns that were clearly not valuable (ie. host_verification_method)
- Additionally, text descriptions were omitted, given their limited information content 
  - details about the listing are readily available in the Amenities, which we did make use of


# Data Wrangling

### Research Notes
- __Lat-long data__ will likely be critically important to deriving rental rates. 
  - These lat-longs have a minimum of 5 decimal places, which gives us a [spatial resolution of ~3ft.](https://en.wikipedia.org/wiki/Decimal_degrees) 
  - From the [Data Assumptions](https://insideairbnb.com/data-assumptions/) FAQ: 
    - _"Location information for listings are anonymized by Airbnb. In practice, this means the location for a listing on the map, or in the data will be from __0-450 feet (150 metres) of the actual address__."_

- __Amenities__ is a JSON array. 
  - These might provide features for predicting the Quality of a lodging (such as: "pool", "stainless steel appliances", etc)
  - There are multiple variations on many items ("Coffee maker" vs "Coffee maker: drip coffee maker" vs "Coffee maker: Keurig coffee machine"), so we'll have to be careful...
  - But, this appears to be a drop-down/list selection - none of the items are misspelled, which would imply user manual input - this is good news!

- __Pricing__ data has some serious quirks:
  - ~1000 records have no pricing - these were removed
  - There are some extreme outliers, on both ends
    - $28,000 / night for a whole hotel
    - $10,000 / night for a 1 bd
    - $20 / night for a room in a Dorm
    - $21 / night for a room in a hostel
    - etc
- We looked for duplicates based on:
    - Name  
    - Lat/Long 
  - We determined they were not "bad" data - they are real listings, and not the result of a bad JOIN or something else
  - Therefore, we will NOT be removing

--> 
  __TODO__ - Figure out how we can mitigate extreme PRICE values
  
    - OPTION 1: Find them given the variables we have (such as minimum nights) 
    - OPTION 2: discuss options for robust regression, to better handle these outliers
    - OPTION 3: remove (not a big fan of this, since they are true data points, and we may need to predict an outlier)
