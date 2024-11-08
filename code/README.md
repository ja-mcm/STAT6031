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
- __Pricing__ data has some quirks:
  - ~1000 records have no pricing - these were removed
  - There are some suspicious-looking clusters ($999 for a 1 BR?) - what do we do with these?
- Duplicates also exist
  - These might be multiple listings in an apartment complex