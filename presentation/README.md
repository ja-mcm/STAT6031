# Presentation Outline

# Slide 1: TITLE

# Slide 2: OUR CITY: New Orleans
### Visuals:
- Map (orange dots)
- Map (TripAdvisor)?


### Some stats for presentation:
- New Orleans population
  - City: 400,000
  - Metro: 1.2M
  - Compare to Cincinnati (300,000, 2.2M)
- 17 MILLION people visited New Orleans in 2023 [source](https://bizneworleans.com/tourism-is-economic-development/)
  - Only 100,000 hotel rooms [in the whole state of Louisiana](https://www.explorelouisiana.com/sites/default/files/2021-02/2020StateOfTourism-October%20Monthly.pdf)
  - 1 Million visitors from cruise ships
  - Where are people going to stay? In an AirBNB! 
      - over 7,000 listings on AirBnb
      - Per [US Census](https://www.census.gov/quickfacts/fact/table/neworleanscitylouisiana/PST045223), city has ~155,000 "households" (we'll assume each household = 1 house)
- Crime is rampant in New Orleans
  - Highest murder rate per capita in US (4x higher than Chicago)
  - F rating in Crime Stat


# Slide 3: OUR DATA
### Visuals: 
- TBD

### Discuss
- Removed invalid entries
  - No price (-1149 rows)
  - Not an open listing (-48 rows)
  - No reviews ()
  - Went from __7,118__ rows of data to __4,248__ rows

- REMOVED unusable columns
  - Ex 1
  - Ex 2
  - ....
  - Went from __74__ columns down to __39__ columns


- ADDED/SUPPLEMENTED
  - Amenities (pool, stainless, etc)
  - TripAdvisor 
  - Added __9__ new predictors
  - In total, our final model had __45 predictor variables__


# Slide 4: OUR MODEL
### Visuals:
- Residual plots?

### Discuss
- Transformations:
  - Price
- Interactions:
  - Review score x reviews (more good reviews may have non-linear impact)
- Models we considered:
  - LM: our baseline
  - Robust LM: given the huge outliers
  - Ridge:
  - Lasso:


# Slide 5: OUR RESULTS
### Visuals:
- RMSE (Prediction) vs Actual

### Discuss
- Best model - why was it best?
- Important variables


# Slide 6: NEXT STEPS
### Visuals:
- None?

### Discuss
- What we learned
  - Beware of messy data (large impact on prediction accuracy)
  - 
- Other potential variables - CRIME, transit, local housing prices.....
- 