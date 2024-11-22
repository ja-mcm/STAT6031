# STAT6031

# Project Goal:
Accurately predict the price of a given AirBNB rental in __NEW ORLEANS__, given the metadata about each property.

# Some stats for presentation:
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


# Our Model
TBD

[Caret](https://towardsdatascience.com/create-predictive-models-in-r-with-caret-12baf9941236) is a great general purpose regression/machine learning library in R.

- Train/test splitting
- Cross validation
- Pre-processing

### Possible supplemental data:
  - Lat-long of top 10 TripAdvisor destinations in the area ("# within walking distance") --- DONE
  - Crime rates ("Do I really want to stay in this area?")
  - Local rental rates ("How expensive is an equivalent apartment in that area?")'

# TODO 
1) ~~Clean out duplicates/bad data (JACK)~~
2) ~~Decide which variables to keep/model (ADRIANA)~~
3) ~~EDA (ADRIANA)~~
4) Model selection (JACK)
5) Validation (ADRIANA)
6) Report writing (ADRIANA)
7) Presentation
