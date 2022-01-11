#-------------------------------------------------------------------------------
# POVERTY VARIABLE SELECTION VALIDATION
#-------------------------------------------------------------------------------

# Get libraries
library(dplyr)
library(foreign)
library(haven)
library(tidyverse)
library(randomForest)
library(here)


# Read data
# ---------

## Asset ownership data
assets <- read.dta(file.path("data", "bangladesh_hies_2016", "HH_SEC_9E.dta"), convert.factors = TRUE)

## Poverty household status data
poverty <- read.dta(file.path("data", "bangladesh_hies_2016", "poverty_indicators2016.dta"))


# Prepare data
# ------------

assets_df <- assets %>% 
  mutate(assets = s9eq00,
         yes = case_when(
           s9eq01b == "X" ~ 1,
           s9eq01b == "" ~ 0)) %>% 
  select(hhid, assets, yes) %>% 
  filter(!is.na(assets),
         assets != "Total") %>% 
  pivot_wider(
    names_from = assets,
    values_from = yes,
  ) %>% 
  # clean up asset names
  rename(radio = Radio,
         washing_machine = `Washing machine`,
         dining_room_furniture = `Dining room Furniture`,
         two_in_one_cassette_player = `Two-in-one, Cassette player`,
         motor_car = `Motor car etc.`,
         refrigerator = `Refrigerator or freezer`,
         drawing_room_furniture = `Drawing room Furniture`,
         dish = `Dish antena/ decoder`,
         watch_clock = `Wrist watch/Wall clock`,
         sewing_machine = `Sewing machine`,
         fans = Fans,
         boat = `Boat/Others`,
         computer = `Computer/TV Card`,
         kitchen_crockery = `Kitchen Items - Crockery`,
         tubewell = `Tubewell (for drinking water only)`,
         bedroom_furniture = `Bedroom Furniture`,
         television = `Television`,
         heaters = `Heaters`,
         carpet = `Carpet`,
         dvd = `VCR/ VCP/DVD`,
         kitchen_microwaveoven = `Mocrooven/Kitchen Items - Cooking`,
         camera = `Camera/ camcorder`,
         mobile = `Mobile`,
         pressurelamps_petromax = `Pressure lamps/ petromax`,
         motorcycle = `Motorcycle/ scooter`,
         kitchen_cutlery = `Kitchen Items - Cutlery`,
         bicycle = `Bicycle`) 
  
poverty_df <- poverty %>% 
  select(psu, hhid, quarter, hhwgt, stratum, stratum16, 
         ruc, urbrural, division_code, division_name, 
         zila_code, zila_name, upperpoor, lowerpoor)

hh_df <- inner_join(poverty_df, assets_df, by = "hhid")


# Separate data into training and validation sets
# -------------------------------------------------

# make things reproducible
set.seed(11516)

# Calculate the size of each of the data sets
size <- floor(nrow(hh_df)/2)

# Generate a random sample of half the households
indexes <- sample(1:nrow(hh_df), size = size)

# Assign the data to the correct sets
training <- hh_df[indexes,]
validation <- hh_df[-indexes,]


# Assign assets as covariates for variable selection
covars <- colnames(hh_df)[15:42]


rf_classifier = randomForest(upperpoor ~ covars, data=training, ntree=100, mtry=2, importance=TRUE)







