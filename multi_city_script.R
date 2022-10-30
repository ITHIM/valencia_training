# Clean memory/environment
rm(list=ls())

# Load required libraries
library(ithimr)
library(tidyverse)
library(readxl)
library(truncnorm)

# Load helpful libraries for EDA
# library(plyr)
# library(janitor)
# library(esquisse)

# Install drpa is not already installed
if (!require("drpa",character.only = TRUE)) {
  print('Installing "drpa" package...')
  remotes::install_github("meta-analyses/drpa")
  library(drpa)
  print("")
}

# Specify list of cities. We are only using SP for now.
cities <<- c('sao_paulo')

# Specify parameters file for SP specific and global vars
input_parameter_file <<- "InputParameters_v18.0.xlsx"

# Read in all params through this script
source("helpful_scripts/load_input_params.R")

# Load the required data, and run the model
print(system.time(for(city in cities){
  city <- c('sao_paulo')
  print(city)
  ithim_objects[[city]] <- run_ithim_setup(
    DIST_CAT = as.character(dist_cat),
    ADD_WALK_TO_PT_TRIPS = as.logical(add_walk_to_pt_trips[[city]]),
    CITY = city,
    AGE_RANGE = c(min_age,max_age),
    ADD_TRUCK_DRIVERS = as.logical(add_truck_drivers),
    ADD_BUS_DRIVERS = as.logical(add_bus_drivers),
    ADD_CAR_DRIVERS = as.logical(add_car_drivers),
    ADD_MOTORCYCLE_FLEET = as.logical(add_motorcycle_fleet[[city]]),
    ADD_PERSONAL_MOTORCYCLE_TRIPS = as.character(add_personal_motorcycle_trips[[city]]),
    PM_emission_inventory = PM_emission_inventories[[city]],
    CO2_emission_inventory = CO2_emission_inventories[[city]],
    speeds = speeds[[city]],
    
    FLEET_TO_MOTORCYCLE_RATIO = fleet_to_motorcycle_ratio[[city]],
    PROPORTION_MOTORCYCLE_TRIPS = proportion_motorcycle_trips[[city]],
    MMET_CYCLING = mmet_cycling, 
    MMET_WALKING = mmet_walking, 
    DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
    SIN_EXPONENT_SUM = sin_exponent_sum,
    CASUALTY_EXPONENT_FRACTION = casualty_exponent_fraction,
    SIN_EXPONENT_SUM_NOV = sin_exponent_sum_nov,
    SIN_EXPONENT_SUM_CYCLE = sin_exponent_sum_cycle,
    CASUALTY_EXPONENT_FRACTION_CYCLE = casualty_exponent_fraction_cycle,
    SIN_EXPONENT_SUM_PED = sin_exponent_sum_ped,
    CASUALTY_EXPONENT_FRACTION_PED = casualty_exponent_fraction_ped,
    SIN_EXPONENT_SUM_VEH = sin_exponent_sum_veh,
    CASUALTY_EXPONENT_FRACTION_VEH = casualty_exponent_fraction_veh,
    CALL_INDIVIDUAL_SIN = as.logical(call_individual_sin),
    PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile,  
    AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile,
    INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
    CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
    PM_CONC_BASE = pm_conc_base[[city]],  
    PM_TRANS_SHARE = pm_trans_share[[city]],  
    BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
    BUS_WALK_TIME = bus_walk_time[[city]],
    RAIL_WALK_TIME = rail_walk_time[[city]],
    
    BUS_TO_PASSENGER_RATIO = bus_to_passenger_ratio[[city]],
    TRUCK_TO_CAR_RATIO = truck_to_car_ratio[[city]],
    CAR_OCCUPANCY_RATIO = car_occupancy_ratio[[city]],
    SCENARIO_NAME = scenario_name
  )
  
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$synth_pop <- SYNTHETIC_POPULATION
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_object=ithim_objects[[city]], seed = 1)
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$PM_emission_inventory <- PM_EMISSION_INVENTORY
  ithim_objects[[city]]$injury_table <- INJURY_TABLE
  ithim_objects[[city]]$vehicle_inventory <- VEHICLE_INVENTORY
  ithim_objects[[city]]$location$country <- country[[city]]
  ithim_objects[[city]]$location$continent <- continent[[city]]
  
}))

# Save the io object in the results folder
saveRDS(ithim_objects, "results/multi_city/io.rds", version = 2)
