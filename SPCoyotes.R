##### Final analyses and data visualization for Stanley Park coyote attack project

#### Script Info/Instructions -----

{
  # Start-up Instructions
  # 1. Pull from github first
  # 2. Read libraries second
  # 3. Read all datasets third
  
  # Script Format
  ##### Script Description
  #### Script Instructions/Details
  ### Section Header
  ## Description of single code section
  # Description of single line
}

### Read libraries -----

{
library(tidyverse) #for tidyverse coding format
#library(visreg) #for visualization of model fit
library(MuMIn) #for dredge()
#library(AICcmodavg) #for aictab
#library(car) #for Anova
#library(Hmisc) # correlation coefficients
#library(vcd) #for Cramer's V test
#library(scales) #for breaks on plot axes
#library(leaflet) #for maps
#library(logistf) #for Firth's Bias-Reduced Logistic Regression
#library(glmnet) #for ridge and lasso regression
#library(adehabitatHR) #for UD analysis
#library(viridis) #for colours
#library(circular) #for Watson's U2 Test
#library(rempsyc) #for table formatting
#library(MASS) #for ordered logistic regression
#library(ggpubr) #for multi-panel figures
}

### Load data -----

{
## Full coyote dataset (used for plots)
df_encounters_full <- read_csv("Data/sp_coyote_project_dataset_no_accuracy_cutoff.csv") %>%
  filter(encounter == "Sighting" | encounter == "Attack" | encounter == "Aggression to Human")

## Data subset with analysis variables only (used for analyses with no accuracy cutoff)
df_encounters <- df_encounters_full %>%
  #keep only variables to be included in analyses
  dplyr::select(encounter_ID, date, weekday, encounter, encounter_binary, coyseason, Lockdown_Phase,
                prop_open_100_scaled, prop_open_150_scaled, prop_open_200_scaled, prop_open_250_scaled,
                prop_natural_cover_100_scaled, prop_natural_cover_150_scaled, prop_natural_cover_200_scaled, prop_natural_cover_250_scaled,
                prop_developed_100_scaled, prop_developed_150_scaled, prop_developed_200_scaled, prop_developed_250_scaled,
                garbage_scaled, picnic_scaled, distance2water_scaled, distance2ocean_scaled, d2den_scaled,
                precip_scaled, avg_temp_scaled, min_temp_scaled, max_temp_scaled, time_cos_scaled, lon, lat) %>%
  #remove any variables from outside study timeline
  filter(!is.na(Lockdown_Phase)) %>%
  #fix binary column so attacks and aggression are both 1 and sightings are 0
  mutate(encounter_binary = ifelse(encounter_binary == 0, 0, 1)) %>%
  #remove all data points with NAs
  na.omit()

## Human activity data summarized by sample block
df_human_activity_blocks <- read_csv("Data/human_activity_blocks.csv")

## Activity data for each individual person recorded
df_individual_human_activity <- read_csv("Data/individual_human_activity.csv")
}
