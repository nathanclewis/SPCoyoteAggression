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

### Read libraries and settings -----

{
library(tidyverse) #for tidyverse coding format
library(visreg) #for visualization of model fit
library(MuMIn) #for dredge()
library(car) #for Anova. and VIF
library(leaflet) #for maps
library(ggpubr) #for multi-panel figures
library(caret) #for cross-validation
  
options(scipen=10000) #no scientific notation on plots
}

### Load data -----

{
## Full coyote dataset (used for plots)
df_encounters_full <- read_csv("Data/sp_coyote_project_dataset.csv") %>%
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
  mutate(encounter_binary = ifelse(encounter_binary == 0, 0, 1),
         #add factor column for cv
         encounter_binary_fc = case_when(encounter_binary == 0 ~ "sighting",
                                         encounter_binary == 1 ~ "encounter",
                                         TRUE ~ NA_character_)) %>%
  #remove all data points with NAs
  na.omit()

## Human activity data summarized by sample block
df_human_activity_blocks <- read_csv("Data/human_activity_blocks.csv")

## Activity data for each individual person recorded
df_individual_human_activity <- read_csv("Data/individual_human_activity.csv")
}

### Map data -----

{
## Define colour palette
color_pal <- colorFactor("RdYlBu", domain = NULL)

## Map reports by interaction type
df_encounters_full %>%
  #load the map package
  leaflet() %>%
  #add the terrain
  addTiles() %>%
  #add circle markers at the site of each report
  addCircleMarkers(~lon, ~lat, radius = 10, fillColor = ~color_pal(encounter),
                   fillOpacity = 1, popup = ~encounter)
}
### Test assumptions of trends in human activity -----

## Create model with all predictors
#Model equation
human_all_variables_model <- lm(total_humans_adjusted ~ weather + site_category + time_from_peak_scaled + week_time,
                                #run with blocks dataset
                                data = df_human_activity_blocks)

## Evaluate model fit and variable estimates

#Generate summary table
summary(human_all_variables_model)
#Generate ANOVA table
Anova(human_all_variables_model)
#Visualize model fit for individual variables
visreg(human_all_variables_model, scale = "response")

### Test land cover radii with model selection -----

{
## 100m buffer model

#Model equation
conflict_100m_model <- glm(encounter_binary ~ prop_natural_cover_100_scaled + prop_open_100_scaled + prop_developed_100_scaled,
                           #Run as logistic regression
                           family = binomial(link = "logit"),
                           #Run with reduced dataset
                           data = df_encounters)

## 150m buffer model

#Model equation
conflict_150m_model <- glm(encounter_binary ~ prop_natural_cover_150_scaled + prop_open_150_scaled + prop_developed_150_scaled,
                           #Run as logistic regression
                           family = binomial(link = "logit"),
                           #Run with reduced dataset
                           data = df_encounters)

## 200m buffer model

#Model equation
conflict_200m_model <- glm(encounter_binary ~ prop_natural_cover_200_scaled + prop_open_200_scaled + prop_developed_200_scaled,
                           #Run as logistic regression
                           family = binomial(link = "logit"),
                           #Run with reduced dataset
                           data = df_encounters)

## 250m buffer model

#Model equation
conflict_250m_model <- glm(encounter_binary ~ prop_natural_cover_250_scaled + prop_open_250_scaled + prop_developed_250_scaled,
                           #Run as logistic regression
                           family = binomial(link = "logit"),
                           #Run with reduced dataset
                           data = df_encounters)

## Null model

#Model equation
null_conflict_model <- glm(encounter_binary ~ 1,
                           #Run as logistic regression
                           family = binomial(link = "logit"),
                           #Run with reduced dataset
                           data = df_encounters)

## Use BIC model selection to determine which land cover buffer radius is the best fit
BIC(conflict_100m_model, conflict_150m_model, conflict_200m_model, conflict_250m_model, null_conflict_model)
}

### Calculate correlation coefficients for model variables -----


## All continuous variables
cor(df_encounters[c(8, 12, 16, 20:29)], method = "pearson")

## Lockdown phase and coyseason
{
#Create frequency table
tbl <- table(df_encounters[c(6,7)])
#Run Chi-Squared Test
chi <- chisq.test(tbl)
#Calculate Cramer's V
sqrt(chi$statistic / sum(tbl))
}

## Lockdown phase and weekday
{
#Create frequency table
tbl <- table(df_encounters[c(3,7)])
#Run Chi-Squared Test
chi <- chisq.test(tbl)
#Calculate Cramer's V
sqrt(chi$statistic / sum(tbl))
}

## Coyote season and weekday
{
#Create frequency table
tbl <- table(df_encounters[c(3,6)])
#Run Chi-Squared Test
chi <- chisq.test(tbl)
#Calculate Cramer's V
sqrt(chi$statistic / sum(tbl))
}

### Compare fit of correlated variables -----

{
## Open and natural

#Model equation
open_model <- glm(encounter_binary ~ prop_open_100_scaled,
                  #Run as a logistic regression
                  family = binomial(link = "logit"),
                  #Run with reduced dataset
                  data = df_encounters)

#Model equation
natural_model <- glm(encounter_binary ~ prop_natural_cover_100_scaled,
                     #Run as a logistic regression
                     family = binomial(link = "logit"),
                     #Run with reduced dataset
                     data = df_encounters)

#Model equation
null_model <- glm(encounter_binary ~ 1,
                  #Run as a logistic regression
                  family = binomial(link = "logit"),
                  #Run with reduced dataset
                  data = df_encounters)

#Use BIC model selection to compare model fits
BIC(open_model, natural_model, null_model)
}

## Maximum, average, and minimum daily temperatures
{
#Model equation
max_temp_model <- glm(encounter_binary ~ max_temp_scaled,
                      #Run as a logistic regression
                      family = binomial(link = "logit"),
                      #Run with reduced dataset
                      data = df_encounters)

#Model equation
min_temp_model <- glm(encounter_binary ~ min_temp_scaled,
                      #Run as a logistic regression
                      family = binomial(link = "logit"),
                      #Run with reduced dataset
                      data = df_encounters)

#Model equation
avg_temp_model <- glm(encounter_binary ~ avg_temp_scaled,
                      #Run as a logistic regression
                      family = binomial(link = "logit"),
                      #Run with reduced dataset
                      data = df_encounters)

#Model equation
null_model <- glm(encounter_binary ~ 1,
                  #Run as a logistic regression
                  family = binomial(link = "logit"),
                  #Run with reduced dataset
                  data = df_encounters)

#Use BIC model selection to compare model fits
BIC(max_temp_model, min_temp_model, avg_temp_model, null_model)
}

## Coyote season and lockdown phase
{
#Model equation
coyseason_model <- glm(encounter_binary ~ coyseason,
                       #Run as a logistic regression
                       family = binomial(link = "logit"),
                       #Run with reduced dataset
                       data = df_encounters)

#Model equation
lockdown_model <- glm(encounter_binary ~ Lockdown_Phase,
                      #Run as a logistic regression
                      family = binomial(link = "logit"),
                      #Run with reduced dataset
                      data = df_encounters)

#Model equation
null_model <- glm(encounter_binary ~ 1,
                  #Run as a logistic regression
                  family = binomial(link = "logit"),
                  #Run with reduced dataset
                  data = df_encounters)

#Use BIC model selection to compare model fits
BIC(coyseason_model, lockdown_model, null_model)
}

### Run model selection on retained variables -----

## Create global model

#Model equation
combined_model_conflicts_sighting <- glm(encounter_binary ~ garbage_scaled + picnic_scaled + d2den_scaled + distance2water_scaled + distance2ocean_scaled + precip_scaled + max_temp_scaled + prop_natural_cover_100_scaled + prop_open_100_scaled + prop_developed_100_scaled + time_cos_scaled + Lockdown_Phase + weekday + Lockdown_Phase:time_cos_scaled + coyseason:d2den_scaled + garbage_scaled:picnic_scaled + weekday:Lockdown_Phase + distance2ocean_scaled:picnic_scaled + distance2ocean_scaled:garbage_scaled + weekday:picnic_scaled + weekday:time_cos_scaled + distance2ocean_scaled:time_cos_scaled,
                                         #Run as a logistic regression
                                         family = binomial(link = "logit"),
                                         #Run with reduced dataset
                                         data = df_encounters,
                                         #Set model to fail if NAs are detected. Mandatory setting for dredge function
                                         na.action = na.fail)

## Identify top models using BIC

#Compare all nested models
combined_conflict_sighting_dredged <- dredge(combined_model_conflicts_sighting, evaluate = TRUE, rank = "BIC")

#View model rankings
View(combined_conflict_sighting_dredged)

## Create top model #1

#Model equation
top_model_1 <- glm(encounter_binary ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + picnic_scaled + precip_scaled + time_cos_scaled + weekday,
                                     #Run as a logistic regression
                                     family = binomial(link = "logit"),
                                     #Run with reduced dataset
                                     data = df_encounters)

## Evaluate top model #1

#Assumption plots
plot(top_model_1)

#Summary table
summary(top_model_1)

#ANOVA table
Anova(top_model_1, test = "F")

#Confidence intervals
confint(top_model_1)

#Evaluate collinearity with VIF
vif(top_model_1)

#Model fit 'visreg' plots
visreg(top_model_1, scale = "response", ylab = "Probability of report being of aggression or an attack", ylim = c(0,1)) 

## Create top model #2

#Model equation
top_model_2 <- glm(encounter_binary ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + garbage_scaled + precip_scaled + time_cos_scaled + weekday,
                                     #Run as a logistic regression
                                     family = binomial(link = "logit"),
                                     #Run with reduced dataset
                                     data = df_encounters)

## Evaluate top model #2

#Assumption plots
plot(top_model_2)

#Summary table
summary(top_model_2)

#ANOVA table
Anova(top_model_2, test = "F")

#Confidence intervals
confint(top_model_2)

#Evaluate collinearity with VIF
vif(top_model_2)

#Model fit 'visreg' plots
visreg(top_model_2, scale = "response", ylab = "Probability of report being of aggression or an attack", ylim = c(0,1)) 

### Run model selection with subset of data where coord precision is <= 100m -----

## Create dataset with only precise data

df_precise_coords <- df_encounters_full %>%
  filter(is.na(coord_confidence) | coord_confidence <= 100) %>%
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
  mutate(encounter_binary = ifelse(encounter_binary == 0, 0, 1),
         #add factor column for cv
         encounter_binary_fc = case_when(encounter_binary == 0 ~ "sighting",
                                         encounter_binary == 1 ~ "encounter",
                                         TRUE ~ NA_character_)) %>%
  #remove all data points with NAs
  na.omit()

## Create global model

#Model equation
combined_model_conflicts_sighting_precise <- glm(encounter_binary ~ garbage_scaled + picnic_scaled + d2den_scaled + distance2water_scaled + distance2ocean_scaled + precip_scaled + max_temp_scaled + prop_natural_cover_100_scaled + prop_developed_100_scaled + time_cos_scaled + Lockdown_Phase + weekday + Lockdown_Phase:time_cos_scaled + coyseason:d2den_scaled + garbage_scaled:picnic_scaled + weekday:Lockdown_Phase + distance2ocean_scaled:picnic_scaled + distance2ocean_scaled:garbage_scaled + weekday:picnic_scaled + weekday:time_cos_scaled + distance2ocean_scaled:time_cos_scaled,
                                         #Run as a logistic regression
                                         family = binomial(link = "logit"),
                                         #Run with dataset containing only reports with <=100m uncertainty radius
                                         data = df_precise_coords,
                                         #Set model to fail if NAs are detected. Mandatory setting for dredge function
                                         na.action = na.fail)

## Identify top models using BIC

#Compare all nested models
combined_conflict_sighting_precise_dredged <- dredge(combined_model_conflicts_sighting_precise, evaluate = TRUE, rank = "BIC")

#View model rankings
View(combined_conflict_sighting_precise_dredged)

### Stratified k-fold cross validation -----

## Define the control for k-fold CV with stratification based on the response variable
set.seed(123)  # For reproducibility
cv_control <- trainControl(method = "cv", 
                           number = 5,  # Number of folds
                           classProbs = TRUE,  # Necessary for classification models
                           summaryFunction = twoClassSummary,
                           #downsample the more prominent class (sightings)
                           sampling = "down")

#Define a place to store the ROC, sens, and spec data for model 1
df_model_1_results <- tibble(
  ROC = rep(NA, 10),
  Sens = rep(NA, 10),
  Spec = rep(NA, 10)
)
## Loop for averaging results over 10 iterations
for (i in 1:10) {
  ## Train the first model with cross-validation
  model_1 <- train(encounter_binary_fc ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + picnic_scaled + precip_scaled + time_cos_scaled + weekday,
                   data = df_encounters, 
                   method = "glm",
                   family = "binomial",
                   trControl = cv_control,
                   # Choose your preferred metric, e.g., "Accuracy" or "ROC"
                   metric = "ROC")  
  #Save results
  df_model_1_results$ROC[i] = model_1$results[[2]]
  df_model_1_results$Sens[i] = model_1$results[[3]]
  df_model_1_results$Spec[i] = model_1$results[[4]]
}

## Calculate means after 10 iterations
mean(df_model_1_results$ROC)
mean(df_model_1_results$Sens)
mean(df_model_1_results$Spec)


#Define a place to store the ROC, sens, and spec data for model 2
df_model_2_results <- tibble(
  ROC = rep(NA, 10),
  Sens = rep(NA, 10),
  Spec = rep(NA, 10)
)
## Loop for averaging results over 10 iterations
for (i in 1:10) {
  ## Train the second model with cross-validation
  model_2 <- train(encounter_binary_fc ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + garbage_scaled + precip_scaled + time_cos_scaled + weekday,
                   data = df_encounters, 
                   method = "glm",
                   family = "binomial",
                   trControl = cv_control,
                   # Choose your preferred metric, e.g., "Accuracy" or "ROC"
                   metric = "ROC")  
  #Save results
  df_model_2_results$ROC[i] = model_2$results[[2]]
  df_model_2_results$Sens[i] = model_2$results[[3]]
  df_model_2_results$Spec[i] = model_2$results[[4]]
}

## Calculate means after 10 iterations
mean(df_model_2_results$ROC)
mean(df_model_2_results$Sens)
mean(df_model_2_results$Spec)

### Test for differences between expected and observed victim demographics and activities -----

## Victim gender/age group
{
#Sum of total humans (extrapolated to hour-long time blocks)
total_humans <- sum(df_human_activity_blocks$male_adults_adjusted) + sum(df_human_activity_blocks$female_adults_adjusted) + sum(df_human_activity_blocks$children_adjusted)
#Calculate expected total of adult males
expected_adult_males <- sum(df_human_activity_blocks$male_adults_adjusted)/total_humans
#Calculate expected total of adult females
expected_adult_females <- sum(df_human_activity_blocks$female_adults_adjusted)/total_humans
#Calculate expected total of children
expected_children <- sum(df_human_activity_blocks$children_adjusted)/total_humans

## Victim sex/age group observed totals

victim_demo_groups <- df_encounters_full %>%
  #Keep only age and sex variables
  dplyr::select(victim_age, victim_sex) %>%
  #Create column to group victims into children, male adults, and female adults
  mutate(group = ifelse(victim_age == "child", "C",
                        ifelse(victim_sex == "M","MA",
                               ifelse(victim_sex == "F","FA",
                                      NA)))) %>%
  #Remove any that do not fit into a group
  filter(!is.na(group)) %>%
  #Group the dataset by the group column
  group_by(group) %>%
  #Add a column with the number of attack victims in each group
  mutate(total = n()) %>%
  #Keep only one row per group
  distinct(group, .keep_all = TRUE) %>%
  #Keep only the group and total columns
  dplyr::select(group, total) %>%
  #Switch the dataset to wide format for analysis
  pivot_wider(names_from = group, values_from = total)

## Goodness of Fit for victim sex/age

df_victim_demos <- c(victim_demo_groups$MA[1], victim_demo_groups$FA[1], victim_demo_groups$C[1])
chisq.test(df_victim_demos, p = c(expected_adult_males, expected_adult_females, expected_children))
}

## Victim activity
{
#Sum of total humans (extrapolated to hour-long time blocks)
total_humans <- sum(df_human_activity_blocks$walkers_adjusted) + sum(df_human_activity_blocks$runners_adjusted) + sum(df_human_activity_blocks$wheels_adjusted)
#Calculate expected total of walkers
expected_walkers <- sum(df_human_activity_blocks$walkers_adjusted)/total_humans
#Calculate expected total of runners
expected_runners <- sum(df_human_activity_blocks$runners_adjusted)/total_humans
#Calculate expected total of cyclists and other wheel-riders
expected_wheels <- sum(df_human_activity_blocks$wheels_adjusted)/total_humans

## Victim activity observed totals

victim_activity_groups <- df_encounters_full %>%
  #Keep only age and sex variables
  dplyr::select(victim_activity) %>%
  #Create column to group victims into walkers, runners, and wheels
  mutate(victim_activity = ifelse(victim_activity == "walking", "Walk",
                                  ifelse(victim_activity == "running", "Run",
                                         ifelse(victim_activity == "scooter_collision", "Wheels",
                                                NA)))) %>%
  #Remove any that do not fit into an activity
  filter(!is.na(victim_activity)) %>%
  #Group the dataset by activity
  group_by(victim_activity) %>%
  #Add a column with the number of attack victims doing each activity
  mutate(total = n()) %>%
  #Keep only one row per activity
  distinct(victim_activity, .keep_all = TRUE) %>%
  #Keep only the activity and total columns
  dplyr::select(victim_activity, total) %>%
  #Switch to wide format for analysis
  pivot_wider(names_from = victim_activity, values_from = total)

## Goodness of Fit for victim activity

df_victim_activity <- c(victim_activity_groups$Walk[1], victim_activity_groups$Run[1], victim_activity_groups$Wheels[1])
chisq.test(df_victim_activity, p = c(expected_walkers, expected_runners, expected_wheels))
}

## Dog presence
{
df_expected_dogs <- df_individual_human_activity %>%
  #Keep only one row per group
  distinct(group_ID, .keep_all = TRUE) %>%
  #Add binary variable for dog presence
  mutate(dogs_binary = ifelse(dogs > 0, "Dogs", "NoDogs")) %>%
  #Group the data by dog presence
  group_by(dogs_binary) %>%
  #Add a column with the total number of groups with/without dogs
  mutate(total = n()) %>%
  #Keep only dog presence and total
  dplyr::select(dogs_binary, total) %>%
  #Keep only one row per group
  distinct(dogs_binary, .keep_all = TRUE) %>%
  #Switch the dataset to wide format for analysis
  pivot_wider(names_from = dogs_binary, values_from = total)

## Dog presence expected totals

#Sum of total human groups (extrapolated to hour-long time blocks)
total_groups <- df_expected_dogs$NoDogs + df_expected_dogs$Dogs
#Calculate expected total groups with dogs
expected_dogs <- df_expected_dogs$Dogs[1]/total_groups
#Calculate expected total groups without dogs
expected_NoDogs <- df_expected_dogs$NoDogs[1]/total_groups

## Observed victims with/without dogs

#No victims had dogs with them
victim_dogs <- c(34, 0)

## Goodness of Fit for dog presence
chisq.test(victim_dogs, p = c(expected_NoDogs, expected_dogs))
}

## Human group size
{
df_human_group_size <- df_individual_human_activity %>%
  #Group dataset by group ID
  group_by(group_ID) %>%
  #Add column for number of people in each group
  mutate(group_size = n()) %>%
  #Ungroup the dataset
  ungroup() %>%
  #Keep only one row per group
  distinct(group_ID, .keep_all = TRUE) %>%
  #Add column for binary group size: group or individual
  mutate(group_size_binary = ifelse(group_size > 1, "Group", "Individual")) %>%
  #Group the data by binary group size
  group_by(group_size_binary) %>%
  #Add column for total groups and individuals
  mutate(total = n()) %>%
  #Keep only one row per group size binary value
  distinct(group_size_binary, .keep_all = TRUE) %>%
  #Keep only binary group size and total columns
  dplyr::select(group_size_binary, total) %>%
  #Switch the dataset to wide format for analysis
  pivot_wider(names_from = group_size_binary, values_from = total) %>%
  #Calculate total number of groups and individuals
  mutate(total = Group + Individual)

## Human group size observed totals

victim_group_size <- df_encounters_full %>%
  #Keep only human group size variable
  dplyr::select(human_group_size) %>%
  #Add binary column with individual or group designation
  mutate(victim_group_size = ifelse(human_group_size == 1, "Individual",
                                    "Group")) %>%
  #Remove rows with missing values
  filter(!is.na(victim_group_size)) %>%
  #Group dataset by group size
  group_by(victim_group_size) %>%
  #Add a column with total individuals and total groups
  mutate(total = n()) %>%
  #Keep only one row with individuals and one with groups
  distinct(victim_group_size, .keep_all = TRUE) %>%
  #Keep only group size and total columns
  dplyr::select(victim_group_size, total) %>%
  #Switch the dataset to wide format for analysis
  pivot_wider(names_from = victim_group_size, values_from = total)

## Goodness of Fit for group size

chisq.test(c(victim_group_size$Individual[1],victim_group_size$Group[1]),
           p = c(df_human_group_size$Individual[1]/df_human_group_size$total[1],
                 df_human_group_size$Group[1]/df_human_group_size$total[1]))
}

### Figures from publication -----

## Plot confidence intervals
{
#List of variables
Variables_model_1 = c("Intercept", "Distance from den", "Distance from ocean", "Lockdown phase 2: Social\ngatherings prohibited", "Lockdown phase 3: Social\ngatherings limited", "Distance from picnic area", "Daily precipitation", "Time of day", "Weekend")
Variables_model_2 = c("Intercept", "Distance from den", "Distance from ocean", "Lockdown phase 2: Social\ngatherings prohibited", "Lockdown phase 3: Social\ngatherings limited", "Distance from garbage bin", "Daily precipitation", "Time of day", "Weekend")

#List of coefficients
coefficients_1 = c(-5.0185, -0.9734, -0.6069, 3.8010, 3.2220, 0.6079, 0.4698, -0.8824, -1.4404)
coefficients_2 = c(-5.0017, -0.7469, -0.4808, 3.7972, 3.1186, 0.4555, 0.4352, -0.8396, -1.2830)

#Create dataframe with confidence intervals for model 1
CI_model_1 <- as_tibble(confint(top_model_1)) %>%
  #Add lists of variables and coefficients
  cbind(Variables_model_1, coefficients_1) %>%
  #Switch dataframe to long format
  pivot_longer(cols = c(`2.5 %`, `97.5 %`) , names_to = "Level", values_to = "CL") %>%
  #Set order of variables
  mutate(Variable=factor(Variables_model_1,levels = c("Daily precipitation", "Distance from picnic area", "Distance from ocean","Time of day", "Distance from den", "Weekend", "Lockdown phase 3: Social\ngatherings limited", "Lockdown phase 2: Social\ngatherings prohibited", "Intercept")),
  #Add model label
         Model = "Model 1") %>%
  #Rename coefficients column to match other model df
  rename(Coefficient = coefficients_1) %>%
  dplyr::select(Model, Variable, Coefficient, Level, CL)

#Create dataframe with confidence intervals for model 2
CI_model_2 <- as_tibble(confint(top_model_2)) %>%
  #Add lists of variables and coefficients
  cbind(Variables_model_2, coefficients_2) %>%
  #Switch dataframe to long format
  pivot_longer(cols = c(`2.5 %`, `97.5 %`) , names_to = "Level", values_to = "CL") %>%
  #Set order of variables
  mutate(Variable=factor(Variables_model_2,levels = c("Daily precipitation","Distance from garbage bin","Distance from ocean","Time of day", "Distance from den", "Weekend", "Lockdown phase 3: Social\ngatherings limited", "Lockdown phase 2: Social\ngatherings prohibited", "Intercept")),
         #Add model label
         Model = "Model 2") %>%
  #Rename coefficients column to match other model df
  rename(Coefficient = coefficients_2) %>%
  dplyr::select(Model, Variable, Coefficient, Level, CL)

#Combine dfs for top models
CIs <- CI_model_1 %>%
  full_join(CI_model_2)

#Create plot and specify variables
ggplot(CIs, aes(x = CL, y = factor(Variable, levels = c("Daily precipitation","Distance from garbage bin","Distance from ocean","Distance from picnic area","Time of day", "Distance from den", "Weekend", "Lockdown phase 3: Social\ngatherings limited", "Lockdown phase 2: Social\ngatherings prohibited", "Intercept")))) +
  #Format data into lines
  geom_line(aes(col = Model)) +
  #Add dashed vertical line at x=0
  geom_vline(xintercept = 0, linetype = "dashed") +
  #Add horizontal lines to separate types of variables
  geom_hline(yintercept = c(6.5, 9.5)) +
  #Add sections labels, and specify locations
  geom_text(data = data.frame(x = c(-7,-7,-7),
                              y = c(6.3,9.3,10.3),
                              label=c("Continuous variables","Categorical variables","Intercept")),
            aes(x=x,y=y,label=label, group = x), size = 4, hjust = 0) +
  #Set theme to black and white
  theme_classic() +
  #Set x label
  labs(x = "95% Confidence Intervals", y = "Predictor Variable") +
  #Add points for coefficients
  geom_point(CIs, mapping = aes(x = Coefficient, col = Model)) +
  #Remove legend
  theme(legend.position="none") +
  #Set text size
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  #Set x-axis breaks
  scale_x_continuous(breaks = c(-6,-4,-2,0,2,4,6))
}

## Table of top models
{
#List of model ranks
`Model rank` <- c(1,2,"2392 (null)")
  
#List of model equations
`Model formula` <- c("d2den + d2ocean + d2picnic + lockdown_phase + precipitation + time_of_day + weekday",
                     "d2den + d2ocean + d2garbage_bin + lockdown_phase + precipitation + time_of_day + weekday",
                     "null")

#List BIC scores
`BIC` <- c(356.85, 358.29, 506.56)

#List ∆BIC
`∆BIC` <- c(0.00, 1.45, 149.71)

#List model weights
`Weight` <- c(0.26, 0.13, 8.10e-34)

#List log-likelihoods
LL <- c(-150.22, -150.94, -250.14)

#Create dataframe of lists
df_top_models_table <- tibble(`Model rank`, `Model formula`, `BIC`, `∆BIC`, `Weight`, LL)

#Format table
nice_table(df_top_models_table)
}

## Table of top variables
{
#List variables
Variable <- c("Lockdown phase", "Distance to nearest den", "Distance to ocean", "Time of day", "Daily precipitation", "Distance to nearest garbage bin", "Week phase", "Null", "Distance to nearest picnic area")
#List the number of appearances in the top models
`Appearances in top models` <- c(2, 2, 2, 2, 2, 1, 2, "NA", 1)
#List univariate ∆BIC values
`∆BIC of univariate model` <- c(107.02, 109.37, 115.30, 124.08, 132.62, 136.80, 141.86, 149.71, 155.46)

#Create dataframe of lists
df_top_variables_table <- tibble(Variable, `Appearances in top models`, `∆BIC of univariate model`)

#Format table
nice_table(df_top_variables_table)
}

## Multi-plot for logistic regression variables
{ 

## Lockdown phase bar plot

p_lockdown <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter = ifelse(encounter_binary == 0, "Sighting", "Aggression")) %>%
  #Remove NAs
  filter(!is.na(Lockdown_Phase)) %>%
  #Create plot and set variables
  ggplot(aes(x = Lockdown_Phase, fill = encounter)) +
  #Format as grouped bar plot
  geom_bar(stat = "count", position = "dodge") +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Pandemic social restriction phase",
       y = "Reports", fill = "Report type")

## Coyote season

# Specify order for x-axis
season_order <- c('Breeding', 'Pup-rearing', 'Dispersal')
p_season <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter = ifelse(encounter_binary == 0, "Sighting", "Aggression"),
         #Rename seasons with capitals
         coyseason = ifelse(coyseason == "breeding", "Breeding",
                            ifelse(coyseason == "dispersal", "Dispersal",
                                   "Pup-rearing"))) %>%
  #Create plot and set variables
  ggplot(aes(x = factor(coyseason, level = season_order), fill = encounter)) +
  #Format as grouped bar plot
  geom_bar(stat = "count", position = "dodge") +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,100,200)) +
  #Set labels
  labs(x = "Phenological season", y = "Reports", fill = "Encounter")

## Week phase
p_weekphase <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter = ifelse(encounter_binary == 0, "Sighting", "Aggression"),
         #Rename week phases with capitals
         weekday = ifelse(weekday == "weekday", "Weekday", "Weekend")) %>%
  #Create plot and set variables
  ggplot(aes(x = weekday, fill = encounter)) +
  #Format as grouped bar plot
  geom_bar(stat = "count", position = "dodge") +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Weekday", y = "Reports", fill = "Encounter")

## Distance from den
p_den <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = d2den, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.0005,0.001)) +
  #Set labels
  labs(x = "Distance from nearest den (m)", y = "Density", col = "Encounter")

## Precipitation
p_precip <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = precip, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,1,2)) +
  #Set labels
  labs(x = "Daily precipitation (mm)", y = "Density", col = "Encounter")

## Max temp
p_maxtemp <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = max_temp, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Daily maximum temperature (˚C)", y = "Density", col = "Encounter")

## Time of day
p_time <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = time_cos, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Time of day (cos transformed)", y = "Density", col = "Encounter")  +
  #Add reference labels for time of day along the x-axis
  geom_text(aes(x=x,y=y,label=label),
            inherit.aes = FALSE, #resolves error
            data = data.frame(x = c(-0.8, 0, 0, 0.8)),
            y = c(0.1, 0.2, 0.05, 0.1),
            label=c("0200 hours","0800 hours","2000 hours","1400 hours"),
            size = 5)

## Distance from ocean
p_ocean <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = d2ocean, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.0015,0.003)) +
  #Set labels
  labs(x = "Distance from the ocean (m)", y = "Density", col = "Encounter")

## Distance from water
p_water <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = distance2water, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.002, 0.004)) +
  #Set labels
  labs(x = "Distance from water (m)", y = "Density", col = "Encounter")

## Distance to nearest garbage bin
p_garbage <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = garbage, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.003,0.006)) +
  #Set labels
  labs(x = "Distance from nearest garbage bin (m)", y = "Density", col = "Encounter")

## Distance to nearest picnic area
p_picnic <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = outdoor_eating, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.001,0.002)) +
  #Set labels
  labs(x = "Distance from nearest picnic area (m)", y = "Density", col = "Encounter")

## Proportion of 100m buffer - natural cover
p_natural <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = prop_natural_cover_100, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Proportion natural area in 100m buffer", y = "Density", col = "Encounter")

## Proportion of 100m buffer - open
p_open <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = prop_open_100, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Proportion open area in 100m buffer", y = "Density", col = "Encounter")

## Proportion of 100m buffer - developed
p_developed <- df_encounters_full %>%
  filter(!is.na(Lockdown_Phase)) %>%
  #Add column for sightings or aggression
  mutate(encounter_binary = ifelse(encounter_binary == 0, "Sighting", "Aggressive")) %>%
  #Create plot and set variables
  ggplot((aes(x = prop_developed_100, col = encounter_binary))) +
  #Format as density plot
  geom_density() +
  #Remove gridlines
  theme_classic() +
  #Set labels
  labs(x = "Proportion developed area in 100m buffer", y = "Density", col = "Encounter")

## Combine all plots into one multi-panel plot

#Include all individual plots
multi_plot<- ggarrange(p_lockdown, p_den, p_ocean, p_time, p_precip, p_garbage, p_weekphase, p_picnic, p_season, p_water, p_natural, p_developed, p_open, p_maxtemp,
                       #Define plot labels
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N"), 
                       #Arrange plot positions
                       ncol = 3, nrow = 7,
                       #Include legend
                       common.legend = T,
                       #Fix position of labels
                       vjust = -0.5) 

#Add titles and labels to the multi-panel graph
multi_plot <- annotate_figure(multi_plot)
multi_plot
#ggsave(filename = "SP_multiplot.png", multi_plot, dpi = "retina")
}

## Plot interaction density over time
{
df_encounters_full %>%
  #Fix name of non-contact aggressive encounters for display purposes
  mutate(encounter = ifelse(encounter == "Aggression to Human",
                            "Non-contact aggression",
                            encounter)) %>%
  #Create plot and set variables
  ggplot(aes(x=date, fill=encounter, color = encounter)) +
  #Format as density plot with mostly transparent fill
  geom_density(alpha=.2) +
  #Remove gridlines
  theme_classic() +
  #Add vertical dotted lines at the beginning of restriction phases two and three
  geom_vline(xintercept = as.numeric(as.Date(c("2020-12-02", "2021-05-26"))), linetype = 'dotted') +
  #Add a vertical dashed line at the beginning of restriction phase one
  geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), linetype = 'dashed') +
  #Add labels to the inserted vertical lines
  geom_label(aes(x=x,y=y,label=label),
             inherit.aes = FALSE, #resolves error
             data = data.frame(x = as.Date(c("2020-03-11","2020-12-02","2021-05-25")),
                               y = c(0.009, 0.009, 0.009),
                               label=c("Start of pandemic\nrestrictions","Indoor gatherings\nbanned","Indoor gatherings\npermitted")),
             size = 4) +
  #Set axis and legend labels
  labs(x = "Date", y = "Density of reports", fill = "Report type", color = "Report type") +
  #Set font size
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 15))
}

## Human victim demographics multi-plot
{
  ## Activity
  p_activity <- victim_activity_groups %>%
    pivot_longer(cols = c(1:3), names_to = "Activity", values_to = "Observed") %>%
    mutate(Expected = c(expected_runners*sum(victim_activity_groups), expected_walkers*sum(victim_activity_groups), expected_wheels*sum(victim_activity_groups))) %>%
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    ggplot(aes(x = Activity, y = Frequency, fill = Category)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_classic()
  
  ## Group size
  p_group_size <- victim_group_size %>%
    pivot_longer(cols = c(1:2), names_to = "Group Size", values_to = "Observed") %>%
    mutate(Expected = c(df_human_group_size$Individual/df_human_group_size$total*sum(victim_group_size), df_human_group_size$Group/df_human_group_size$total*sum(victim_group_size))) %>%
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    ggplot(aes(x = `Group Size`, y = Frequency, fill = Category)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_classic()
  
  ## Age
  p_demo <- victim_demo_groups %>%
    pivot_longer(cols = c(1:3), names_to = "Group", values_to = "Observed") %>%
    mutate(Expected = c(expected_adult_males*sum(victim_demo_groups), expected_adult_females*sum(victim_demo_groups), expected_children*sum(victim_demo_groups))) %>%
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    mutate(Group = case_when(
      Group == "MA" ~ "Male adults",
      Group == "FA" ~ "Female adults",
      Group == "C" ~ "Children",
      TRUE ~ NA_character_
    )) %>%
    ggplot(aes(x = Group, y = Frequency, fill = Category)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_classic()
  
  ## Dog Presence
  p_dog <- as_tibble(victim_dogs) %>%
    rename(Observed = value) %>%
    mutate(Expected = c(expected_NoDogs*sum(victim_dogs), expected_dogs*sum(victim_dogs)),
           Dogs = c("Absent", "Present")) %>%
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    ggplot(aes(x = Dogs, y = Frequency, fill = Category)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme_classic()   

  ## Combine all plots into one multi-panel plot
  
  #Include all individual plots
  victim_multi_plot <- ggarrange(p_activity, p_group_size, p_demo, p_dog,
                         #Define plot labels
                         labels = c("A", "B", "C", "D"),
                         #Arrange plot positions
                         ncol = 2, nrow = 2,
                         #Include legend
                         common.legend = T,
                         #Fix position of labels
                         vjust = 0.5) 
  
  #Add titles and labels to the multi-panel graph
  victim_multi_plot <- annotate_figure(victim_multi_plot)
  victim_multi_plot
  ggsave(filename = "SP_victims_multiplot.png", victim_multi_plot, dpi = "retina")
}

## Human activity multi-plot for appendix
{
  human_multiplot <- df_human_activity_blocks %>%
    mutate(site_category = case_when(
      site_category == "developed" ~ "Developed",
      site_category == "forest" ~ "Forest",
      site_category == "lakeside" ~ "Lakeside",
      site_category == "seawall" ~ "Seawall",
      TRUE ~ NA_character_
    )) %>%
    ggplot(aes(x = start_time, y = total_humans_adjusted, col = weather)) +
    geom_point(aes(col = weather)) +
    geom_smooth(method = "loess", se = FALSE, span = 3) +
    facet_wrap(~site_category) +
    scale_y_log10() +
    theme_bw() +
    labs(x = "Time of day", y = "People per hour", col = "Weather")
  #ggsave(filename = "SP_human_multiplot.png", human_multiplot, dpi = "retina")
}