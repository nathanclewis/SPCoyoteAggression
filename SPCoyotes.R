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
library(car) #for Anova and vif
library(leaflet) #for mapping
library(ggpubr) #for multi-panel figures
library(caret) #for cross-validation
  
options(scipen=10000) #no scientific notation on plots
}

### Load data -----

{
## Full coyote dataset (used for plots)
df_encounters_full <- read_csv("Data/sp_coyote_project_dataset.csv")

## Data subset with analysis variables only (used for analyses with no accuracy cutoff)
df_encounters <- df_encounters_full %>%
  #keep only variables to be included in analyses
  dplyr::select(encounter_ID, date, weekday, encounter, encounter_binary, coyseason, Lockdown_Phase,
                prop_open_100_scaled, prop_open_150_scaled, prop_open_200_scaled, prop_open_250_scaled,
                prop_natural_cover_100_scaled, prop_natural_cover_150_scaled, prop_natural_cover_200_scaled, prop_natural_cover_250_scaled,
                prop_developed_100_scaled, prop_developed_150_scaled, prop_developed_200_scaled, prop_developed_250_scaled,
                garbage_scaled, picnic_scaled, distance2water_scaled, distance2ocean_scaled, d2den_scaled, d2den,
                precip_scaled, avg_temp_scaled, min_temp_scaled, max_temp_scaled, time_cos_scaled, lon, lat) %>%
  #remove any reports from outside study timeline
  filter(!is.na(Lockdown_Phase)) %>%
  #fix binary column so attacks and aggression are both 1 and sightings are 0
  mutate(encounter_binary = ifelse(encounter_binary == 0, 0, 1),
         #add factor column for cross validation
         encounter_binary_fc = case_when(encounter_binary == 0 ~ "sighting",
                                         encounter_binary == 1 ~ "encounter",
                                         TRUE ~ NA_character_)) %>%
  #change reference category from weekday to weekend
  mutate(weekday = fct_relevel(weekday, "weekend")) %>%
  #remove all data points with NAs
  na.omit() #%>%
  #remove spatiotemporally similar (possibly auto-correlated) events
  #filter(!encounter_ID %in% c(1760, 1761, 2255, 'COS5', 1673, 5839, 5840, 4917, 1693, 5061, 5841, 4981, 4979, 5830, 4987,
  #                            4984, 5857, 5854, 5156, 5860, 5858, 5864, 1828, 2286, 5198, 5304, 5367, 6124, 5376, 2335,
  #                            5510, 4588, 4733, 2244, 5058, 1762, 2256, 2249, 5063, 1758, 1818, 5261, 5262, 5292, 5293))

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
  addCircleMarkers(~lon, ~lat, radius = 10, fillColor = ~color_pal(encounter), fillOpacity = 1,
                   #add a popup to tell you what type of encounter each point represents when clicked on
                   popup = ~encounter)
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

#Generate ANOVA table with F and p-values
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

## Compare all continuous variables
cor(df_encounters[c(8, 12, 16, 20:29)], method = "spearman")

## Degrees of freedom for comparisons
length(df_encounters$prop_open_100_scaled) - 2

## Lockdown phase and coyseason (phenological season)
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

## Coyseason and weekday
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

## Coyote phenological season and lockdown phase
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

### Test for non-linear effects in retained predictors -----

## Create new dataframe with quadratic versions of each variable

#Start with the reduced dataset
df_nonlinear_tests <- df_encounters %>%
  #Create a new variable for the quadratic form of each retained variable
  mutate(nat_100_quad = prop_natural_cover_100_scaled^2,
         dev_100_quad = prop_developed_100_scaled^2,
         picnic_quad = picnic_scaled^2,
         garb_quad = garbage_scaled^2,
         d2den_quad = d2den_scaled^2,
         d2ocean_quad = distance2ocean_scaled^2,
         d2water_quad = distance2water_scaled^2,
         precip_quad = picnic_scaled^2,
         max_temp_quad = max_temp_scaled^2,
         time_quad = time_cos_scaled^2)

## Natural cover
{
  #Model equation
  nat_linear <- glm(encounter_binary ~ prop_natural_cover_100_scaled,
                    #Run with the new dataset with nonlinear variables
                    data = df_nonlinear_tests,
                    #Run as a logistic regression
                    family = binomial(link = "logit"))
  
  #Model equation
  nat_nonlinear <- glm(encounter_binary ~ prop_natural_cover_100_scaled + nat_100_quad,
                       #Run with the new dataset with nonlinear variables
                       data = df_nonlinear_tests,
                       #Run as a logistic regression
                       family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(nat_linear, nat_nonlinear)
}

## Developed cover
{
  #Model equation
  dev_linear <- glm(encounter_binary ~ prop_developed_100_scaled,
                    #Run with the new dataset with nonlinear variables
                    data = df_nonlinear_tests,
                    #Run as a logistic regression
                    family = binomial(link = "logit"))
  
  #Model equation
  dev_nonlinear <- glm(encounter_binary ~ prop_developed_100_scaled + dev_100_quad,
                       #Run with the new dataset with nonlinear variables
                       data = df_nonlinear_tests,
                       #Run as a logistic regression
                       family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(dev_linear, dev_nonlinear)
}

## Distance to nearest picnic area
{
  #Model equation
  picnic_linear <- glm(encounter_binary ~ picnic_scaled,
                       #Run with the new dataset with nonlinear variables
                       data = df_nonlinear_tests,
                       #Run as a logistic regression
                       family = binomial(link = "logit"))
  
  #Model equation
  picnic_nonlinear <- glm(encounter_binary ~ picnic_scaled + picnic_quad,
                          #Run with the new dataset with nonlinear variables
                          data = df_nonlinear_tests,
                          #Run as a logistic regression
                          family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(picnic_linear, picnic_nonlinear)
}

## Distance to nearest garbage bin
{
  #Model equation
  garb_linear <- glm(encounter_binary ~ garbage_scaled,
                     #Run with the new dataset with nonlinear variables
                     data = df_nonlinear_tests,
                     #Run as a logistic regression
                     family = binomial(link = "logit"))
  
  #Model equation
  garb_nonlinear <- glm(encounter_binary ~ garbage_scaled + garb_quad,
                        #Run with the new dataset with nonlinear variables
                        data = df_nonlinear_tests,
                        #Run as a logistic regression
                        family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(garb_linear, garb_nonlinear)
}

## Distance to nearest den
{
  #Model equation
  den_linear <- glm(encounter_binary ~ d2den_scaled,
                    #Run with the new dataset with nonlinear variables
                    data = df_nonlinear_tests,
                    #Run as a logistic regression
                    family = binomial(link = "logit"))
  
  #Model equation
  den_nonlinear <- glm(encounter_binary ~ d2den_scaled + d2den_quad,
                       #Run with the new dataset with nonlinear variables
                       data = df_nonlinear_tests,
                       #Run as a logistic regression
                       family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(den_linear, den_nonlinear)
}

## Distance to the ocean
{
  #Model equation
  water_linear <- glm(encounter_binary ~ distance2water_scaled,
                      #Run with the new dataset with nonlinear variables
                      data = df_nonlinear_tests,
                      #Run as a logistic regression
                      family = binomial(link = "logit"))
  
  #Model equation
  water_nonlinear <- glm(encounter_binary ~ distance2water_scaled + d2water_quad,
                         #Run with the new dataset with nonlinear variables
                         data = df_nonlinear_tests,
                         #Run as a logistic regression
                         family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(water_linear, water_nonlinear)
}

## Distance to water
{
  #Model equation
  ocean_linear <- glm(encounter_binary ~ distance2ocean_scaled,
                      #Run with the new dataset with nonlinear variables
                      data = df_nonlinear_tests,
                      #Run as a logistic regression
                      family = binomial(link = "logit"))
  
  #Model equation
  ocean_nonlinear <- glm(encounter_binary ~ distance2ocean_scaled + d2ocean_quad,
                         #Run with the new dataset with nonlinear variables
                         data = df_nonlinear_tests,
                         #Run as a logistic regression
                         family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(ocean_linear, ocean_nonlinear)
}

## Precipitation
{
  #Model equation
  precip_linear <- glm(encounter_binary ~ precip_scaled,
                       #Run with the new dataset with nonlinear variables
                       data = df_nonlinear_tests,
                       #Run as a logistic regression
                       family = binomial(link = "logit"))
  
  #Model equation
  precip_nonlinear <- glm(encounter_binary ~ precip_scaled + precip_quad,
                          #Run with the new dataset with nonlinear variables
                          data = df_nonlinear_tests,
                          #Run as a logistic regression
                          family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(precip_linear, precip_nonlinear)
}

## Maximum daily temperature
{
  #Model equation
  temp_linear <- glm(encounter_binary ~ max_temp_scaled,
                     #Run with the new dataset with nonlinear variables
                     data = df_nonlinear_tests,
                     #Run as a logistic regression
                     family = binomial(link = "logit"))
  
  #Model equation
  temp_nonlinear <- glm(encounter_binary ~ max_temp_scaled + max_temp_quad,
                        #Run with the new dataset with nonlinear variables
                        data = df_nonlinear_tests,
                        #Run as a logistic regression
                        family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(temp_linear, temp_nonlinear)
}

## Time of day
{
  #Model equation
  time_linear <- glm(encounter_binary ~ time_cos_scaled,
                     #Run with the new dataset with nonlinear variables
                     data = df_nonlinear_tests,
                     #Run as a logistic regression
                     family = binomial(link = "logit"))
  
  #Model equation
  time_nonlinear <- glm(encounter_binary ~ time_cos_scaled + time_quad,
                        #Run with the new dataset with nonlinear variables
                        data = df_nonlinear_tests,
                        #Run as a logistic regression
                        family = binomial(link = "logit"))
  
  #Compare using BIC
  BIC(time_linear, time_nonlinear)
}

### Run model selection on retained variables -----

## Create global model

#Model equation
combined_model_conflicts_sighting <- glm(encounter_binary ~ garbage_scaled + picnic_scaled + d2den_scaled + distance2water_scaled + distance2ocean_scaled + precip_scaled + max_temp_scaled + prop_natural_cover_100_scaled + prop_developed_100_scaled + time_cos_scaled + weekday + Lockdown_Phase + Lockdown_Phase:time_cos_scaled + Lockdown_Phase:weekday + coyseason:d2den_scaled + garbage_scaled:picnic_scaled + distance2ocean_scaled:picnic_scaled + distance2ocean_scaled:garbage_scaled + weekday:picnic_scaled + weekday:time_cos_scaled + distance2ocean_scaled:time_cos_scaled,
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
{
#Model equation
top_model_1 <- glm(encounter_binary ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + picnic_scaled + precip_scaled + time_cos_scaled + weekday,
                   #Run as a logistic regression
                   family = binomial(link = "logit"),
                   #Run with reduced dataset
                   data = df_encounters)

## Evaluate top model #1

#Diagnostic plots
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
}

## Create top model #2
{
#Model equation
top_model_2 <- glm(encounter_binary ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + garbage_scaled + precip_scaled + time_cos_scaled + weekday,
                                     #Run as a logistic regression
                                     family = binomial(link = "logit"),
                                     #Run with reduced dataset
                                     data = df_encounters)

## Evaluate top model #2

#Diagnostic plots
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
}

### Run model selection on retained variables without Lockdown Phase -----

## Create global model

#Model equation
noLP_model_conflicts_sighting <- glm(encounter_binary ~ garbage_scaled + picnic_scaled + d2den_scaled + distance2water_scaled + distance2ocean_scaled + precip_scaled + max_temp_scaled + prop_natural_cover_100_scaled + prop_open_100_scaled + prop_developed_100_scaled + time_cos_scaled + weekday + coyseason:d2den_scaled + garbage_scaled:picnic_scaled + distance2ocean_scaled:picnic_scaled + distance2ocean_scaled:garbage_scaled + weekday:picnic_scaled + weekday:time_cos_scaled + distance2ocean_scaled:time_cos_scaled,
                                     #Run as a logistic regression
                                     family = binomial(link = "logit"),
                                     #Run with reduced dataset
                                     data = df_encounters,
                                     #Set model to fail if NAs are detected. Mandatory setting for dredge function
                                     na.action = na.fail)

## Identify top models using BIC

#Compare all nested models
noLP_conflict_sighting_dredged <- dredge(noLP_model_conflicts_sighting, evaluate = TRUE, rank = "BIC")

#View model rankings
View(noLP_conflict_sighting_dredged)

### Run model selection with subset of data where coord precision is <= 100m -----

## Create dataset with only precise data

#Start with full dataset
df_precise_coords <- df_encounters_full %>%
  #Keep only reports with a confidence radius of less than 100m
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

#Set a seed for reproducibility
set.seed(123)

#Set parameters for cross validation
cv_control <- trainControl(method = "cv",
                           #Split data into 5 folds
                           number = 5,
                           #Compute class probabilities. Necessary for classification models
                           classProbs = TRUE,
                           #Set results summary to include desired elements
                           summaryFunction = twoClassSummary,
                           #Downsample the more prominent class (sightings) to reduce huge sample difference
                           sampling = "down")

#Define a place to store the ROC, sensitivity, and specificity data for model 1
df_model_1_results <- tibble(
  #Receiver Operating Characteristic
  ROC = rep(NA, 10),
  #Sensitivity
  Sens = rep(NA, 10),
  #Specificity
  Spec = rep(NA, 10)
)

## Loop for averaging results over 10 iterations
for (i in 1:10) {
  #Train the first model with cross-validation
  model_1 <- train(encounter_binary_fc ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + picnic_scaled + precip_scaled + time_cos_scaled + weekday,
                   #Use the reduced dataset
                   data = df_encounters, 
                   #Train the model as a GLM
                   method = "glm",
                   #Train the model with a binomial response
                   family = "binomial",
                   #Use the list of settings created above
                   trControl = cv_control,
                   #Choose form in which to receive results
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
  #Receiver Operating Characteristic
  ROC = rep(NA, 10),
  #Sensitivity
  Sens = rep(NA, 10),
  #Specificity
  Spec = rep(NA, 10)
)

## Loop for averaging results over 10 iterations
for (i in 1:10) {
  ## Train the second model with cross-validation
  model_2 <- train(encounter_binary_fc ~ d2den_scaled + distance2ocean_scaled + Lockdown_Phase + garbage_scaled + precip_scaled + time_cos_scaled + weekday,
                   #Use the reduced dataset
                   data = df_encounters, 
                   #Train the model as a GLM
                   method = "glm",
                   #Train the model with a binomial response
                   family = "binomial",
                   #Use the list of settings created above
                   trControl = cv_control,
                   #Choose form in which to receive results
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

### Chi-square test for differences between expected and observed victim characteristics -----

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
vic_act_results <- chisq.test(df_victim_activity, p = c(expected_walkers, expected_runners, expected_wheels))
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

### Post-hoc test for victim activities -----

## Calculate standardized residuals

vic_act_sr <- vic_act_results$stdres;vic_act_sr

## Evaluate significance using standardized residuals and Benjamini-Hochberg correction

p_vals <- 2 * (1 - pnorm(abs(vic_act_sr)))

p_adj <- p.adjust(p_vals, method = "BH");p_adj

### Figures from publication -----

## Plot confidence intervals
{
  # List of variables in each model
  Variables_model_1 <- c("Intercept", "Distance from den", "Distance from ocean",
                         "Lockdown phase 2: Social\ngatherings prohibited",
                         "Lockdown phase 3: Social\ngatherings limited",
                         "Distance from picnic area", "Daily precipitation",
                         "Time of day", "Weekday")
  
  Variables_model_2 <- c("Intercept", "Distance from den", "Distance from ocean",
                         "Lockdown phase 2: Social\ngatherings prohibited",
                         "Lockdown phase 3: Social\ngatherings limited",
                         "Distance from garbage bin", "Daily precipitation",
                         "Time of day", "Weekday")
  
  # List of coefficients
  coefficients_1 <- c(-6.4588, -0.9734, -0.6069, 3.8010, 3.2220, 0.6079, 0.4698, -0.8824, 1.4404)
  coefficients_2 <- c(-6.2847, -0.7469, -0.4808, 3.7972, 3.1186, 0.4555, 0.4352, -0.8396, 1.2830)
  
  # Desired plotting order
  var_levels <- c("Daily precipitation",
                  "Distance from garbage bin",
                  "Distance from ocean",
                  "Distance from picnic area",
                  "Time of day",
                  "Distance from den",
                  "Weekday",
                  "Lockdown phase 3: Social\ngatherings limited",
                  "Lockdown phase 2: Social\ngatherings prohibited",
                  "Intercept")
  
  # Build tidy CI table and plot
  p_conf_ints <- bind_rows(
    as_tibble(confint(top_model_1)) %>%
      cbind(Variable = Variables_model_1,
            Coefficient = coefficients_1) %>%
      pivot_longer(`2.5 %`:`97.5 %`,
                   names_to = "Level",
                   values_to = "CL") %>%
      mutate(Model = "Model 1"),
    as_tibble(confint(top_model_2)) %>%
      cbind(Variable = Variables_model_2,
            Coefficient = coefficients_2) %>%
      pivot_longer(`2.5 %`:`97.5 %`,
                   names_to = "Level",
                   values_to = "CL") %>%
      mutate(Model = "Model 2")
  ) %>%
    mutate(
      Variable = factor(Variable, levels = var_levels),
      y_base   = as.numeric(Variable),
      y        = y_base + if_else(Model == "Model 1", -0.1, 0.1)
    ) %>%
    group_by(Model, Variable, y, y_base) %>%
    summarise(
      Lower = min(CL),
      Upper = max(CL),
      Coefficient = unique(Coefficient),
      .groups = "drop"
    ) %>%
    ggplot(aes(y = y, col = Model)) +
    geom_segment(aes(x = Lower, xend = Upper, yend = y),
                 linewidth = 2) +
    geom_point(aes(x = Coefficient),
               size = 5, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = c(6.5, 9.5)) +
    geom_text(
      data = tibble(
        x = c(-7, -7, -7),
        y = c(6.3, 9.3, 10.5),
        label = c("Continuous variables",
                  "Categorical variables",
                  "Intercept")
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 5,
      hjust = 0
    ) +
    scale_y_continuous(
      breaks = unique(as.numeric(factor(var_levels, levels = var_levels))),
      labels = var_levels
    ) +
    scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
    labs(x = "95% Confidence Intervals", y = "") +
    theme_classic() +
    theme(
      legend.position = "right",
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_blank())
  p_conf_ints
  
  #Save plot
  #ggsave(filename = "Plots/conf_ints.tiff", p_conf_ints, dpi = "retina")
}

## Multi-plot for logistic regression variables
{ 

## Lockdown phase bar plot

p_lockdown <- df_encounters_full %>%
    #Keep only reports from study period
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
         y = "Reports", fill = "Report type") +
    #Set y-axis breaks
    scale_y_continuous(breaks = c(0,75, 150)) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))

## Coyote season

# Specify order for x-axis
season_order <- c('Breeding', 'Pup-rearing', 'Dispersal')

p_season <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Phenological season", y = "Reports", fill = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Week phase
p_weekphase <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Weekday", y = "Reports", fill = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,150,300))

## Distance from den
p_den <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Distance from nearest den (m)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Precipitation
p_precip <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Daily precipitation (mm)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Max temp
p_maxtemp <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Daily maximum temperature (˚C)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.3,0.6))

## Time of day
p_time <- df_encounters_full %>%
  #Keep only reports from study period
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
            y = c(0.1, 0.27, 0.08, 0.1),
            label=c("0200 hours","0800 hours","2000 hours","1400 hours"),
            size = 3) +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,0.4,0.8))

## Distance from ocean
p_ocean <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Distance from the ocean (m)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Distance from water
p_water <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Distance from water (m)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Distance to nearest garbage bin
p_garbage <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Distance from nearest garbage bin (m)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Distance to nearest picnic area
p_picnic <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Distance from nearest picnic area (m)", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Proportion of 100m buffer - natural cover
p_natural <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Proportion natural area in 100m buffer", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Proportion of 100m buffer - open
p_open <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Proportion open area in 100m buffer", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  #Set y-axis breaks
  scale_y_continuous(breaks = c(0,3,6,9))

## Proportion of 100m buffer - developed
p_developed <- df_encounters_full %>%
  #Keep only reports from study period
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
  labs(x = "Proportion developed area in 100m buffer", y = "Density", col = "Encounter") +
  #Set text size
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## Combine all plots into one multi-panel plot

#Include all individual plots
multi_plot<- ggarrange(p_lockdown, p_weekphase, p_season, p_den, p_ocean, p_time, p_precip, p_garbage, p_picnic, p_water, p_natural, p_developed, p_open, p_maxtemp,
                       #Define plot labels
                       labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N"), 
                       #Arrange plot positions
                       ncol = 3, nrow = 7,
                       #Include legend
                       common.legend = T,
                       #Fix position of labels
                       vjust = -0.5) 

#Add titles and labels to the multi-panel graph
multi_plot <- annotate_figure(multi_plot); multi_plot

#Save plot
#ggsave(filename = "Plots/SP_rawdata_multiplot.tiff", multi_plot, dpi = "retina")
}

## Multi-plot for logistic regression model predictions
{
  ## Pandemic lockdown phase
  vp_lockdown <- visreg(top_model_1, "Lockdown_Phase", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Pandemic lockdown phase", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Distance to nearest den
  vp_den <- visreg(top_model_1, "d2den_scaled", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Distance to nearest den", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Distance to ocean
  vp_ocean <- visreg(top_model_1, "distance2ocean_scaled", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Distance to ocean", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Time of day
  vp_time <- visreg(top_model_1, "time_cos_scaled", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Time of day", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Precipitation
  vp_precip <- visreg(top_model_1, "precip_scaled", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Daily precipitation", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Distance to nearest garbage bin
  vp_garbage <- visreg(top_model_2, "garbage_scaled", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Distance to nearest garbage bin", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Weekday
  vp_weekday <- visreg(top_model_1, "weekday", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Weekday", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Distance to nearest picnic area
  vp_picnic <- visreg(top_model_1, "picnic_scaled", scale = "response", gg = TRUE) +
    theme_classic() +
    labs(x = "Distance to nearest picnic area", y = "Probability\nof aggression") +
    ylim(0,1) +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
  
  ## Combine all plots into one multi-panel plot
  
  #Include all individual plots
  vp_multi_plot<- ggarrange(vp_lockdown, vp_den, vp_ocean, vp_time, vp_precip, vp_garbage, vp_weekday, vp_picnic,
                         #Define plot labels
                         labels = c("A", "B", "C", "D", "E", "F", "G", "H"), 
                         #Arrange plot positions
                         ncol = 2, nrow = 4,
                         #Fix position of labels
                         vjust = 1) 
  
  #Add titles and labels to the multi-panel graph
  vp_multi_plot <- annotate_figure(vp_multi_plot); vp_multi_plot
  
  #Save plot
  #ggsave(filename = "Plots/SP_visreg_multiplot.tiff", vp_multi_plot, dpi = "retina")
}

## Plot interaction density over time
{
#Create data frame for seasons
  df_seasons = tibble(
    #Start date of each season
    xmin = as.Date(c("2019-01-01", "2019-05-01", "2019-09-01", "2020-01-01", "2020-05-01", "2020-09-01", "2021-01-01", "2021-05-01")),
    #End date of each season
    xmax = as.Date(c("2019-05-01", "2019-09-01", "2020-01-01", "2020-05-01", "2020-09-01", "2021-01-01", "2021-05-01", "2021-09-01")),
    #Label for each season
    season_unordered = c("Breeding", "Pup-Rearing", "Dispersal", "Breeding", "Pup-Rearing", "Dispersal", "Breeding", "Pup-Rearing"),
    #Order season labels
    Season = factor(season_unordered, levels = c("Breeding", "Pup-Rearing", "Dispersal"))
  )

#Create plot
p_interactions_density <- df_encounters_full %>%
  #Fix name of non-contact aggressive encounters for display purposes
  mutate(encounter = ifelse(encounter == "Aggression to Human",
                            "Non-contact aggression",
                            encounter)) %>%
  #Create plot and set variables
  ggplot(aes(x=date)) +
  #Add rectangles to the bottom of the plot to represent phenological seasons
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = -0.0009, fill = Season),
            #Resolves error
            inherit.aes = FALSE, 
            #Get data for rectangles from the df created above
            data = df_seasons) +
  #Manually choose colours for seasons
  scale_fill_manual(values = c("Breeding" = "thistle",
                                "Pup-Rearing" = "orchid",
                                "Dispersal" = "purple")) +
    #Create new fill legend for density plot
    ggnewscale::new_scale("fill")+
    #Format as density plot with mostly transparent fill
    geom_density(aes(fill = encounter), alpha = 0.3) +
    #Add points to represent actual events
    geom_jitter(aes(x = date, y = 0, col = encounter), height = 0.0008, width = 0) +
    #Remove gridlines
    theme_classic() +
    #Add vertical dotted lines at the beginning of restriction phases two and three
    geom_vline(xintercept = as.numeric(as.Date(c("2020-12-02", "2021-05-26"))), linetype = 'dotted') +
    #Add a vertical dashed line at the beginning of restriction phase one
    geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), linetype = 'dashed') +
    #Add labels to the inserted vertical lines
    geom_label(aes(x=x,y=y,label=label),
               #Resolves error
               inherit.aes = FALSE,
               #Specifications for location and text of each label
               data = data.frame(x = as.Date(c("2020-03-11","2020-10-30","2021-06-20")),
                                 y = c(0.009, 0.009, 0.00875),
                                 label=c("Start of pandemic\nrestrictions","Indoor gatherings\nprohibited","Indoor gatherings\npermitted with\nrestrictions")),
               #Set text size
               size = 8) +
    #Set axis and legend labels
    labs(x = "Date", y = "Density of reports", fill = "Report type", color = "Report type") +
    #Set font size
    theme(text = element_text(size = 30),
          axis.text = element_text(size = 30))

  #View plot
  p_interactions_density
  
  #Save plot
  #ggsave(filename = "Plots/time_densityplot.tiff", p_interactions_density, dpi = "retina")
}

## Human victim demographics multi-plot
{
  ## Activity
  
  p_activity <- victim_activity_groups %>%
    #Rotate dataframe to long format
    pivot_longer(cols = c(1:3), names_to = "Activity", values_to = "Observed") %>%
    #Add column for expected totals
    mutate(Expected = c(expected_runners*sum(victim_activity_groups), expected_walkers*sum(victim_activity_groups), expected_wheels*sum(victim_activity_groups))) %>%
    #Rotate dataframe to combine observed and expected data into the same column
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    #Create plot and define axes
    ggplot(aes(x = Activity, y = Frequency, fill = Category)) +
    #Format the plot as a grouped bar plot
    geom_bar(position = "dodge", stat = "identity") +
    #Remove gridlines
    theme_classic() +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  ## Group size
  p_group_size <- victim_group_size %>%
    #Rotate dataframe to long format
    pivot_longer(cols = c(1:2), names_to = "Group Size", values_to = "Observed") %>%
    #Add column for expected totals
    mutate(Expected = c(df_human_group_size$Individual/df_human_group_size$total*sum(victim_group_size), df_human_group_size$Group/df_human_group_size$total*sum(victim_group_size))) %>%
    #Rotate dataframe to combine observed and expected data into the same column
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    #Create plot and define axes
    ggplot(aes(x = `Group Size`, y = Frequency, fill = Category)) +
    #Format the plot as a grouped bar plot
    geom_bar(position = "dodge", stat = "identity") +
    #Remove gridlines
    theme_classic() +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  ## Age
  p_demo <- victim_demo_groups %>%
    #Rotate dataframe to long format
    pivot_longer(cols = c(1:3), names_to = "Gender/Age Group", values_to = "Observed") %>%
    #Add column for expected totals
    mutate(Expected = c(expected_adult_males*sum(victim_demo_groups), expected_adult_females*sum(victim_demo_groups), expected_children*sum(victim_demo_groups))) %>%
    #Rotate dataframe to combine observed and expected data into the same column
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    #Change group names from codes to full words so they are more clear on the plot
    mutate(`Gender/Age Group` = case_when(
      `Gender/Age Group` == "MA" ~ "Male adults",
      `Gender/Age Group` == "FA" ~ "Female adults",
      `Gender/Age Group` == "C" ~ "Children",
      TRUE ~ NA_character_
    )) %>%
    #Create plot and define axes
    ggplot(aes(x = `Gender/Age Group`, y = Frequency, fill = Category)) +
    #Format the plot as a grouped bar plot
    geom_bar(position = "dodge", stat = "identity") +
    #Remove gridlines
    theme_classic() +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  ## Dog Presence
  p_dog <- as_tibble(victim_dogs) %>%
    #Rename variables to make them more clear on the plot labels
    rename(Observed = value) %>%
    #Add columns for expected totals and categories
    mutate(Expected = c(expected_NoDogs*sum(victim_dogs), expected_dogs*sum(victim_dogs)),
           Dogs = c("Absent", "Present")) %>%
    #Rotate dataframe to combine observed and expected data into the same column
    pivot_longer(cols = c(Observed, Expected), names_to = "Category", values_to = "Frequency") %>%
    #Create plot and define axes
    ggplot(aes(x = Dogs, y = Frequency, fill = Category)) +
    #Format the plot as a grouped bar plot
    geom_bar(position = "dodge", stat = "identity") +
    #Remove gridlines
    theme_classic() +
    #Set text size
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))

  ## Combine all plots into one multi-panel plot
  
  #Include all individual plots
  victim_multi_plot <- ggarrange(p_demo, p_activity, p_group_size, p_dog,
                         #Define plot labels
                         labels = c("A", "B", "C", "D"),
                         #Arrange plot positions
                         ncol = 2, nrow = 2,
                         #Include legend
                         common.legend = T,
                         #Fix position of labels
                         vjust = 0.5)
  
  #Add titles and labels to the multi-panel graph
  victim_multi_plot <- annotate_figure(victim_multi_plot) +
    #make the background white
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA)
    )
  
  
  #View plot
  victim_multi_plot
  
  #Save plot
  #ggsave(filename = "Plots/SP_victims_multiplot.tiff", victim_multi_plot, dpi = "retina")
}

## Human activity (2024) multi-plot for appendix
{
  human_multiplot <- df_human_activity_blocks %>%
    #Capitalize site category names
    mutate(site_category = case_when(
      site_category == "developed" ~ "Developed",
      site_category == "forest" ~ "Forest",
      site_category == "lakeside" ~ "Lakeside",
      site_category == "seawall" ~ "Seawall",
      TRUE ~ NA_character_
    ),
    #Change values of 0 to a number (#): 0<#<1 to ensure they are shown when the y-axis is transformed
    total_humans_adjusted = ifelse(total_humans_adjusted == 0, 0.9, total_humans_adjusted)) %>%
    #Create the plot and set the axes
    ggplot(aes(x = start_time, y = total_humans_adjusted, col = weather)) +
    #Format the plot as a scatter plot with colour-coded points
    geom_point(aes(col = weather)) +
    #Add a smoothed 'loess' line of fit for each weather group
    geom_smooth(method = "loess", se = FALSE, span = 3) +
    #Split the plot into panels by site category
    facet_wrap(~site_category) +
    #Transform the y-axis by log10 to show more variation in less frequented site categories
    scale_y_log10() +
    #Make background black and white with faint gridlines
    theme_bw() +
    #Label axes
    labs(x = "Time of day", y = "People per hour", col = "Weather") +
    #Set font sizes
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          strip.text = element_text(size = 14))
  
  #View plot
  human_multiplot
  
  #Save plot
  #ggsave(filename = "Plots/SP_human_multiplot.png", human_multiplot, dpi = "retina")
}