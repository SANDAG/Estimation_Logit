# Setup ----
## 1) Set Working Directory ----
setwd("C:/Users/jcast/San Diego Association of Governments/Data Science - Documents/Economics/FY2024_25 Projects/Misc Projects/1231_ATC/4_Outputs/Handoff to WSP")

## 2) Clear Environment ----
rm(list = ls())

## 3) Install and Load Packages ----
# Function to install and load required packages.
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    if (!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# List of packages required for data manipulation and database connection. Add or remove packages as needed. 
packages <- c("tidyverse", "dplyr", 
              "zoo", "conflicted", 
              "mlogit", "readr", 
              "writexl","gtools", 
              "geosphere", "broom", 
              "knitr", "kableExtra",
              "purrr", "randomForest")

# Apply the package testing function to each package.
lapply(packages, pkgTest)


#Shortcut to reset directory
goaway <- "dir.create(tempdir())"

## 4) Remove conlicts ----
# Set preferred package for select()
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Data Cleaning ----

## Read in and Clean Data ----
# Dataset was provided by TAM team on June 18, 2025

atc_expanded_survey <- read_csv("data_model_output_simplified_taz.csv") 

## Clean Dataframe ----

# This code categorizes mode into simpler categories based off respondent choices 
data <- atc_expanded_survey %>%
  mutate(custom_access_mode = case_when( 
    parking_location_label %in% c("ADMIN_BUILDING_LOT_2417_MCCAIN_ROAD",
                                  "TERM1_PARKING_PLAZA",
                                  "TERM2_PARKING_PLAZA",
                                  "TERM2_CURBSIDE_VALET") ~ "Parked - On-Site",
    parking_location_other %in% c("San Diego airport parking") ~ "Parked - On-Site",
    parking_location_other %in% c("Aladdin", "Wally") ~ "Parked - Off-Site",
    parking_location_label %in%  c("OFF_AIRPORT_PARKING",
                                   "EMPLOYEE_LOT_3665_ADMIRAL_BOLAND_WAY", 
                                   "Employee parking") ~ "Parked - Off-Site",
    main_mode_label %in% c("DROVE_ALONE_AND_PARKED",
                           "GET_IN_PARKED_VEHICLE_AND_DRIVE_ALONE",
                           "GET_IN_PARKED_VEHICLE_AND_RIDE_WITH_OTHER_TRAVELERS",
                           "DROVE_WITH_OTHERS_AND_PARKED",
                           "GET_IN_PARKED_VEHICLE_AND_DRIVE_WITH_OTHERS",
                           "RODE_WITH_OTHER_TRAVELERS_AND_PARKED") & 
      parking_location_label %in% c("nan", "REFUSED", "OTHER") ~ "Parked - Unknown",
    main_mode_label %in% c("DROPPED_OFF_BY_FAMILY_FRIEND",
                           "PICKED_UP_BY_FAMILY_FRIEND") ~ "Pickup/ Drop Off",
    main_mode_label %in% c("RENTAL_CAR_PARKED",
                           "RENTAL_CAR_DROPPED_OFF",
                           "RENTAL_CAR_PICKED_UP",
                           "RENTAL_CAR_GET_IN_PARKED") ~ "Rental Car",
    main_mode_label %in% c("HOTEL_SHUTTLE_VAN",
                           "CHARTERED_TOUR_BUS",
                           "OTHER_SHARED_VAN",
                           "EMPLOYEE_SHUTTLE") ~ "Shuttle/ Vanpool",
    main_mode_label %in% c("TAXI") ~ "Taxi",
    main_mode_label %in% c("UBER_LYFT",
                           "CAR_SERVICE_BLACK_LIMO") ~ "TNC",
    main_mode_label %in% c("WALK",
                           "BICYCLE_PERSONAL_ELECTRIC",
                           "BICYCLE_NON_ELECTRIC_BIKESHARE",
                           "WHEELCHAIR_OR_MOBILITY_DEVICE",
                           "BICYCLE_ELECTRIC_BIKESHARE",
                           "BICYCLE_PERSONAL_NON_ELECTRIC") ~ "Active Transportation",
    main_mode_label %in% c("OTHER_PUBLIC_TRANSIT",
                           "MTS_ROUTE_992",
                           "AIRPORT_FLYER_SHUTTLE") ~ "Public Transit",
    main_mode_label %in% c("OTHER", "nan", "REFUSED_NO_ANSWER") ~ "Unknown",
    TRUE ~ NA_character_)) %>%
  mutate(resident_visitor_label = case_when(
    market_segment %in% c("VISITOR_NON_BUSINESS", "VISITOR_BUSINESS") ~ "Visitor",
    market_segment %in% c("RESIDENT_BUSINESS", "RESIDENT_NON_BUSINESS") ~ "Resident",
    TRUE ~ "Employee"),
    trip_purpose_label = case_when(
      market_segment %in% c("Resident - Business", "Visitor - Business") ~ "Business",
      market_segment %in% c("Resident - Personal", "Visitor - Personal") ~ "Personal",
      TRUE ~ "Employee")) %>%
  filter(!custom_access_mode %in% c("Unknown", "Parked - Unknown"))

with(data, table(custom_access_mode, mode_group, useNA = "ifany"))


# Three observations have a TNC wait time of 2024 minutes. Significantly skews distribution and well outside expected range.  
# Replace with average wait time. 
data %>% filter(tnc_wait != 2024) %>% summarise(mean(tnc_wait))
data <- data %>% mutate(tnc_wait = if_else(tnc_wait == 2024, 5.32, tnc_wait)) 
max(data$tnc_wait) #QA Check


## Fill missing Income observations using Random Forest ----
# Several respondents did not report income. To maintain those observations we impute income 
# using a random forest model based off demographic characteristics 

### Income bins and midpoint estimates ----
income_bins <- tibble(
  household_income_label = c(
    "LESS_THAN_15K",
    "BETWEEN_15K_20K",
    "BETWEEN_20K_25K",
    "BETWEEN_25K_30K",
    "BETWEEN_30K_35K",
    "BETWEEN_35K_40K",
    "BETWEEN_40K_45K",
    "BETWEEN_45K_50K",
    "BETWEEN_50K_60K",
    "BETWEEN_60K_75K",
    "BETWEEN_75K_100K",
    "BETWEEN_100K_150K",
    "BETWEEN_150_199K",
    "BETWEEN_200_299K",
    "MORE_THAN_300K",
    "MORE_THAN_150K"),
  household_income_numeric = c(
    15000,
    mean(c(15000, 20000)),
    mean(c(20000, 25000)),
    mean(c(25000, 30000)),
    mean(c(30000, 35000)),
    mean(c(35000, 40000)),
    mean(c(40000, 45000)),
    mean(c(45000, 50000)),
    mean(c(50000, 60000)),
    mean(c(60000, 75000)),
    mean(c(75000, 100000)),
    mean(c(100000, 150000)),
    mean(c(150000, 199000)),
    mean(c(200000, 299000)),
    300000,
    150000))

### Train data: valid income + age + gender ----
train_data <- data %>%
  filter(!is.na(household_income_label),
         household_income_label != "PREFER_NOT_TO_SAY",
         !is.na(age_label),
         !is.na(gender_label)) %>%
  left_join(income_bins, by = "household_income_label") %>%
  select(household_income_numeric, age_label, gender_label, market_segment)

### Train random forest ----
set.seed(42)
rf_model <- randomForest(
  household_income_numeric ~ age_label + gender_label + market_segment,
  data = train_data,
  ntree = 100)

### Predict for missing/prefer-not-to-say rows ----
missing_income_rows <- data %>%
  mutate(row_id = row_number()) %>%
  filter(is.na(household_income_label) |
           household_income_label == "PREFER_NOT_TO_SAY") %>%
  filter(!is.na(age_label) & !is.na(gender_label)) %>%
  select(row_id, age_label, gender_label, market_segment)

predicted_numeric <- predict(rf_model, newdata = missing_income_rows)

### Map numeric prediction to closest income bin label ----
get_nearest_label <- function(x) {
  income_bins$household_income_label[which.min(abs(income_bins$household_income_numeric - x))]
}

predicted_labels <- sapply(predicted_numeric, get_nearest_label)

### Replace values in the main dataset ----
data$household_income_label[missing_income_rows$row_id] <- predicted_labels

### Verify it worked ----
data %>%
  count(household_income_label, sort = TRUE)

### Regroup Income Labels ----
data %>%
  count(custom_access_mode, household_income_label) %>%
  print(n=Inf)

data <- data %>% 
  mutate(income_group = case_when(
    household_income_label %in% c("LESS_THAN_15K",
                                  "BETWEEN_15K_20K",
                                  "BETWEEN_20K_25K",
                                  "BETWEEN_25K_30K",
                                  "BETWEEN_30K_35K") ~ "Under 35k",
    household_income_label %in% c("BETWEEN_35K_40K",
                                  "BETWEEN_40K_45K",
                                  "BETWEEN_45K_50K",
                                  "BETWEEN_50K_60K") ~ "35k–60k",
    household_income_label %in% c("BETWEEN_60K_75K",
                                  "BETWEEN_75K_100K") ~ "60k–100k",
    household_income_label == "BETWEEN_100K_150K" ~ "100k–150k",
    household_income_label %in% c("BETWEEN_150_199K",
                                  "BETWEEN_200_299K",
                                  "MORE_THAN_150K",
                                  "MORE_THAN_300K") ~ "Over 150k",
    TRUE ~ NA_character_))



## 4) Expand Survey Using Weights ----

# The survey is expanded using a probabilistic expansion
# Probabilistic expansion ensures weighted observations are correctly represented by sampling with replacement. 
# Each row is selected in proportion to its weight, avoiding rounding distortions while preserving the intended distribution.


expanded_data <- data[sample(1:nrow(data), size = round(sum(data$weight)), replace = TRUE, prob = data$weight), ] %>%
  mutate(new_id = row_number()) %>%
  select(new_id,everything())

visitor_modes <- c("Pickup/ Drop Off", "Public Transit", "Rental Car", "Shuttle/ Vanpool", "Taxi", "TNC")
resident_modes <- c("Parked - On-Site", "Parked - Off-Site", "Pickup/ Drop Off", "Public Transit", "Taxi", "TNC")

expanded_data <- expanded_data %>%
  distinct(new_id, custom_access_mode) %>%  # Get unique IDs and access modes
  tidyr::crossing(mode_options = unique(data$custom_access_mode)) %>%  # Expand so each ID gets all modes
  mutate(choice = ifelse(custom_access_mode == mode_options, TRUE, FALSE)) %>%  # Mark chosen mode
  right_join(expanded_data) %>%
  mutate(
    # In-Vehicle Time
    ivtt = case_when(
      mode_options %in% c("Pickup/ Drop Off", "Taxi", "TNC", "Rental Car", 
                          "Shuttle/ Vanpool", "Parked - On-Site", "Parked - Off-Site") ~ auto_time,
      mode_options == "Public Transit" ~ transit_totalivtt,
      TRUE ~ NA_real_),
    
    # Out-of-Vehicle Time
    ovtt = case_when(
      mode_options == "Pickup/ Drop Off"   ~ 0,
      mode_options == "Taxi"               ~ taxi_wait,
      mode_options == "TNC"                ~ tnc_wait,
      mode_options == "Rental Car"         ~ rental_additional_time,
      mode_options == "Shuttle/ Vanpool"   ~ shuttle_additional_time,
      mode_options == "Parked - On-Site"   ~ onsite_parking_additional_time,
      mode_options == "Parked - Off-Site"  ~ offsite_parking_additional_time,
      mode_options == "Public Transit"     ~ transit_acc + transit_firstwait + transit_xferwait + transit_egr,
      TRUE ~ NA_real_),
    
    # expenditures
    expend = case_when(
      mode_options == "Pickup/ Drop Off"   ~ 0,
      mode_options == "Taxi"               ~ taxi_fare,
      mode_options == "TNC"                ~ tnc_fare,
      mode_options == "Rental Car"         ~ 0,
      mode_options == "Shuttle/ Vanpool"   ~ 0,
      mode_options == "Parked - On-Site"   ~ onsite_parking_cost,
      mode_options == "Parked - Off-Site"  ~ offsite_parking_cost,
      mode_options == "Public Transit"     ~ transit_fare,
      TRUE ~ NA_real_),
    # Log Transforms
    log_ivtt = log1p(ivtt),
    log_ovtt = log1p(ovtt),
    log_expend = log1p(expend),
    total_time = ivtt + ovtt, 
    log_total_time = log1p(total_time)) %>%
  mutate(
    market_segment = tolower(market_segment),
    market_group = case_when(
      market_segment %in% c("resident_business", "resident_non_business") ~ "resident",
      market_segment %in% c("visitor_business", "visitor_non_business") ~ "visitor"),
    purpose = if_else(market_segment %in%c("resident_business",
                                           "visitor_business"), "business", "non_business")) %>%
  filter(market_segment != "employee",
         mode_options != "Active Transportation") %>%
  filter(
    (market_group == "resident" & custom_access_mode %in% c("Parked - On-Site", "Parked - Off-Site", "Pickup/ Drop Off", "Public Transit", "Taxi", "TNC")) |
      (market_group == "visitor" & custom_access_mode %in% c("Pickup/ Drop Off", "Public Transit", "Rental Car", "Shuttle/ Vanpool", "Taxi", "TNC"))) %>%
  select(new_id, market_segment, market_group, purpose, mode_options, custom_access_mode,
         ivtt, ovtt, expend, total_time,
         log_ivtt, log_ovtt, log_expend, log_total_time,
         income_group) %>%
  mutate(income_group = factor(income_group, levels = c("Over 150k", "Under 35k", "35k–60k", "60k–100k", "100k–150k")),
         purpose_binary = if_else(market_group == "business", 1, 0))

# Set reference level for purpose
expanded_data$purpose <- relevel(factor(expanded_data$purpose), ref = "non_business")

# Data Analysis ----

## Define Allowed Modes by Segment ----

visitor_modes <- c("Pickup/ Drop Off", "Public Transit", "Rental Car", "Shuttle/ Vanpool", "Taxi", "TNC")
resident_modes <- c("Parked - On-Site", "Parked - Off-Site", "Pickup/ Drop Off", "Public Transit", "Taxi", "TNC")

## Create Function to Prepare dfidx Object ----
prep_logit_df <- function(group, allowed_modes) {
  expanded_data %>%
    filter(market_group == group, custom_access_mode %in% allowed_modes) %>%
    select(new_id, custom_access_mode) %>%
    unique() %>%
    dfidx(choice = "custom_access_mode", shape = "wide", chid.var = "new_id") %>%
    mutate(mode_options = idx$id2) %>%
    left_join(
      expanded_data %>% dplyr::rename(mode_chosen = custom_access_mode),
      by = c("new_id", "mode_options")
    ) %>%
    { class(.) <- c("dfidx", "data.frame"); . }   # <-- strip tbl_ classes LAST
}

# Create dfidx Objects
resident_dfidx <- prep_logit_df("resident", resident_modes)
visitor_dfidx  <- prep_logit_df("visitor", visitor_modes)

## Define Nesting Structures ----

resident_nest <- list(
  "Parked" = c("Parked - On-Site", "Parked - Off-Site"),
  "Rideshare" = c("Taxi", "TNC"),
  "Other" = c("Public Transit", "Pickup/ Drop Off"))


visitor_nest <- list(
  "Other" = c("Pickup/ Drop Off", "Rental Car", "Shuttle/ Vanpool", "Public Transit"),
  "Rideshare" = c("Taxi", "TNC"))

# 2-Model Specification:  ----

## Base Model ----

# Resident Nest 3
resident_model <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | purpose,
  data = resident_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = resident_nest, 
  constPar = c("iv:Other" = 1))

summary(resident_model)

# Visitor Nest (pickup vs transportation)
visitor_model <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | purpose,
  data = visitor_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = visitor_nest,
  constPar = c("iv:Other" = 1))

# Named list of income models
income_models_spec <- list(
  "Resident Model (Purpose Only)" = resident_model,
  "Visitor Model (Purpose Only)"  = visitor_model)

# Generate coefficient table
income_coef_df_spec <- imap_dfr(income_models_spec, function(model, name) {
  tidy(model) %>%
    mutate(
      Model = name,
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE ~ ""
      ),
      coef_display = paste0(round(estimate, 3), signif)
    ) %>%
    select(Variable = term, Model, coef_display)
})

# Pivot wide
income_coef_table_spec <- income_coef_df_spec %>%
  pivot_wider(names_from = Model, values_from = coef_display)

# Display table
income_coef_table_spec %>%
  knitr::kable(caption = "Nested Logit Coefficients (2-Model Specification)") %>%
  kableExtra::kable_styling(full_width = FALSE)


## Add Income ----

resident_model_income <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | purpose + income_group,
  data = resident_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = resident_nest,
  constPar = c("iv:Other" = 1))

visitor_model_income <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | purpose + income_group,
  data = visitor_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = visitor_nest,
  constPar = c("iv:Other" = 1))

# Named list of income models
income_models_2spec <- list(
  "Resident Model (Income + Purpose)" = resident_model_income,
  "Visitor Model (Income + Purpose)"  = visitor_model_income)

# Generate coefficient table
income_coef_df_2spec <- imap_dfr(income_models_2spec, function(model, name) {
  tidy(model) %>%
    mutate(
      Model = name,
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE ~ ""
      ),
      coef_display = paste0(round(estimate, 3), signif)
    ) %>%
    select(Variable = term, Model, coef_display)
})

# Pivot wide
income_coef_table_2spec <- income_coef_df_2spec %>%
  pivot_wider(names_from = Model, values_from = coef_display)

# Display table
income_coef_table_2spec %>%
  knitr::kable(caption = "Nested Logit Coefficients with Income as ISC (2-Model Specification)") %>%
  kableExtra::kable_styling(full_width = FALSE)

## 2-Model Specification: Add Interaction ----

resident_model_purpose_ovtt <- mlogit(
  custom_access_mode ~ log_ivtt:purpose + log_expend:purpose + log_ovtt:purpose,
  data = resident_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = resident_nest,
  constPar = c("iv:Other" = 1))

summary(resident_model_purpose_ovtt)

visitor_model_purpose_ovtt <- mlogit(
  custom_access_mode ~ log_ivtt:purpose + log_expend:purpose + log_ovtt:purpose,
  data = visitor_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = visitor_nest,
  constPar = c("iv:Other" = 1))

summary(visitor_model_purpose_ovtt)

# Define and tidy the two-model interaction specifications
interaction_models_2spec <- list(
  "Resident: ASC x Purpose" = resident_model_purpose_ovtt,
  "Visitor: ASC x Purpose"  = visitor_model_purpose_ovtt)

# Create coefficient table
coef_table_2spec <- map_dfr(names(interaction_models_2spec), function(name) {
  tidy(interaction_models_2spec[[name]]) %>%
    mutate(model = name)
}) %>%
  mutate(
    estimate = round(estimate, 3),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p.value, 3))),
    signif = case_when(
      p.value == "<0.001" ~ "***",
      as.numeric(p.value) < 0.01 ~ "**",
      as.numeric(p.value) < 0.05 ~ "*",
      as.numeric(p.value) < 0.1 ~ "†",
      TRUE ~ ""),
    estimate_sig = paste0(estimate, signif)) %>%
  select(term, model, estimate_sig) %>%
  pivot_wider(names_from = model, values_from = estimate_sig)

# Display the coefficient table
coef_table_2spec %>%
  kable(caption = "Interaction Effects: 2-Model Specification") %>%
  kable_styling(full_width = FALSE)

# 4 Model Specification ----

## Filter and Split Data ----
resident_business_data <- expanded_data %>%
  filter(market_group == "resident", purpose == "business", custom_access_mode %in% resident_modes)

resident_nonbusiness_data <- expanded_data %>%
  filter(market_group == "resident", purpose == "non_business", custom_access_mode %in% resident_modes)

visitor_business_data <- expanded_data %>%
  filter(market_group == "visitor", purpose == "business", custom_access_mode %in% visitor_modes)

visitor_nonbusiness_data <- expanded_data %>%
  filter(market_group == "visitor", purpose == "non_business", custom_access_mode %in% visitor_modes)

## Prep dfidx for each subset ----
prep_dfidx_by_purpose <- function(df, allowed_modes) {
  df %>%
    dplyr::select(new_id, custom_access_mode) %>%
    unique() %>%
    dfidx::dfidx(choice = "custom_access_mode", shape = "wide", chid.var = "new_id") %>%
    dplyr::mutate(mode_options = idx$id2) %>%
    dplyr::left_join(df %>% dplyr::rename(mode_chosen = custom_access_mode),
                     by = c("new_id", "mode_options")) %>%
    { class(.) <- c("dfidx", "data.frame"); . }   # strip tbl_ classes LAST
}

resident_dfidx_business    <- prep_dfidx_by_purpose(resident_business_data, resident_modes)
resident_dfidx_nonbusiness <- prep_dfidx_by_purpose(resident_nonbusiness_data, resident_modes)
visitor_dfidx_business     <- prep_dfidx_by_purpose(visitor_business_data, visitor_modes)
visitor_dfidx_nonbusiness  <- prep_dfidx_by_purpose(visitor_nonbusiness_data, visitor_modes)

## Estimate Nested Logit Models (using previously best-performing nests) ----
resident_model_business <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend,
  data = resident_dfidx_business,
  reflevel = "Pickup/ Drop Off",
  nests = resident_nest,
  constPar = c("iv:Other" = 1))

resident_model_nonbusiness <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend,
  data = resident_dfidx_nonbusiness,
  reflevel = "Pickup/ Drop Off",
  nests = resident_nest,
  constPar = c("iv:Other" = 1,
               "iv:Rideshare" = 1)) 

#Note: Rideshare IV coefficient in the Resident - Non-Business model is invalid (>1). Set IV manually to 1 to de-nest the group.

visitor_model_business <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend,
  data = visitor_dfidx_business,
  reflevel = "Pickup/ Drop Off",
  nests = visitor_nest,
  constPar = c("iv:Other" = 1))

visitor_model_nonbusiness <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend,
  data = visitor_dfidx_nonbusiness,
  reflevel = "Pickup/ Drop Off",
  nests = visitor_nest,
  constPar = c("iv:Other" = 1))


## Coefficient Table ----
# Define custom order: intercepts first, then variables, then inclusive values
ordered_terms <- c(
  # Intercepts
  "(Intercept):Parked - Off-Site",
  "(Intercept):Parked - On-Site",
  "(Intercept):Public Transit",
  "(Intercept):Taxi",
  "(Intercept):TNC",
  "(Intercept):Rental Car",
  "(Intercept):Shuttle/ Vanpool",
  # ASCs
  "log_ivtt", "log_ovtt", "log_expend",
  # Nest inclusive values
  "iv:Parked", "iv:Rideshare"
)

## Summarize Model Performance ----
purpose_split_models <- list(
  "Resident Business" = resident_model_business,
  "Resident Non-Business" = resident_model_nonbusiness,
  "Visitor Business" = visitor_model_business,
  "Visitor Non-Business" = visitor_model_nonbusiness)

# Tidy and format
purpose_split_coef <- imap_dfr(purpose_split_models, function(model, name) {
  tidy(model) %>%
    mutate(
      Segment = name,
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE ~ ""),
      coef_display = paste0(round(estimate, 3), stars)) %>%
    select(Segment, term, coef_display)
}) %>%
  mutate(term = factor(term, levels = ordered_terms)) %>%
  arrange(term)

# Pivot wide
purpose_split_wide <- purpose_split_coef %>%
  pivot_wider(names_from = Segment, values_from = coef_display)

# Display table
purpose_split_wide %>%
  knitr::kable(caption = "Nested Logit Coefficients by Market Group and Purpose", align = "lcccc") %>%
  kableExtra::kable_styling(full_width = FALSE)

# 4-Model Specification: Add Income ----

resident_model_business_income <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | income_group,
  data = resident_dfidx_business,
  reflevel = "Pickup/ Drop Off")

summary(resident_model_business_income)

resident_model_nonbusiness_income <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | income_group,
  data = resident_dfidx_nonbusiness,
  reflevel = "Pickup/ Drop Off")

summary(resident_model_nonbusiness_income)

visitor_model_business_income <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | income_group,
  data = visitor_dfidx_business,
  reflevel = "Pickup/ Drop Off")

summary(visitor_model_business_income)

visitor_model_nonbusiness_income <- mlogit(
  custom_access_mode ~ log_ivtt + log_ovtt + log_expend | income_group,
  data = visitor_dfidx_nonbusiness,
  reflevel = "Pickup/ Drop Off")

summary(visitor_model_nonbusiness_income)

## Output Table ----

# Create named list of models
income_models <- list(
  "Resident Business"       = resident_model_business_income,
  "Resident Non-Business"   = resident_model_nonbusiness_income,
  "Visitor Business"        = visitor_model_business_income,
  "Visitor Non-Business"    = visitor_model_nonbusiness_income)

# Create coefficient table
income_coef_df <- imap_dfr(income_models, function(model, name) {
  tidy(model) %>%
    mutate(
      Model = name,
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ "†",
        TRUE ~ ""
      ),
      coef_display = paste0(round(estimate, 3), signif)
    ) %>%
    select(Variable = term, Model, coef_display)
})

# Pivot to wide format
income_coef_table <- income_coef_df %>%
  pivot_wider(names_from = Model, values_from = coef_display)

# Display table
income_coef_table %>%
  knitr::kable(caption = "Nested Logit Coefficients with Income as ISC (4-Model Specification)") %>%
  kableExtra::kable_styling(full_width = FALSE)

# Test Interaction Between Purpose and ovtt ----

resident_model_purpose_ovtt <- mlogit(
  custom_access_mode ~ log_ivtt:purpose + log_expend:purpose + log_ovtt:purpose,
  data = resident_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = resident_nest,
  constPar = c("iv:Other" = 1))

visitor_model_purpose_ovtt <- mlogit(
  custom_access_mode ~ log_ivtt:purpose + log_expend:purpose + log_ovtt:purpose,
  data = visitor_dfidx,
  reflevel = "Pickup/ Drop Off",
  nests = visitor_nest,
  constPar = c("iv:Other" = 1))

# Define and tidy the two-model interaction specifications
interaction_models_2spec <- list(
  "Resident: ASC x Purpose" = resident_model_purpose_ovtt,
  "Visitor: ASC x Purpose"  = visitor_model_purpose_ovtt)

# Create coefficient table
coef_table_2spec <- map_dfr(names(interaction_models_2spec), function(name) {
  tidy(interaction_models_2spec[[name]]) %>%
    mutate(model = name)
}) %>%
  mutate(
    estimate = round(estimate, 3),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p.value, 3))
    ),
    signif = case_when(
      p.value == "<0.001" ~ "***",
      as.numeric(p.value) < 0.01 ~ "**",
      as.numeric(p.value) < 0.05 ~ "*",
      as.numeric(p.value) < 0.1 ~ "†",
      TRUE ~ ""
    ),
    estimate_sig = paste0(estimate, signif)
  ) %>%
  select(term, model, estimate_sig) %>%
  pivot_wider(names_from = model, values_from = estimate_sig)

# Display the coefficient table
coef_table_2spec %>%
  kable(caption = "Interaction Effects: 2-Model Specification") %>%
  kable_styling(full_width = FALSE)

