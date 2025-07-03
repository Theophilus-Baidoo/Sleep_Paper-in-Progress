library(nhanesA)
library(dplyr)
library(purrr)

# List of all datasets you need
dataset_names <- c(
  "DEMO_J", "BMX_J", "BPX_J", "SMQ_J", "DIQ_J", "ALQ_J",
  "PAQ_J", "MCQ_J", "RHQ_J", "SLQ_J", "HUQ_J", "OHQ_J",
  "BGQ_J", "FSQ_J", "FSX_J", "BPQ_J", "DPQ_J", "ECQ_J",
  "DR1TOT_J", "OCQ_J", "TRIGLY_J", "HDL_J"  # Added based on your variable list
)

# Downloader with warning for missing datasets
download_df <- function(name) {
  df <- tryCatch(nhanes(name), error = function(e) {
    warning(paste("Couldn't download:", name)); NULL
  })
  df
}

# Download all datasets
raw_list <- map(dataset_names, download_df) %>%
  set_names(dataset_names)

# Keep only datasets with SEQN column
raw_list <- raw_list %>% keep(~ !is.null(.) && "SEQN" %in% names(.))

# Merge all datasets by SEQN
nhanes_all <- reduce(raw_list, full_join, by = "SEQN")

# Ensure SLQ050 is numeric and recode Refused/Don't Know to NA
#nhanes_all <- nhanes_all %>%
#  mutate(
 #   SLQ050 = as.numeric(SLQ050),
 #   SLQ050 = ifelse(SLQ050 %in% c(1, 2), SLQ050, NA),
 #   SLQ050 = factor(SLQ050, levels = c(1, 2), labels = c("Yes", "No"))
 # )



# Initial sample size
cat("Initial sample size (unique SEQN):", n_distinct(nhanes_all$SEQN), "\n")

# Function to print sample size at each stage
stage_n <- function(data, desc) {
  n <- n_distinct(data$SEQN)
  cat(sprintf("%-50s: %6d\n", desc, n))
}

# Step 0: Full dataset
data0 <- nhanes_all
stage_n(data0, "0. Full 2017–2018 sample")

# Step 1: Age ≥ 18
data1 <- data0 %>%
  filter(RIDAGEYR >= 18)
stage_n(data1, "1. Age ≥ 18")

# Step 2: Not pregnant women age 18–44
data2 <- data1 %>%
  filter(!(RIAGENDR == 2 & RIDAGEYR <= 44 & RIDEXPRG == 1))
stage_n(data2, "2. Not pregnant (RIDEXPRG)")

# Step 3: Complete SLQ050 and SLQ030 responses
data3 <- data2 %>%
  filter(SLQ050 %in% c("Yes", "No"),
         SLQ030 %in% c("Never",
                       "Rarely - 1-2 nights a week",
                       "Occasionally - 3-4 nights a week",
                       "Frequently - 5 or more nights a week"))
stage_n(data3, "3. Complete SLQ050/030 (exclude NA, Refused, Don't know)")

# Step 4: Non-missing BMI
data4 <- data3 %>%
  filter(!is.na(BMXBMI))
stage_n(data4, "4. Non-missing BMI")








# 4. Has sleep variables (SLQ050 and SLD012)
#data4 <- data3 %>%
#  filter(!is.na(SLQ050), !is.na(SLD012))
#stage_n(data4, "4. Non‑missing sleep (SLQ050 & SLD012)")



# View a sample of the cleaned data
#head(data4)
dim(data4)


library(dplyr)

# Subset to only needed variables
nhanes_data <- data4 %>%
  dplyr::select(
    # Demographics
    SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, DMDMARTL, WTINT2YR, SDMVPSU, SDMVSTRA, INDFMPIR, DMDEDUC2,
    
    # Occupation
    OCQ670,
    
    # Sleep
    SLD012, SLQ050, SLD013, SLQ030, SLQ040, SLQ120,
    
    # Alcohol & Smoking
    ALQ130, SMQ040,
    
    # Health Conditions
    DIQ010, BPQ020, DPQ020,
    
    # BMI
    BMXBMI,
    
    # Physical Activity
    PAQ605, PAQ620, PAQ635, PAQ650, PAQ665,
    
    # Diet
    DR1TSFAT, DR1TSUGR, DR1TKCAL,
    
    # Lipids
    LBXTR, LBDLDL, LBDHDD
  ) %>%
  rename(
    # Demographics
    age = RIDAGEYR,
    gender = RIAGENDR,
    race_ethnicity = RIDRETH1,
    marital_status = DMDMARTL,
    interview_weight = WTINT2YR,
    psu = SDMVPSU,
    stratum = SDMVSTRA,
    income_ratio = INDFMPIR,
    education = DMDEDUC2,
    
    # Occupation / Shift Work
    work_schedule = OCQ670,
    
    # Sleep
    sleep_weekday = SLD012,
    trouble_sleep = SLQ050,
    sleep_weekend = SLD013,
    snore = SLQ030,
    apnea_symptoms = SLQ040,
    daytime_sleepy = SLQ120,
    
    # Alcohol & Smoking
    alcohol_day = ALQ130,
    current_smoker = SMQ040,
    
    # Health Conditions
    diabetes = DIQ010,
    hypertension = BPQ020,
    depression = DPQ020,
    
    # BMI
    bmi = BMXBMI,
    
    # Physical Activity
    work_vigorous = PAQ605,
    work_moderate = PAQ620,
    walk_bike = PAQ635,
    leisure_vigorous = PAQ650,
    leisure_moderate = PAQ665,
    
    # Diet
    sat_fat = DR1TSFAT,
    sugar = DR1TSUGR,
    energy_kcal = DR1TKCAL,
    
    # Lipids
    triglycerides = LBXTR,
    ldl = LBDLDL,
    hdl = LBDHDD
  )

# Exclude implausible energy intakes:
#   Women (gender==2): keep 500–3,500 kcal/day  
#   Men   (gender==1): keep 800–4,200 kcal/day
library(dplyr)

nhanes_data <- nhanes_data %>%
  filter(
    is.na(energy_kcal) |  
      (gender == "Female" & between(energy_kcal, 500, 3500)) |   # Women: 500–3 500 kcal
      (gender == "Male"   & between(energy_kcal, 800, 4000))     # Men:   800–4 000 kcal per Willett
  )
stage_n(nhanes_data, "5. Implausible energy intakes")
# Check renamed column names
#colnames(nhanes_data)
dim(nhanes_data)
#summary(nhanes_data)

write.csv(nhanes_data, "nhanes_2017_2018_cleaned2.csv", row.names = FALSE)


