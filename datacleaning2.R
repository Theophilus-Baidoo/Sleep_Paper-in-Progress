
library(dplyr)

nhanes_data <- read.csv("nhanes_2017_2018_cleaned2.csv", header = TRUE)

dim(nhanes_data)

# Convert SEQN to sequential numbers starting from 1
nhanes_data$SEQN <- seq(1, nrow(nhanes_data))

# Loop through columns and convert character columns to factors
for (col_name in names(nhanes_data)) {
  if (is.character(nhanes_data[[col_name]])) {
    nhanes_data[[col_name]] <- as.factor(nhanes_data[[col_name]])
  }
}

# Print the structure of the updated dataset to verify
str(nhanes_data)



summary(nhanes_data)




# Convert "Don't know" and "Refused" to NA for relevant variables, preserving factor levels
columns_to_clean <- c("depression", "diabetes", "education", "snore","apnea_symptoms","daytime_sleepy",
                      "work_vigorous", "trouble_sleep","work_moderate","hypertension", "marital_status","work_schedule")

for (col in columns_to_clean) {
  # Ensure column is treated as a factor
  nhanes_data[[col]] <- factor(nhanes_data[[col]])
  
  # Replace "Don't know" and "Refused" with NA
  nhanes_data[[col]] <- forcats::fct_recode(nhanes_data[[col]], NULL = "Don't know", NULL = "Refused", NULL = "Don't Know")
}


# Replace 999 and 777 in Alcohol_Consumption with NA, keeping it numeric
nhanes_data$alcohol_day <- ifelse(nhanes_data$alcohol_day %in% c(999, 777), NA, nhanes_data$alcohol_day)





# Ensure numeric variables retain their type
numeric_columns <- c("alcohol_day", "sugar", "sat_fat", "sleep_weekday", "bmi", "interview_weight", "SEQN")
for (col in numeric_columns) {
  nhanes_data[[col]] <- as.numeric(nhanes_data[[col]])
}



# Compute 1.5×IQR fences
sugar_iqr <- IQR(nhanes_data$sugar, na.rm=TRUE)
sugar_q3  <- quantile(nhanes_data$sugar, 0.75, na.rm=TRUE)
upper_sugar <- sugar_q3 + 1.5 * sugar_iqr

sat_iqr   <- IQR(nhanes_data$sat_fat, na.rm=TRUE)
sat_q3    <- quantile(nhanes_data$sat_fat, 0.75, na.rm=TRUE)
upper_sat   <- sat_q3   + 1.5 * sat_iqr

# Get the 95th percentile values
sugar_p95 <- quantile(nhanes_data$sugar,   0.95, na.rm=TRUE)
sat_p95   <- quantile(nhanes_data$sat_fat, 0.95, na.rm=TRUE)

# Winsorize only those beyond the 1.5×IQR fence
nhanes_data <- nhanes_data %>%
  mutate(
    sugar   = if_else(sugar   > upper_sugar, sugar_p95, sugar),
    sat_fat = if_else(sat_fat > upper_sat,   sat_p95,   sat_fat)
  )



saveRDS(nhanes_data, file = "nhanes_cleandata2.rds")

