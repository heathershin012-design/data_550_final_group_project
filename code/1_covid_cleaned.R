# ==========================================
# DATA CLEANING FOR GROUP PROJECT
# ==========================================

library(here)
library(dplyr)

# ==========================================
# 1. LOAD DATA
# ==========================================
here::i_am("code/1_covid_cleaned.R")

absolute_path_to_data <- here::here("data", "covid_sub.csv")
covid <- read.csv(absolute_path_to_data, header = TRUE)

# Check structure
names(covid)
str(covid)
summary(covid)

# ==========================================
# 2. REMOVE EXTRA INDEX COLUMN
# ==========================================
covid <- covid %>% select(-X)

# ==========================================
# 3. CHECK MISSING VALUES
# ==========================================
print(colSums(is.na(covid)))

# ==========================================
# 4. STANDARDIZE TEXT VARIABLES
# ==========================================
covid <- covid %>%
  mutate(
    across(where(is.character), trimws),
    across(where(is.character), tolower)
  )

# ==========================================
# 5. RECODE SEX into Female/Male
# ==========================================
covid <- covid %>%
  mutate(
    SEX = case_when(
      SEX == "female" ~ "Female",
      SEX == "male"   ~ "Male",
      TRUE            ~ NA_character_
    )
  )

# ==========================================
# 6. RECODE YES/NO VARIABLES TO 0/1
# ==========================================
# 1 = Yes, 0 = No
yes_no_cols <- c("USMER", "INTUBED", "PNEUMONIA", "PREGNANT",
                 "DIABETES", "COPD", "ASTHMA", "INMSUPR",
                 "HIPERTENSION", "OTHER_DISEASE", "CARDIOVASCULAR",
                 "OBESITY", "RENAL_CHRONIC", "TOBACCO", "ICU")

covid <- covid %>%
  mutate(across(all_of(yes_no_cols), ~ case_when(
    tolower(.) == "yes" ~ 1,
    tolower(.) == "no"  ~ 0,
    TRUE                ~ NA_real_
  )))

# ==========================================
# 7. CREATE BINARY OUTCOME VARIABLES
# ==========================================

# Hospitalization: 1 = hospitalized, 0 = returned home
covid <- covid %>%
  mutate(
    hosp_bin = ifelse(PATIENT_TYPE == "hospitalization", 1,
                      ifelse(PATIENT_TYPE == "returned home", 0, NA))
  )

# Death: 1 = died, 0 = survived
# DATE_DIED is missing when patient did NOT die — keep as 0
covid <- covid %>%
  mutate(
    death_bin = ifelse(is.na(DATE_DIED) | DATE_DIED == "", 0, 1)
  )

# COVID positive: 1-3 = positive, 4-7 = not positive
covid <- covid %>%
  mutate(
    covid_positive_bin = ifelse(CLASIFFICATION_FINAL %in% c(1, 2, 3), 1,
                                ifelse(CLASIFFICATION_FINAL %in% c(4, 5, 6, 7), 0, NA))
  )

# Severe outcome: hospitalized OR died
covid <- covid %>%
  mutate(
    severe_outcome = ifelse(hosp_bin == 1 | death_bin == 1, 1,
                            ifelse(hosp_bin == 0 & death_bin == 0, 0, NA))
  )

# ==========================================
# 8. REMOVE MISSING VALUES (SELECTIVE)
# ==========================================
# NOTE: We do NOT remove NAs from DATE_DIED, INTUBED, ICU, PREGNANT
# because missing in those columns is meaningful (not applicable)
# We only remove NAs from columns where missing = truly unknown

covid <- covid %>%
  filter(!is.na(AGE),
         !is.na(PNEUMONIA),
         !is.na(DIABETES),
         !is.na(COPD),
         !is.na(ASTHMA),
         !is.na(INMSUPR),
         !is.na(HIPERTENSION),
         !is.na(OTHER_DISEASE),
         !is.na(CARDIOVASCULAR),
         !is.na(OBESITY),
         !is.na(RENAL_CHRONIC),
         !is.na(TOBACCO),
         !is.na(SEX),
         !is.na(hosp_bin),
         !is.na(covid_positive_bin),
         !is.na(severe_outcome))

# ==========================================
# 9. QUALITY CHECKS
# ==========================================
cat("Rows after cleaning:", nrow(covid), "\n")

print(table(covid$SEX, useNA = "ifany"))
print(table(covid$USMER, useNA = "ifany"))
print(table(covid$DIABETES, useNA = "ifany"))
print(table(covid$hosp_bin, useNA = "ifany"))
print(table(covid$death_bin, useNA = "ifany"))
print(table(covid$severe_outcome, useNA = "ifany"))
print(table(covid$covid_positive_bin, useNA = "ifany"))

# ==========================================
# 10. EXPORT CLEANED DATA
# ==========================================
saveRDS(covid, file = here::here("output", "covid_cleaned.rds"))

cat("Done! Cleaned data saved to output/covid_cleaned.rds\n")