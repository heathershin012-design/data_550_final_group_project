# ==========================================
# DATA CLEANING FOR GROUP PROJECT
# Hyelim's role: missing data, recoding, derived variables
# ==========================================
library(here)

setwd("/Users/hyelimshin/Desktop/data550_project/data_550_final_group_project")

here::i_am("code/1_covid_cleaned.R")

absolute_path_to_data <- here("data", "covid_sub.csv")

covid <- read.csv(absolute_path_to_data, header = TRUE)




library(dplyr)

# ==========================================
# 1. LOAD DATA
# ==========================================

# Check structure
names(covid)
str(covid)
summary(covid)

# ==========================================
# 2. REMOVE EXTRA INDEX COLUMN
# ==========================================
# This column came from exporting the file and is not needed for analysis


# ==========================================
# 3. CHECK MISSING VALUES
# ==========================================
colSums(is.na(covid))

# ==========================================
# 4. STANDARDIZE TEXT VARIABLES
# ==========================================
# Make text consistent: lowercase and remove extra spaces
covid <- covid %>%
  mutate(
    across(where(is.character), trimws),
    across(where(is.character), tolower)
  )

# ==========================================
# 5. RECODE DEMOGRAPHIC VARIABLES
# ==========================================

# Recode sex into cleaner labels
covid <- covid %>%
  mutate(
    sex = case_when(
      SEX == "female" ~ "Female",
      SEX == "male" ~ "Male",
      TRUE ~ NA_character_
    )
  )
# ==========================================
# 6. RECODE YES/NO VARIABLES TO 0/1 for Regression Analysis
# ==========================================
# 1 = Yes, 0 = No

covid <- covid %>%
  mutate(
    usmer_bin          = ifelse(USMER == "yes", 1, ifelse(USMER == "no", 0, NA)),
    intubed_bin        = ifelse(INTUBED == "yes", 1, ifelse(INTUBED == "no", 0, NA)),
    pneumonia_bin      = ifelse(PNEUMONIA == "yes", 1, ifelse(PNEUMONIA == "no", 0, NA)),
    pregnant_bin       = ifelse(PREGNANT == "yes", 1, ifelse(PREGNANT == "no", 0, NA)),
    diabetes_bin       = ifelse(DIABETES == "yes", 1, ifelse(DIABETES == "no", 0, NA)),
    copd_bin           = ifelse(COPD == "yes", 1, ifelse(COPD == "no", 0, NA)),
    asthma_bin         = ifelse(ASTHMA == "yes", 1, ifelse(ASTHMA == "no", 0, NA)),
    inmsupr_bin        = ifelse(INMSUPR == "yes", 1, ifelse(INMSUPR == "no", 0, NA)),
    hypertension_bin   = ifelse(HIPERTENSION == "yes", 1, ifelse(HIPERTENSION == "no", 0, NA)),
    other_disease_bin  = ifelse(OTHER_DISEASE == "yes", 1, ifelse(OTHER_DISEASE == "no", 0, NA)),
    cardiovascular_bin = ifelse(CARDIOVASCULAR == "yes", 1, ifelse(CARDIOVASCULAR == "no", 0, NA)),
    obesity_bin        = ifelse(OBESITY == "yes", 1, ifelse(OBESITY == "no", 0, NA)),
    renal_bin          = ifelse(RENAL_CHRONIC == "yes", 1, ifelse(RENAL_CHRONIC == "no", 0, NA)),
    tobacco_bin        = ifelse(TOBACCO == "yes", 1, ifelse(TOBACCO == "no", 0, NA)),
    icu_bin            = ifelse(ICU == "yes", 1, ifelse(ICU == "no", 0, NA))
  )


# ==========================================
# 7. CREATE BINARY OUTCOME VARIABLES
# ==========================================

# Hospitalization outcome
# hospitalization = 1, returned home = 0
covid <- covid %>%
  mutate(
    hosp_bin = ifelse(PATIENT_TYPE == "hospitalization", 1,
                      ifelse(PATIENT_TYPE == "returned home", 0, NA))
  )

# Death outcome
# DATE_DIED is missing if patient did not die
covid <- covid %>%
  mutate(
    death_bin = ifelse(is.na(DATE_DIED) | DATE_DIED == "", 0, 1)
  )

# COVID diagnosis outcome based on CLASIFFICATION_FINAL
# 1, 2, 3 = diagnosed with COVID
# 4, 5, 6, 7 = inconclusive / not carrier / not positive
covid <- covid %>%
  mutate(
    covid_positive_bin = ifelse(CLASIFFICATION_FINAL %in% c(1, 2, 3), 1,
                                ifelse(CLASIFFICATION_FINAL %in% c(4, 5, 6, 7), 0, NA))
  )

# Worse outcome variable: hospitalization OR death
covid <- covid %>%
  mutate(
    severe_outcome = ifelse(hosp_bin == 1 | death_bin == 1, 1,
                            ifelse(hosp_bin == 0 & death_bin == 0, 0, NA))
  )

# ==========================================
# 8. QUALITY CHECKS
# ==========================================
table(covid$sex, useNA = "ifany")
table(covid$age_group, useNA = "ifany")

table(covid$diabetes_bin, useNA = "ifany")
table(covid$copd_bin, useNA = "ifany")
table(covid$asthma_bin, useNA = "ifany")
table(covid$inmsupr_bin, useNA = "ifany")
table(covid$hypertension_bin, useNA = "ifany")
table(covid$other_disease_bin, useNA = "ifany")
table(covid$cardiovascular_bin, useNA = "ifany")
table(covid$obesity_bin, useNA = "ifany")
table(covid$renal_bin, useNA = "ifany")
table(covid$tobacco_bin, useNA = "ifany")

table(covid$hosp_bin, useNA = "ifany")
table(covid$death_bin, useNA = "ifany")
table(covid$severe_outcome, useNA = "ifany")
table(covid$covid_positive_bin, useNA = "ifany")

# ==========================================
#1, 2, 3 → 1 (COVID positive)
#4, 5, 6, 7 → 0 (not COVID / inconclusive)
# ==========================================

covid <- covid %>%
  mutate(
    CLASIFFICATION_FINAL = ifelse(
      CLASIFFICATION_FINAL %in% c(1, 2, 3), 1,
      ifelse(CLASIFFICATION_FINAL >= 4, 0, NA)
    )
  )

# ==========================================
# 9. EXPORT CLEANED DATA FOR TEAMMATES
# ==========================================
write.csv(covid, "covid_cleaned.csv", row.names = FALSE)


table(covid$CLASIFFICATION_FINAL, covid$covid_positive_bin, useNA = "ifany")


