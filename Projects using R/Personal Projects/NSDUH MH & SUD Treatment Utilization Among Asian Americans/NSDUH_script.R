# Tajrian Amad
# Project: Disparities in Mental Health and Substance Use Treatment Utilization among Asian Americans in the United States
# Data set: National Survey on Drug Use and Health (NSDUH) - 2022 and 2023

install.packages(c("survey", "dplyr", "ggplot2", "haven", "srvyr", "tidyr", "labelled", "knitr", "purrr", "car"))

library(survey)
library(dplyr)
library(ggplot2)
library(haven)
library(srvyr)
library(tidyr)
library(labelled)
library(knitr)
library(purrr)
library(car)

# Merging datasets and cleaning -----------------------------------------------

# Create a new subset for each year data with selected relevant variables
# 2022 data
nsduh_2022_subset <- puf2022_110424 %>% 
  select(
    VEREP, VESTR_C, ANALWT2_C, # Survey design variables
    NEWRACE2, IRSEX, EDUHIGHCAT, SEXIDENT, CATAG3, IRMARIT, IRWRKSTAT, IRINSUR4, INCOME, POVERTY3, # Demographic and stratification variables
    HEALTH2, # Overall health status
    AMIPY, SMIPY, MHTRTPY, # Mental health variables
    IRPYUD5ALC, IRPYUD5MRJ, UD5ILALANY, SUTRTPY # Substance use variables
  ) %>%
  
  # New data set limited to Asian American respondents
  filter(NEWRACE2 == 5) %>% 
  
  # Re-code gender
  mutate(
    gender = case_when( 
      IRSEX == 1 ~ "Male",
      IRSEX == 2 ~ "Female"
    )
  )

# 2023 data
nsduh_2023_subset <- puf2023_102124 %>%
  select(
    VEREP, VESTR_C, ANALWT2_C, # Survey design variables
    NEWRACE2, IRSEX, EDUHIGHCAT, SEXIDENT22, CATAG3, IRMARIT, IRWRKSTAT, IRINSUR4, INCOME, POVERTY3, # Demographic and stratification variables
    HEALTH2, # Overall health status
    AMIPY, SMIPY, MHTRTPY, # Mental health variables
    IRPYUD5ALC, IRPYUD5MRJ, UD5ILALANY, SUTRTPY # Substance use variables
  ) %>%
  
  # New data set limited to Asian American respondents
  filter(NEWRACE2 == 5) %>%
  
  # Re-code gender
  mutate(
    gender = case_when(
      IRSEX == 1 ~ "Male",
      IRSEX == 2 ~ "Female"
    )
  ) %>%
  
  # Rename sexual identity to match with the SEXIDENT variable in 2022
  rename(SEXIDENT = SEXIDENT22)

# Merge both the 2022 and 2023 subsets
nsduh_merged <- bind_rows(nsduh_2022_subset, nsduh_2023_subset)

# Create a 5-level variable for SEXIDENT
nsduh_merged <- nsduh_merged %>%
  mutate(
    SEXIDENT_5 = case_when(
      SEXIDENT == 1 ~ 1,
      SEXIDENT == 2 ~ 2,
      SEXIDENT == 3 ~ 3,
      SEXIDENT == 4 ~ 4,
      is.na(SEXIDENT) ~ 5,
      TRUE ~ 5
    )
  ) %>%
  
  mutate(
    SEXIDENT_5 = factor(SEXIDENT_5,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Heterosexual", "Gay/Lesbian", "Bisexual", "Other", "Missing/Unspecified"))
  )

# Adjust weights for pooled analysis
nsduh_merged <- nsduh_merged %>% 
  mutate(ANALWT_ADJ = ANALWT2_C / 2) # Create a new adjusted weight variable

# Re-code the outcome variables into binary variables
nsduh_merged <- nsduh_merged %>%
  mutate(
    ami_past_year = if_else(AMIPY == 1, 1, 0, missing = NA_real_),
    smi_past_year = if_else(SMIPY == 1, 1, 0, missing = NA_real_),
    received_mh_tx = if_else(MHTRTPY == 1, 1, 0, missing = NA_real_), 
    alc_disorder = if_else(IRPYUD5ALC == 1, 1, 0, missing = NA_real_),
    mrj_disorder = if_else(IRPYUD5MRJ == 1, 1, 0, missing = NA_real_),
    any_sud = if_else(UD5ILALANY == 1, 1, 0, missing = NA_real_),
    received_sud_tx = if_else(SUTRTPY == 1, 1, 0, missing = NA_real_)
  )
  
# Create a survey design object with adjusted weight
nsduh_merged_design <- svydesign(
  id = ~VEREP,
  strata = ~VESTR_C,
  weights = ~ANALWT_ADJ, 
  data = nsduh_merged,
  nest = TRUE
)

# Convert to srvyr design object
nsduh_merged_srvyr_design <- nsduh_merged_design %>% as_survey_design()

# Check the design summary
summary(nsduh_merged_srvyr_design)

# Check missing data
sum(is.na(nsduh_merged))

# Data Analysis -----------------------------------------------------------

# Weighted proportions (population totals) for mental health outcomes among Asian Americans

mh_AA <- nsduh_merged_srvyr_design %>%
  summarise(
    ami_total = survey_total(ami_past_year, na.rm = TRUE),
    smi_total = survey_total(smi_past_year, na.rm = TRUE),
    mh_tx_total = survey_total(received_mh_tx, na.rm = TRUE)
  )
print(mh_AA)

# Mental health outcomes by gender
mh_by_gender <- nsduh_merged_srvyr_design %>%
  group_by(gender) %>%
  summarise(
    ami_total = survey_total(ami_past_year, na.rm = TRUE),
    smi_total = survey_total(smi_past_year, na.rm = TRUE),
    mh_tx_total = survey_total(received_mh_tx, na.rm = TRUE)
  )
print(mh_by_gender)

# Weighted proportions (population totals) for substance use outcomes among Asian Americans

sud_AA <- nsduh_merged_srvyr_design %>% 
  summarise(
    alc_disorder_total = survey_total(alc_disorder, na.rm = TRUE),
    mrj_disorder_total = survey_total(mrj_disorder, na.rm = TRUE),
    any_sud_total = survey_total(any_sud, na.rm = TRUE),
    sud_tx_total = survey_total(received_sud_tx, na.rm = TRUE)
  )
print(sud_AA)

# Substance use outcomes by gender
sud_by_gender <- nsduh_merged_srvyr_design %>% 
  group_by(gender) %>% 
  summarise(
    alc_disorder_total = survey_total(alc_disorder, na.rm = TRUE),
    mrj_disorder_total = survey_total(mrj_disorder, na.rm = TRUE),
    any_sud_total = survey_total(any_sud, na.rm = TRUE),
    sud_tx_total = survey_total(received_sud_tx, na.rm = TRUE)
  )
print(sud_by_gender)


# 2-way tables - weighted and unweighted N + percentages by gender

# Age groups
age_weighted_by_gender <- svytable(~CATAG3 + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N by gender
print(age_weighted_by_gender)
age_weighted_total <- svytable(~ CATAG3, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
age_weighted_total_pct <- prop.table(age_weighted_total) * 100
print(round(age_weighted_total_pct, 2))
age_weighted_by_gender_pct <- prop.table(age_weighted_by_gender,margin = 2) * 100
print(round(age_weighted_by_gender_pct, 2))
age_unweighted_by_gender <- table(nsduh_merged$CATAG3, nsduh_merged$gender, useNA = "always") # Unweighted N
print(age_unweighted_by_gender)
age_unweighted_total <- rowSums(age_unweighted_by_gender[, c("Female", "Male")])
print(age_unweighted_total)
print(svychisq(~CATAG3 + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Education level - combine 1 (less than high school) & 5 ((12 to 17 y/o))
edu_weighted_by_gender <- svytable(~EDUHIGHCAT + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(edu_weighted_by_gender)
edu_weighted_total <- svytable(~ EDUHIGHCAT, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
edu_weighted_total_pct <- prop.table(edu_weighted_total) * 100
print(round(edu_weighted_total_pct, 2))
edu_weighted_by_gender_pct <- svytable(~EDUHIGHCAT + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentages
print(round(edu_weighted_by_gender_pct * 100, 2))
edu_unweighted_by_gender <- table(nsduh_merged$EDUHIGHCAT, nsduh_merged$gender, useNA = "always") # Unweighted N
print(edu_unweighted_by_gender)
edu_unweighted_total <- rowSums(edu_unweighted_by_gender[, c("Female", "Male")])
print(edu_unweighted_total)
print(svychisq(~EDUHIGHCAT + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Sexual orientation
sex_ident_weighted_by_gender <- svytable(~SEXIDENT_5 + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(sex_ident_weighted_by_gender)
sex_ident_weighted_total <- svytable(~SEXIDENT_5, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
sex_ident_weighted_total_pct <- prop.table(sex_ident_weighted_total) * 100
print(round(sex_ident_weighted_total_pct, 2))
sex_ident_weighted_by_gender_pct <- svytable(~SEXIDENT_5 + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentages
print(round(sex_ident_weighted_by_gender_pct * 100, 2))
sex_ident_unweighted_by_gender <- table(nsduh_merged$SEXIDENT_5, nsduh_merged$gender, useNA = "always") # Unweighted N
print(sex_ident_unweighted_by_gender)
sex_ident_unweighted_total <- rowSums(sex_ident_unweighted_by_gender[, c("Female", "Male")])
print(sex_ident_unweighted_total)
print(svychisq(~SEXIDENT_5 + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Marital status
marital_weighted_by_gender <- svytable(~IRMARIT + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(marital_weighted_by_gender)
marital_weighted_total <- svytable(~IRMARIT, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
marital_weighted_total_pct <- prop.table(marital_weighted_total) * 100
print(round(marital_weighted_total_pct, 2))
marital_weighted_by_gender_pct <- svytable(~IRMARIT + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentages
print(round(marital_weighted_by_gender_pct * 100, 2))
marital_unweighted_by_gender <- table(nsduh_merged$IRMARIT, nsduh_merged$gender, useNA = "always") # Unweighted N
print(marital_unweighted_by_gender)
marital_unweighted_total <- rowSums(marital_unweighted_by_gender[, c("Female", "Male")])
print(marital_unweighted_total)
print(svychisq(~IRMARIT + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Employment status
emp_weighted_by_gender <- svytable(~IRWRKSTAT + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(emp_weighted_by_gender)
emp_weighted_total <- svytable(~IRWRKSTAT, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
emp_weighted_total_pct <- prop.table(emp_weighted_total) * 100
print(round(emp_weighted_total_pct, 2))
emp_weighted_gender_pct <- svytable(~IRWRKSTAT + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentage
print(round(emp_weighted_gender_pct * 100, 2))
emp_unweighted_by_gender <- table(nsduh_merged$IRWRKSTAT, nsduh_merged$gender, useNA = "always") # Unweighted N
print(emp_unweighted_by_gender)
emp_unweighted_total <- rowSums(emp_unweighted_by_gender[, c("Female", "Male")])
print(emp_unweighted_total)
print(svychisq(~IRWRKSTAT + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Insurance status
ins_weighted_by_gender <- svytable(~IRINSUR4 + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(ins_weighted_by_gender)
ins_weighted_total <- svytable(~IRINSUR4, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
ins_weighted_total_pct <- prop.table(ins_weighted_total) * 100
print(round(ins_weighted_total_pct, 2))
ins_weighted_gender_pct <- svytable(~IRINSUR4 + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentage
print(round(ins_weighted_gender_pct * 100, 2))
ins_unweighted_by_gender <- table(nsduh_merged$IRINSUR4, nsduh_merged$gender, useNA = "always") # Unweighted N
print(ins_unweighted_by_gender)
ins_unweighted_total <- rowSums(ins_unweighted_by_gender[, c("Female", "Male")])
print(ins_unweighted_total)
print(svychisq(~IRINSUR4 + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Annual household income
inc_weighted_by_gender <- svytable(~INCOME + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(inc_weighted_by_gender)
inc_weighted_total <- svytable(~INCOME, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
inc_weighted_total_pct <- prop.table(inc_weighted_total) * 100
print(round(inc_weighted_total_pct, 2))
inc_weighted_gender_pct <- svytable(~INCOME + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentage
print(round(inc_weighted_gender_pct * 100, 2))
inc_unweighted_by_gender <- table(nsduh_merged$INCOME, nsduh_merged$gender, useNA = "always") # Unweighted N
print(inc_unweighted_by_gender)
inc_unweighted_total <- rowSums(inc_unweighted_by_gender[, c("Female", "Male")])
print(inc_unweighted_total)
print(svychisq(~INCOME + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Poverty
pov_weighted_by_gender <- svytable(~POVERTY3 + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(pov_weighted_by_gender)
pov_weighted_gender_pct <- svytable(~POVERTY3 + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentage
print(round(pov_weighted_gender_pct * 100, 2))
pov_unweighted_by_gender <- table(nsduh_merged$POVERTY3, nsduh_merged$gender, useNA = "always") # Unweighted N
print(pov_unweighted_by_gender)
print(svychisq(~POVERTY3 + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Overall health 
hlth_weighted_by_gender <- svytable(~HEALTH2 + gender, design = nsduh_merged_design, na.action = na.pass) # Weighted N
print(hlth_weighted_by_gender)
hlth_weighted_total <- svytable(~HEALTH2, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
hlth_weighted_total_pct <- prop.table(hlth_weighted_total) * 100
print(round(hlth_weighted_total_pct, 2))
hlth_weighted_gender_pct <- svytable(~HEALTH2 + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass) # Weighted percentage
print(round(hlth_weighted_gender_pct * 100, 2))
hlth_unweighted_by_gender <- table(nsduh_merged$HEALTH2, nsduh_merged$gender, useNA = "always") # Unweighted N
print(hlth_unweighted_by_gender)
hlth_unweighted_total <- rowSums(hlth_unweighted_by_gender[, c("Female", "Male")])
print(hlth_unweighted_total)
print(svychisq(~HEALTH2 + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Any mental illness
ami_weighted_gender <- svytable(~ami_past_year + gender, design = nsduh_merged_design, na.action = na.pass)
print(ami_weighted_gender)
ami_weighted_total <- svytable(~ami_past_year, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
ami_weighted_total_pct <- prop.table(ami_weighted_total) * 100
print(round(ami_weighted_total_pct, 2))
ami_weighted_gender_pct <- svytable(~ami_past_year + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(ami_weighted_gender_pct * 100, 2))
ami_unweighted_gender <- table(nsduh_merged$ami_past_year, nsduh_merged$gender, useNA = "always")
print(ami_unweighted_gender)
ami_unweighted_total <- rowSums(ami_unweighted_gender[, c("Female", "Male")])
print(ami_unweighted_total)
print(svychisq(~ami_past_year + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Serious mental illness
smi_weighted_gender <- svytable(~smi_past_year + gender, design = nsduh_merged_design, na.action = na.pass)
print(smi_weighted_gender)
smi_weighted_total <- svytable(~smi_past_year, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
smi_weighted_total_pct <- prop.table(smi_weighted_total) * 100
print(round(smi_weighted_total_pct, 2))
smi_weighted_gender_pct <- svytable(~smi_past_year + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(smi_weighted_gender_pct * 100, 2))
smi_unweighted_gender <- table(nsduh_merged$smi_past_year, nsduh_merged$gender, useNA = "always")
print(smi_unweighted_gender)
smi_unweighted_total <- rowSums(smi_unweighted_gender[, c("Female", "Male")])
print(smi_unweighted_total)
print(svychisq(~smi_past_year + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Receiving mental health treatment
mh_trt_weighted_gender <- svytable(~received_mh_tx + gender, design = nsduh_merged_design, na.action = na.pass)
print(mh_trt_weighted_gender)
mh_trt_weighted_total <- svytable(~received_mh_tx, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
mh_trt_weighted_total_pct <- prop.table(mh_trt_weighted_total) * 100
print(round(mh_trt_weighted_total_pct, 2))
mh_trt_weighted_gender_pct <- svytable(~received_mh_tx + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(mh_trt_weighted_gender_pct * 100, 2))
mh_trt_unweighted_gender <- table(nsduh_merged$received_mh_tx, nsduh_merged$gender, useNA = "always")
print(mh_trt_unweighted_gender)
mh_trt_unweighted_total <- rowSums(mh_trt_unweighted_gender[, c("Female", "Male")])
print(mh_trt_unweighted_total)
print(svychisq(~received_mh_tx + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Alcohol use disorder 
alc_weighted_gender <- svytable(~alc_disorder + gender, design = nsduh_merged_design, na.action = na.pass)
print(alc_weighted_gender)
alc_weighted_total <- svytable(~alc_disorder, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
alc_weighted_total_pct <- prop.table(alc_weighted_total) * 100
print(round(alc_weighted_total_pct, 2))
alc_weighted_gender_pct <- svytable(~alc_disorder + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(alc_weighted_gender_pct * 100, 2))
alc_unweighted_gender <- table(nsduh_merged$alc_disorder, nsduh_merged$gender, useNA = "always")
print(alc_unweighted_gender)
alc_unweighted_total <- rowSums(alc_unweighted_gender[, c("Female", "Male")])
print(alc_unweighted_total)
print(svychisq(~alc_disorder + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Marijuana use 
mrj_weighted_gender <- svytable(~mrj_disorder + gender, design = nsduh_merged_design, na.action = na.pass)
print(mrj_weighted_gender)
mrj_weighted_total <- svytable(~mrj_disorder, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
mrj_weighted_total_pct <- prop.table(mrj_weighted_total) * 100
print(round(mrj_weighted_total_pct, 2))
mrj_weighted_gender_pct <- svytable(~mrj_disorder + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(mrj_weighted_gender_pct * 100, 2))
mrj_unweighted_gender <- table(nsduh_merged$mrj_disorder, nsduh_merged$gender, useNA = "always")
print(mrj_unweighted_gender)
mrj_unweighted_total <- rowSums(mrj_unweighted_gender[, c("Female", "Male")])
print(mrj_unweighted_total)
print(svychisq(~mrj_disorder + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Any SUD
sud_weighted_gender <- svytable(~any_sud + gender, design = nsduh_merged_design, na.action = na.pass)
print(sud_weighted_gender)
sud_weighted_total <- svytable(~any_sud, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
sud_weighted_total_pct <- prop.table(sud_weighted_total) * 100
print(round(sud_weighted_total_pct, 2))
sud_weighted_gender_pct <- svytable(~any_sud + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(sud_weighted_gender_pct * 100, 2))
sud_unweighted_gender <- table(nsduh_merged$any_sud, nsduh_merged$gender, useNA = "always")
print(sud_unweighted_gender)
sud_unweighted_total <- rowSums(sud_unweighted_gender[, c("Female", "Male")])
print(sud_unweighted_total)
print(svychisq(~any_sud + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# SUD Treatment
sud_trt_weighted_gender <- svytable(~received_sud_tx + gender, design = nsduh_merged_design, na.action = na.pass)
print(sud_trt_weighted_gender)
sud_trt_weighted_total <- svytable(~received_sud_tx, design = nsduh_merged_design, na.action = na.pass) # Total weighted N
sud_trt_weighted_total_pct <- prop.table(sud_trt_weighted_total) * 100
print(round(sud_trt_weighted_total_pct, 2))
sud_trt_weighted_gender_pct <- svytable(~received_sud_tx + gender, design = nsduh_merged_design, Ntotal = TRUE, na.action = na.pass)
print(round(sud_trt_weighted_gender_pct * 100, 2))
sud_trt_unweighted_gender <- table(nsduh_merged$received_sud_tx, nsduh_merged$gender, useNA = "always")
print(sud_trt_unweighted_gender)
sud_trt_unweighted_total <- rowSums(sud_trt_unweighted_gender[, c("Female", "Male")])
print(sud_trt_unweighted_total)
print(svychisq(~received_sud_tx + gender, design = nsduh_merged_design, statistic = "Chisq", na.action = na.omit)) # Chi square test

# Logistic regression for mental health treatment and SUD treatment by gender

# Mental health treatment - unadjusted model
mentalhlth_unadj_model <- svyglm(received_mh_tx ~ gender,
                                 design = nsduh_merged_design,
                                 family = quasibinomial(),
                                 na.action = na.omit)

print(summary(mentalhlth_unadj_model))

print(exp(cbind(OR = coef(mentalhlth_unadj_model), confint(mentalhlth_unadj_model)))) # OR and 95% CI

# Mental health treatment - adjusted model
mentalhlth_adj_model <- svyglm(received_mh_tx ~ gender + CATAG3 + IRMARIT + EDUHIGHCAT + INCOME + SEXIDENT_5 + IRINSUR4 + HEALTH2,
                               design = nsduh_merged_design,
                               family = quasibinomial(),
                               na.action = na.omit)

print(summary(mentalhlth_adj_model))

print(exp(cbind(OR = coef(mentalhlth_adj_model), confint(mentalhlth_adj_model)))) # OR and 95% CI

# SUD treatment - unadjusted model
sudtrt_unadj_model <- svyglm(received_sud_tx ~ gender,
                                 design = nsduh_merged_design,
                                 family = quasibinomial(),
                                 na.action = na.omit)

print(summary(sudtrt_unadj_model))

print(exp(cbind(OR = coef(sudtrt_unadj_model), confint(sudtrt_unadj_model)))) # OR and 95% CI

# SUD treatment - adjusted model
sudtrt_adj_model <- svyglm(received_sud_tx ~ gender + CATAG3 + IRMARIT + EDUHIGHCAT + IRWRKSTAT + SEXIDENT_5 + IRINSUR4,
                               design = nsduh_merged_design,
                               family = quasibinomial(),
                               na.action = na.omit)

print(summary(sudtrt_adj_model))

print(exp(cbind(OR = coef(sudtrt_adj_model), confint(sudtrt_adj_model)))) # OR and 95% CI



