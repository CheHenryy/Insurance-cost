##Load libraries 
library(tidyverse) # for data manipulation and visualization


## Reading data 
df_r<-read.csv("C:/Users/chn03/Downloads/insurance cost/Insurance-cost/insurance_costs.csv")

##Understanding the data structure
dim(df_r)
str(df_r)
head(df_r, 10)
tail(df_r, 4)
colSums(is.na(df_r))
pct_miss_case(df_r)

## Data cleaning 

df_clean <- df_r %>%
  clean_names() %>%
  distinct() %>%
  mutate(across(where(is.character), ~ {
    val <- str_to_title(str_trim(.x))
    na_if(val, "")
  })) %>%
  mutate(across(c(bmi, age, annual_checkups), as.numeric)) %>%
  mutate(
    bmi_cat = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal Weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      .default = NA_character_
    ),
    age_cat = case_when(
      age >= 18 & age < 65 ~ "Adult",
      age >= 65 ~ "Senior",
      .default = NA_character_
    ),
    smoker = case_when(
      smoker == "Yes" ~ "Smoker",
      smoker == "No" ~ "Non-Smoker",
      .default = smoker
    ),
    chronic_condition = case_when(
      chronic_condition == "Yes" ~ "Has Condition",
      chronic_condition == "No" ~ "No Condition",
      .default = chronic_condition
    )
  ) %>%
  mutate(
    age_cat = factor(age_cat, levels = c( "Adult", "Senior")),
    bmi_cat = factor(bmi_cat, levels = c("Underweight", "Normal Weight", "Overweight", "Obese")),
    exercise_level = factor(exercise_level, levels = c("Low", "Medium", "High"))
  )  # complete case since missing data is less than 5% and likely MCAR


## checking missing data

pct_miss_case(df_clean)
gg_miss_var(df_clean, show_pct = TRUE)
gg_miss_var(df_clean, show_pct = TRUE,facet = bmi_cat)
gg_miss_var(df_clean, show_pct = TRUE,facet = annual_checkups)
pct_complete_case(df_clean)

df_clean <- 
  df_clean %>% 
  drop_na()
