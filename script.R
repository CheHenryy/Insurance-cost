
##Load libraries 
library(tidyverse) # for data manipulation and visualization
library(here)
library(flextable) # for creating tables
library(janitor) # for data cleaning
library(naniar) # for missing data visualization
library(rstatix) # for summary statistics
library(gtsummary) # for creating tables
library(kableExtra)
library(modelsummary)

## Reading data 
df_r<-read.csv("insurance_costs.csv")

##Understanding the data structure
dim(df_r)
str(df_r)
head(df_r, 10)
tail(df_r, 4)
view(df_r)
colSums(is.na(df_r))

table(df_r$exercise_level)
## checking missing data

pct_miss_case(df_r)
gg_miss_var(df_r, show_pct = TRUE)
gg_miss_var(df_r, show_pct = TRUE,facet = bmi_cat)
gg_miss_var(df_r, show_pct = TRUE,facet = annual_checkups)
pct_complete_case(df_r)


## Data cleaning 
df_clean<-df_r %>%
  clean_names() %>%
  distinct() %>%
  mutate(across(where(is.character), ~ {
    val <- str_to_title(str_trim(.x))
    na_if(val, "")
  })) %>%
  mutate(across(where(is.character), ~str_to_title(str_trim(.x)))) %>%
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
      age < 18 ~ "Child",
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
  )

 
table(df_clean$smoker)
######## exploratory data analysis 


## Summary tables
table(df_clean$children)
table(df_clean$prior_accidents)
table(df_clean$prior_claims)
table(df_clean$annual_checkups)


cont_sum<-df_r %>%
  get_summary_stats(age,bmi,charges, type = "common")
cont_sum
flextable(cont_sum)%>% 
  colformat_double(digits = 1)


cat_sum<-df_clean %>%
  select(sex, smoker, region, bmi_cat, age_cat,chronic_condition,exercise_level,plan_type) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"), 
    missing = "no")
cat_sum


##Cross tabulations
cross_tab1<-df_clean %>%
  select(smoker, region) %>%
  tbl_cross(
    row = smoker,
    col = region,
    percent = "row"
  )
cross_tab1

cross_tab2<-df_clean %>%
  select(age_cat, bmi_cat) %>%
  tbl_cross(
    row = age_cat,
    col = bmi_cat,
    percent = "row"
  )

cross_tab2

cross_tab3<-df_clean %>%
  select(chronic_condition, exercise_level) %>%
  tbl_cross(
    row = chronic_condition,
    col = exercise_level,
    percent = "row"
  )
cross_tab3

cross_tab4<-df_clean %>%
  select(plan_type, region) %>%
  tbl_cross(
    row = plan_type,
    col = region,
    percent = "row"
  )
cross_tab4

cross_tab5<-df_clean %>%
  select(plan_type, smoker) %>%
  tbl_cross(
    row = smoker,
    col = plan_type,
    percent = "row"
  )
cross_tab5

cross_tab6<-df_clean %>%
  select(age_cat, chronic_condition) %>%
  tbl_cross(
    row = age_cat,
    col = chronic_condition,
    percent = "row"
  )
cross_tab6

cross_tab7<-df_clean %>%
  select(exercise_level, bmi_cat) %>%
  tbl_cross(
    row = exercise_level,
    col = bmi_cat,
    percent = "row"
  )
cross_tab7

### Visualizations
ggplot(df_clean, aes(x = age, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = " A scatterplot of age and insurance cost", x = "Age", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "A scatterplot of BMI and insurance cost", x = "BMI",
       y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by smoking status", x = "Smoking Status", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = age_cat, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by age category", x = "Age Category", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi_cat, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by BMI category", x = "BMI Category", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = chronic_condition, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by chronic condition status", x = "Chronic Condition Status", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = exercise_level, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by exercise level", x = "Exercise Level", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = plan_type, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by plan type", x = "Plan Type", y = "Insurance Cost") +
  theme_minimal()

ggplot(df_clean, aes(x = region, y = charges)) +
  geom_boxplot() +
  labs(title = "Boxplot of insurance cost by region", x = "Region", y = "Insurance Cost") +
  theme_minimal()

### histogram of continuous variables
ggplot(df_clean, aes(x = age)) +
  geom_histogram(binwidth = 5, bins = 10) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

ggplot(df_clean, aes(x = bmi)) +
  geom_histogram(binwidth = 4, bins = 10) +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency")

ggplot(df_clean, aes(x = charges)) +
  geom_histogram(binwidth = 2500, bins=10, ) +
  labs(title = "Histogram of Insurance Charges", x = "Insurance Charges", y = "
Frequency")


##scatterplots 
ggplot(df_clean, aes(x = age, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Age and Insurance Charges", x = "Age", y = "Insurance Charges") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of BMI and Insurance Charges", x = "BMI", y = "Insurance Charges") +
  theme_minimal()
  
ggplot(df_clean, aes(x = age, y = bmi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatterplot of Age and BMI", x = "Age", y = "BMI") +
  theme_minimal()

ggplot(df_clean, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges, color = plan_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges, color = chronic_condition)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

ggplot(df_clean, aes(x = bmi, y = charges, color = age_cat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()


### linear regression model
names(df_clean)
model1a <- lm(charges ~ age + bmi + sex + smoker + chronic_condition + exercise_level, data = df_clean)
summary(model1a)
#model1a <- model1a %>% tbl_regression() %>% as_flex_table()
model1a

model1b <- lm(charges ~ age_cat + bmi_cat +sex + smoker + chronic_condition + exercise_level, data = df_clean)
summary(model1b)
#model1b <- model1b %>% tbl_regression() %>% as_flex_table()
model1b

model2a <- lm(charges ~ age + bmi + sex + smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + annual_checkups + plan_type , data = df_clean)
summary(model2a)
#model2a <- model2a %>% tbl_regression() %>% as_flex_table()
model2a

model2b <- lm(charges ~ age_cat + bmi_cat +sex + smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + annual_checkups + plan_type, data = df_clean)
summary(model2b)
#model2b <- model2b %>% tbl_regression() %>% as_flex_table()
model2b

##models
all_models <- list(
  "Model 1a" = model1a,
  "Model 1b" = model1b,
  "Model 2a" = model2a,
  "Model 2b" = model2b
)

var_labels <- c(
  "(Intercept)" = "Intercept",
  "age" = "Age (Years)",
  "age_catSenior" = "Age: Senior",
  "age_catChild" = "Age: Child",
  "bmi" = "BMI (Continuous)",
  "bmi_catObese" = "BMI: Obese",
  "bmi_catOverweight" = "BMI: Overweight",
  "bmi_catUnderweight" = "BMI: Underweight",
  "sexMale" = "Sex: Male",
  "smokerSmoker" = "Current Smoker",
  "chronic_conditionNo Condition" = "No Chronic Condition",
  "exercise_levelLow" = "Exercise: Low",
  "exercise_levelMedium" = "Exercise: Medium",
  "prior_accidents" = "Prior Accidents",
  "prior_claims" = "Prior Claims",
  "annual_checkups" = "Annual Checkups",
  "plan_typePremium" = "Plan: Premium",
  "plan_typeStandard" = "Plan: Standard"
)

final_model<-modelsummary(
  all_models,
  coef_rename = var_labels,
  fmt = 2,
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  output = "flextable"
) %>%
  flextable::autofit()

final_model

### model diagnostics 
par(mfrow = c(2, 2))
plot(model2b)
plot(model2a)

## transforming the dependent variable to address non-normality
df_clean <- df_clean %>%
  mutate(log_charges = log(charges + 1)) # Adding 1 to avoid
# log(0) issues



# Refit the model with log-transformed charges
model1a1 <- lm(log_charges ~ age + bmi + sex + smoker + chronic_condition + exercise_level, data = df_clean)
summary(model1a1)
#model1a <- model1a %>% tbl_regression() %>% as_flex_table()
model1a1

model1b1 <- lm(log_charges ~ age_cat + bmi_cat +sex + smoker + chronic_condition + exercise_level, data = df_clean)
summary(model1b1)
#model1b <- model1b %>% tbl_regression() %>% as_flex_table()
model1b1

model2a1<- lm(log_charges ~ age + bmi + sex + smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + annual_checkups + plan_type , data = df_clean)
summary(model2a1)
#model2a <- model2a %>% tbl_regression() %>% as_flex_table()
model2a1

model2b1 <- lm(log_charges ~ age_cat + bmi_cat +sex + smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + annual_checkups + plan_type, data = df_clean)
summary(model2b1)

all_models1 <- list(
  "Model 1a1" = model1a1,
  "Model 1b1" = model1b1,
  "Model 2a1" = model2a1,
  "Model 2b1" = model2b1
)
final_model1<-modelsummary(
  all_models1,
  coef_rename = var_labels,
  fmt = 2,
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  output = "flextable"
) %>%
  flextable::autofit()
final_model1


### model diagnostics 
par(mfrow = c(2, 2))
plot(model2b1)
plot(model2a1)
