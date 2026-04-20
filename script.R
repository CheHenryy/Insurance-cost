
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
  ) %>%
  drop_na()

str(df_clean)
 table(df_clean$exercise_level)

 #exploratory data analysis 

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

##boxplots

p1<-ggplot(df_clean, aes(x = age_cat, y = charges, fill = age_cat)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Charges by Age Category", x = "Age Category", y = "Insurance Charges") +
  theme_minimal() + theme(legend.position = "none")

p2<-ggplot(df_clean, aes(x = bmi_cat, y = charges, fill = bmi_cat)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Charges by BMI Category", x = "BMI Category", y = "Insurance Charges") +
  theme_minimal() + theme(legend.position = "none")

p3<-ggplot(df_clean, aes(x = chronic_condition, y = charges, fill = chronic_condition)) +
  geom_boxplot() + 
  labs(title = "Boxplot of Charges by Chronic Condition", x = "Chronic Condition Status", y = "Insurance Charges") +
  theme_minimal() + theme(legend.position = "none")

p4<-ggplot(df_clean, aes(x = exercise_level, y = charges, fill = exercise_level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Charges by Exercise Level", x = "Exercise Level", y = "Insurance Charges") +
  theme_minimal() + theme(legend.position = "none")


library(cowplot)
plot_grid(p1, p2, p3, p4,
          labels = c("P1", "P2", "P3", "P4"), 
          ncol = 2)
### histogram of continuous variables
d1<-ggplot(df_clean, aes(x = age)) +
  geom_histogram(binwidth = 10, fill = "#B57B7B", color = "white") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")+
  theme_minimal()

d2<-ggplot(df_clean, aes(x = bmi)) +
  geom_histogram(binwidth = 4, fill = "#6B8E6B", color = "white") +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency")+
  theme_minimal()

d3<-ggplot(df_clean, aes(x = charges)) +
  geom_histogram(binwidth = 2500, fill = "#5B7FA6", color = "white" ) +
  labs(title = "Histogram of Insurance Charges", x = "Insurance Charges", y = "
Frequency") +
  theme_minimal()

d4<-ggplot(df_clean, aes(x = annual_checkups)) +
  geom_histogram(binwidth = 1, fill = "#A67B5B", color = "white") +
  labs(title = "Histogram of Annual Checkups", x = "Annual Checkups", y = "Frequency") +
  theme_minimal()

plot_grid(d1, d2, d3, d4,
          labels = c("D1", "D2", "D3", "D4"), 
          ncol = 2)
##scatterplots 

a1<-ggplot(df_clean, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

a2<-ggplot(df_clean, aes(x = age, y = charges, color = plan_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by plan type", x = "Age", y = "Insurance Charges") +
  theme_minimal()

a3<-ggplot(df_clean, aes(x = age, y = charges, color = chronic_condition)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by chronic condition", x = "Age", y = "Insurance Charges") +
  theme_minimal()

a4<-ggplot(df_clean, aes(x = age, y = charges, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Age and Insurance Charges by plan type", x = "Age", y = "Insurance Charges") +
  theme_minimal()

plot_grid(a1, a2, a3, a4,
          labels = c("A1", "A2", "A3", "A4"), 
          ncol = 2)

b1<-ggplot(df_clean, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by Smoking Status", x = "Age", y = "Insurance Charges") +
  theme_minimal()

b2<-ggplot(df_clean, aes(x = bmi, y = charges, color = plan_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by plan type", x = "Age", y = "Insurance Charges") +
  theme_minimal()

b3<-ggplot(df_clean, aes(x = bmi, y = charges, color = chronic_condition)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by chronic conditions", x = "Age", y = "Insurance Charges") +
  theme_minimal()

b4<-ggplot(df_clean, aes(x = bmi, y = charges, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by sex", x = "Age", y = "Insurance Charges") +
  theme_minimal()

plot_grid(b1, b2, b3, b4,
          labels = c("B1", "B2", "B3", "B4"), 
          ncol = 2)

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
