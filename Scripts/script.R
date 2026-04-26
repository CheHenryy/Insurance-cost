
##Load libraries 
library(tidyverse) # for data manipulation and visualization
library(here)
library(flextable) # for creating tables
library(janitor) # for data cleaning
library(naniar) # for missing data visualization
library(rstatix) # for summary statistics
library(gtsummary) # for creating tables
library(kableExtra) # for creating tables
library(modelsummary) # for summarizing regression models
library(lm.beta)

## Reading data 
source(here::here("Scripts", "import_and_clean.R"))

## Summary tables
table(df_clean$children)
table(df_clean$prior_accidents)
table(df_clean$prior_claims)
table(df_clean$annual_checkups)


cont_sum<-df_clean %>%
  get_summary_stats(age,bmi,charges, show = c("mean", "sd", "median", "min", "max"))
cont_sum
flextable(cont_sum)%>% 
  colformat_double(digits = 1)


cat_sum<-df_clean %>%
  select(sex, smoker, region, bmi_cat, age_cat,chronic_condition,exercise_level,plan_type) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"))
cat_sum

##Cross tabulations
cross_tab1<-df_clean %>%
  select(smoker, bmi_cat) %>%
  tbl_cross(
    row = smoker,
    col = bmi_cat,
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
  select(chronic_condition, bmi_cat) %>%
  tbl_cross(
    row = chronic_condition,
    col = bmi_cat,
    percent = "row"
  )
cross_tab3

cross_tab4<-df_clean %>%
  select(plan_type, bmi_cat) %>%
  tbl_cross(
    row = plan_type,
    col = bmi_cat,
    percent = "row"
  )
cross_tab4


cross_tab5<-df_clean %>%
  select(exercise_level, bmi_cat) %>%
  tbl_cross(
    row = exercise_level,
    col = bmi_cat,
    percent = "row"
  )
cross_tab5

cross_tab6<-df_clean %>%
  select(chronic_condition, age_cat) %>%
  tbl_cross(
    row = chronic_condition,
    col = age_cat,
    percent = "row"
  )
cross_tab6

cross_tab7<-df_clean %>%
  select(plan_type, age_cat) %>%
  tbl_cross(
    row = plan_type,
    col = age_cat,
    percent = "row"
  )
cross_tab7

complete_crostab<-tbl_stack(list(cross_tab1, cross_tab2,cross_tab3,cross_tab4,cross_tab5,
               cross_tab6, cross_tab7),group_header = c("Crosstab:  Smoking Status by BMI Category", 
                                                         "Crosstab: Age group by BMI category", 
                                                         "Crosstab: Chronic Condition by BMI category",
                                                        " Crosstab: Plan type by BMI category",
                                                        " Crosstab: Exercise level by BMI category",
                                                        " Crosstab: Chronic conditions by age category",
                                                        " Crosstab: Plan type by age category"))

complete_crostab
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
  labs(title = "Scatterplot of BMI and Insurance Charges by Smoking Status", x = "BMI", y = "Insurance Charges") +
  theme_minimal()

b2<-ggplot(df_clean, aes(x = bmi, y = charges, color = plan_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by plan type", x = "BMI", y = "Insurance Charges") +
  theme_minimal()

b3<-ggplot(df_clean, aes(x = bmi, y = charges, color = chronic_condition)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by chronic conditions", x = "BMI", y = "Insurance Charges") +
  theme_minimal()

b4<-ggplot(df_clean, aes(x = bmi, y = charges, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of BMI and Insurance Charges by sex", x = "BMI", y = "Insurance Charges") +
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

model2a <- lm(charges ~ age + bmi + sex + smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + plan_type , data = df_clean)
summary(model2a)
#model2a <- model2a %>% tbl_regression() %>% as_flex_table()

model2b <- lm(charges ~ age_cat + bmi_cat +sex + smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + plan_type, data = df_clean)
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

### diagnostic plot 
par(mfrow = c(2, 2))
#plot(model2b)
plot(model2a)

model_beta<-lm.beta(model2a)

std_coefs <- model_beta$standardized.coefficients[-1]  # Remove intercept (NA)

# Build data frame
results_table <- data.frame(
  Variable = names(std_coefs),
  Std_Beta = round(as.numeric(std_coefs), 3),
  Abs_Std_Beta = round(abs(as.numeric(std_coefs)), 3)
) %>%
  arrange(desc(Abs_Std_Beta))

# Now kable works perfectly
standard<-kable(results_table, 
      digits = 3, 
      col.names = c("Variable", "standerdised beta", "Absolute standerdised beta"),
      caption = "Standardized Regression Coefficients (Ranked by Importance)")
standard
