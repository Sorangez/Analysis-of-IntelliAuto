# Load required libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(gridExtra)
library(openxlsx)
library(car)
library(mice)

# Import the dataset
data <- ISYS3447_A2_IntelliAuto

# ----------------------------------------
# Q1: Data Cleaning and Plot Generation
# ----------------------------------------

# Detect and replace outliers for Q1 only
detect_outliers <- function(x) {  
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x[x < lower_bound | x > upper_bound])
}

# Copy original data for Q1 processing
data_q1 <- data

# Identifying errors and outliers
error_rows <- data_q1[data_q1$Age - data_q1$WrkYears < 16, ]
employment_greater_than_working <- data_q1[data_q1$EmpYears > data_q1$WrkYears, ]
age_outliers <- detect_outliers(data_q1$Age)
working_hours_outliers <- detect_outliers(data_q1$WorkHrs) 

# Impute NA for identified outliers and logical inconsistencies
data_q1_impute <- data_q1
data_q1_impute$WorkHrs[data_q1$WorkHrs %in% working_hours_outliers] <- NA
data_q1_impute$WrkYears[data_q1$Age - data_q1$WrkYears < 16] <- NA
data_q1_impute$EmpYears[data_q1$EmpYears > data_q1$WrkYears] <- NA

# Impute missing data for Q1
imputed_data_q1 <- mice(data_q1_impute, m = 10, method = 'pmm', maxit = 20, seed = 100) 
data_cleaned_q1 <- complete(imputed_data_q1)

# Q1-specific factor conversions
data_cleaned_q1$JobSatisfaction <- as.numeric(factor(data_cleaned_q1$JobSat, levels = c("1.Very Sat", "2.Mod Sat", "3.Little Dissat", "4.Very Dissat")))
data_cleaned_q1$RichWork <- as.numeric(factor(data_cleaned_q1$RichWork, levels = c("1.Yes", "2.No", "3.Not Sure")))
data_cleaned_q1$JobChar <- as.numeric(factor(data_cleaned_q1$JobChar, levels = c("1.High Inc", "2.Not Fired", "3.Flex Hours", "4.Opp Advance", "5.Enjoy Work")))
data_cleaned_q1$GetAhead <- as.numeric(factor(data_cleaned_q1$GetAhead, levels = c("1.Work", "2.Luck", "3.Work&Luck")))
data_cleaned_q1$FutPromo <- as.numeric(factor(data_cleaned_q1$FutPromo, levels = c("1.V Likely", "2.Likely", "3.Not sure", "4.Unlikely", "5.V Unlikely")))
data_cleaned_q1$SexPromo <- as.numeric(factor(data_cleaned_q1$SexPromo, levels = c("1.Better", "2.Worse", "3.No Effect")))
data_cleaned_q1$Advances <- as.numeric(factor(data_cleaned_q1$Advances, levels = c("1.Rapid", "2.Steady", "3.Same", "4.Lost")))
data_cleaned_q1$IDecide <- as.numeric(factor(data_cleaned_q1$IDecide, levels = c("1.Always", "2.Much", "3.Some", "4.Never")))
data_cleaned_q1$StayOrg <- as.numeric(factor(data_cleaned_q1$StayOrg, levels = c("1.V Likely", "2.Likely", "3.Not Sure", "4.Unlikely", "5.V Unlikely")))
data_cleaned_q1$UnManRel <- as.numeric(factor(data_cleaned_q1$UnManRel, levels = c("1.V Good", "2.Good", "3.So So", "4.Bad", "5.V Bad")))
data_cleaned_q1$CoWrkRel <- as.numeric(factor(data_cleaned_q1$CoWrkRel, levels = c("1.V Good", "2.Good", "3.So So", "4.Bad", "5.V Bad")))
data_cleaned_q1$Schooling <- factor(data_cleaned_q1$Schooling, levels = c("4.Not Imp'tant", "3.Some Imp'tant", "2.Imp'tant", "1.V Imp'tant"))
data_cleaned_q1$Training <- factor(data_cleaned_q1$Training, levels = c("4.Not Imp'tant", "3.Some Imp'tant", "2.Imp'tant", "1.V Imp'tant"))
data_cleaned_q1$AwareI4.0 <- factor(data_cleaned_q1$AwareI4.0, levels = c("No", "Yes"))
data_cleaned_q1$OrgMoney <- factor(data_cleaned_q1$OrgMoney, levels = c("No Budget", "Yes Budget"), labels = c("No Budget", "Yes Budget"))

# Generate plots for Q1
age_distribution_plot <- ggplot(data_cleaned_q1, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution of All Employees", x = "Age", y = "Count") +
  theme_minimal()

data_cleaned_q1 <- data_cleaned_q1 %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 25, 35, 50, Inf), labels = c("0-25", "26-35", "36-50", "51+"), right = FALSE))

age_group_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = AgeGroup)) +
  geom_bar(color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("0-25" = "blue", "26-35" = "green", "36-50" = "orange", "51+" = "red")) +
  labs(title = "Employee Count by Age Group", x = "Age Group", y = "Count") +
  theme_minimal()

work_hours_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, y = WorkHrs, fill = AgeGroup)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  stat_summary(fun.data = function(x) {
    return(data.frame(
      y = quantile(x, probs = 0.75, na.rm = TRUE),
      label = paste0("n=", length(x), "\nMed=", round(median(x, na.rm = TRUE), 1), "\nIQR=", round(IQR(x, na.rm = TRUE), 1))
    ))
  }, geom = "text", vjust = -0.5, color = "black") +
  scale_fill_manual(values = c("0-25" = "blue", "26-35" = "green", "36-50" = "orange", "51+" = "red")) +
  labs(title = "Work Hours by Age Group", x = "Age Group", y = "Work Hours") +
  theme_minimal() +
  theme(legend.position = "none")

correlation_education <- cor(data_cleaned_q1$Age, data_cleaned_q1$EducYrs, use = "complete.obs", method = "pearson")
age_education_plot <- ggplot(data_cleaned_q1, aes(x = Age, y = EducYrs)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = paste("Age vs. Education Years (Correlation:", round(correlation_education, 2), ")"), x = "Age", y = "Education Years") +
  theme_minimal()

age_sex_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = Sex)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Age Groups by Sex", x = "Age Group", y = "Count", fill = "Sex") +
  theme_minimal()

occupation_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = Occupn)) +
  geom_bar(color = "black", alpha = 0.7, position = "stack") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_stack(vjust  = 0.5)) +
  labs(title = "Occupations by Age Group", x = "Age Group", y = "Count", fill = "Occupation") +
  theme_minimal()

age_earners_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = factor(Earners))) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
  scale_fill_manual(values = c("1" = "green", "2" = "blue", "3" = "orange", "4" = "red", "5" = "purple"),
                    labels = c("1" = "1 Earner", "2" = "2 Earners", 
                               "3" = "3 Earners", "4" = "4 Earners", "5" = "5 Earners")) +
  labs(title = "Number of Earners by Age Group", x = "Age Group", y = "Count", fill = "Number of Earners") +
  theme_minimal()

occupations <- unique(data_cleaned_q1$Occupn)
occupation_plots <- list()
for (occupn in occupations) {
  plot <- ggplot(data_cleaned_q1 %>% filter(Occupn == occupn), aes(x = AgeGroup, fill = AgeGroup)) +
    geom_bar(color = "black", alpha = 0.7) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
    scale_fill_manual(values = c("0-25" = "blue", "26-35" = "green", "36-50" = "orange", "51+" = "red")) +
    labs(title = paste("Age Groups for Occupation:", occupn), x = "Age Group", y = "Count") +
    theme_minimal()
  occupation_plots[[occupn]] <- plot
}

correlation_pre_tax_inc <- cor(data_cleaned_q1$Age, data_cleaned_q1$PreTaxInc, use = "complete.obs", method = "pearson")
pre_tax_inc_plot <- ggplot(data_cleaned_q1, aes(x = Age, y = PreTaxInc)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = paste("Age vs. PreTax Income (Correlation:", round(correlation_pre_tax_inc, 2), ")"),
       x = "Age",
       y = "PreTax Income") +
  theme_minimal()

correlation_pre_tax_fam_inc <- cor(data_cleaned_q1$Age, data_cleaned_q1$PreTaxFamInc, use = "complete.obs", method = "pearson")
pre_tax_fam_inc_plot <- ggplot(data_cleaned_q1, aes(x = Age, y = PreTaxFamInc)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = paste("Age vs. PreTax Family Income (Correlation:", round(correlation_pre_tax_fam_inc, 2), ")"),
       x = "Age",
       y = "PreTax Family Income") +
  theme_minimal()

job_satisfaction_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = factor(JobSatisfaction))) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
  scale_fill_manual(values = c("1" = "green", "2" = "blue", "3" = "orange", "4" = "red"),
                    labels = c("1" = "Very satisfied", "2" = "Moderately satisfied", 
                               "3" = "A little dissatisfied", "4" = "Very dissatisfied")) +
  labs(title = "Job Satisfaction by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Job Satisfaction") +
  theme_minimal()

rich_work_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = factor(RichWork))) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
  scale_fill_manual(values = c("1" = "green", "2" = "red", "3" = "yellow"),
                    labels = c("1" = "Yes", "2" = "No", "3" = "Not Sure")) +
  labs(title = "Rich Work by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Rich Work") +
  theme_minimal()

job_char_plot <- ggplot(data_cleaned_q1, aes(x = AgeGroup, fill = factor(JobChar))) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 1), vjust = -0.5) +
  scale_fill_manual(values = c("1" = "green", "2" = "blue", "3" = "orange", "4" = "red", "5" = "purple"),
                    labels = c("1" = "High Income", "2" = "Not Fired", 
                               "3" = "Flexible Hours", "4" = "Opportunity for Advancement", "5" = "Enjoy Work")) +
  labs(title = "Job Characteristics by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Job Characteristic") +
  theme_minimal()

print(age_distribution_plot)
print(age_group_plot)
print(work_hours_plot)
print(age_education_plot)
print(age_sex_plot)
print(occupation_plot)
print(age_earners_plot)

# Print occupation-specific plots
for (occupn in occupations) {
  print(occupation_plots[[occupn]])
}

print(pre_tax_inc_plot)
print(pre_tax_fam_inc_plot)
print(job_satisfaction_plot)
print(rich_work_plot)
print(job_char_plot)


# ----------------------------------------
# Q2-Q4: Data Cleaning and Modeling
# ----------------------------------------

# Use the original dataset for Q2-Q4 without outlier removal
cleaned_data_q2_q4 <- na.omit(data)  # Remove rows with missing data
cleaned_data_q2_q4 <- cleaned_data_q2_q4[!(cleaned_data_q2_q4$EmpYears > cleaned_data_q2_q4$WrkYears |
                                             cleaned_data_q2_q4$WrkYears + 16 > cleaned_data_q2_q4$Age |
                                             cleaned_data_q2_q4$EducYrs > cleaned_data_q2_q4$Age |
                                             cleaned_data_q2_q4$PreTaxInc > cleaned_data_q2_q4$PreTaxFamInc), ]

# Factor Conversion and Reordering for Q2-Q4
cleaned_data_q2_q4$JobSat <- factor(cleaned_data_q2_q4$JobSat, levels = c("4.Very Dissat", "3.Little Dissat", "2.Mod Sat", "1.Very Sat"))
cleaned_data_q2_q4$RichWork <- factor(cleaned_data_q2_q4$RichWork, levels = c("2.No", "3.Not Sure", "1.Yes"))
cleaned_data_q2_q4$JobChar <- factor(cleaned_data_q2_q4$JobChar, levels = c("1.High Inc", "2.Not Fired", "3.Flex Hours", "4.Opp Advance", "5.Enjoy Work"))
cleaned_data_q2_q4$GetAhead <- factor(cleaned_data_q2_q4$GetAhead, levels = c("2.Luck", "1.Work", "3.Work&Luck"))
cleaned_data_q2_q4$MemUnion <- factor(cleaned_data_q2_q4$MemUnion, levels = c("No Union", "Yes Union"))
cleaned_data_q2_q4$FutPromo <- factor(cleaned_data_q2_q4$FutPromo, levels = c("5.V Unlikely", "4.Unlikely", "3.Not sure", "2.Likely", "1.V Likely"))
cleaned_data_q2_q4$SexPromo <- factor(cleaned_data_q2_q4$SexPromo, levels = c("2.Worse", "3.No Effect", "1.Better"))
cleaned_data_q2_q4$Advances <- factor(cleaned_data_q2_q4$Advances, levels = c("4.Lost", "3.Same", "2.Steady", "1.Rapid"))
cleaned_data_q2_q4$IDecide <- factor(cleaned_data_q2_q4$IDecide, levels = c("4.Never", "3.Some", "2.Much", "1.Always"))
cleaned_data_q2_q4$ProudOrg <- factor(cleaned_data_q2_q4$ProudOrg, levels = c("4.No Proud", "3.Ind Proud", "2.Some Proud", "1.V Proud"))
cleaned_data_q2_q4$StayOrg <- factor(cleaned_data_q2_q4$StayOrg, levels = c("5.V Unlikely", "4.Unlikely", "3.Not Sure", "2.Likely", "1.V Likely"))
cleaned_data_q2_q4$UnManRel <- factor(cleaned_data_q2_q4$UnManRel, levels = c("5.V Bad", "4.Bad", "3.So So", "2.Good", "1.V Good"))
cleaned_data_q2_q4$CoWrkRel <- factor(cleaned_data_q2_q4$CoWrkRel, levels = c("5.V Bad", "4.Bad", "3.So So", "2.Good", "1.V Good"))
cleaned_data_q2_q4$Schooling <- factor(cleaned_data_q2_q4$Schooling, levels = c("4.Not Imp'tant", "3.Some Imp'tant", "2.Imp'tant", "1.V Imp'tant"))
cleaned_data_q2_q4$Training <- factor(cleaned_data_q2_q4$Training, levels = c("4.Not Imp'tant", "3.Some Imp'tant", "2.Imp'tant", "1.V Imp'tant"))
cleaned_data_q2_q4$AwareI4.0 <- factor(cleaned_data_q2_q4$AwareI4.0, levels = c("Yes", "No"))

# ---------------------------------------------------
# Q2: Linear Model and Multicollinearity Check
# ---------------------------------------------------

# Fit the initial linear model with all variables
model_q2 <- lm(WrkYears ~ WorkHrs + Occupn + Age + EducYrs + Sex + Earners + 
                 PreTaxInc + PreTaxFamInc + JobSat + RichWork + JobChar + GetAhead + 
                 MemUnion + EmpYears + NumPromo + FutPromo + SexPromo + 
                 Advances + IDecide + OrgMoney + ProudOrg + StayOrg + UnManRel + 
                 CoWrkRel + Schooling + Training + AwareI4.0, data = cleaned_data_q2_q4)

# Summary and VIF for multicollinearity check
summary(model_q2)
vif(model_q2)

# Simplified model
selected_variables_q2 <- c("WorkHrs", "EmpYears", "Age", "JobChar", "AwareI4.0", "Sex")
final_model_q2 <- lm(WrkYears ~ EmpYears + JobChar + Age + AwareI4.0 + Sex, data = cleaned_data_q2_q4)
summary(final_model_q2)

# Q2 Scenarios for predictions
new_data_q2 <- data.frame(
  EmpYears = 4,
  Age = 30,
  JobChar = factor("2.Not Fired", levels = levels(cleaned_data_q2_q4$JobChar)),
  AwareI4.0 = factor("Yes", levels = levels(cleaned_data_q2_q4$AwareI4.0)),
  Sex = factor("Male", levels = levels(cleaned_data_q2_q4$Sex))
)

predictions_q2 <- predict(final_model_q2, newdata = new_data_q2)
cat("Q2 Scenario Predictions:\n")
print(predictions_q2)

# ---------------------------------------------------
# Q3: Iterative Variable Removal
# ---------------------------------------------------

# Iterative Variable Removal from the final Q2 model
variables_q3 <- names(final_model_q2$model)[-1]
remove_insignificant_variables <- function(model, variables) {
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients
  t_values <- coefficients[, "t value"]
  p_values <- coefficients[, "Pr(>|t|)"]
  
  for (var in variables) {
    related_terms <- grep(var, rownames(coefficients), value = TRUE)
    if (any(abs(t_values[related_terms]) < 1.96 | p_values[related_terms] > 0.05)) {
      model <- update(model, paste(". ~ . -", var))
      variables <- setdiff(variables, var)
      return(remove_insignificant_variables(model, variables))
    }
  }
  return(model)
}

final_model_q3 <- remove_insignificant_variables(final_model_q2, variables_q3)
summary(final_model_q3)

# Q3 Scenarios: Predicting WrkYears with final iterative model
new_data_q3 <- data.frame(
  EmpYears = 4,
  Age = 30,
  JobChar = factor("2.Not Fired", levels = levels(cleaned_data_q2_q4$JobChar)),
  AwareI4.0 = factor("Yes", levels = levels(cleaned_data_q2_q4$AwareI4.0)),
  Sex = factor("Male", levels = levels(cleaned_data_q2_q4$Sex))
)

predictions_q3 <- predict(final_model_q3, newdata = new_data_q3)
cat("Q3 Scenario Predictions with Iterative Model:\n")
print(predictions_q3)

# Calculate the ratio of union to non-union employees within each occupation
cleaned_data_ratio <- cleaned_data %>%
  group_by(Occupn_Full, MemUnion) %>%
  summarise(Count = n()) %>%
  mutate(Ratio = Count / sum(Count)) %>%
  ungroup()

# Plot Union Membership by Occupation with ratio displayed on the bars
ggplot(cleaned_data_ratio, aes(x = Occupn_Full, y = Count, fill = MemUnion)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(Ratio, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Union Membership by Occupation",
       x = "Occupation",
       y = "Count of Employees",
       fill = "Union Membership") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---------------------------------------------------
# Q4: Logistic Regression for Predicting Employment > 15 Years
# ---------------------------------------------------

# Create the binary variable 'Employed_15_Years' based on 'EmpYears'
cleaned_data_q2_q4$Employed_15_Years <- ifelse(cleaned_data_q2_q4$EmpYears > 15, 1, 0)

# Select relevant predictors for the logistic regression model
predictors_q4 <- c('WorkHrs', 'Earners', 'Training', 'Engagement')

# Subset data to include only the selected predictors and the target variable
data_subset_q4 <- cleaned_data_q2_q4[, c(predictors_q4, 'Employed_15_Years')]
data_subset_q4 <- na.omit(data_subset_q4)

# Fit the logistic regression model
logit_model_q4 <- glm(Employed_15_Years ~ ., data = data_subset_q4, family = binomial)

# Display the summary of the model
summary(logit_model_q4)

# Predict probabilities and binary outcomes
data_subset_q4$predicted_prob <- predict(logit_model_q4, type = "response")
data_subset_q4$predicted_outcome <- ifelse(data_subset_q4$predicted_prob > 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix_q4 <- table(data_subset_q4$Employed_15_Years, data_subset_q4$predicted_outcome)
print(confusion_matrix_q4)

# Calculate model accuracy
accuracy_q4 <- sum(diag(confusion_matrix_q4)) / sum(confusion_matrix_q4)
cat("Model Accuracy:", accuracy_q4, "\n")

# Loop over selected predictors to generate plots
for (predictor in predictors_q4) {
  p_q4 <- ggplot(data_subset_q4, aes_string(x = predictor, y = "predicted_prob")) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
    labs(title = paste("Logistic Regression: Probability of Employment > 15 Years vs.", predictor),
         x = predictor,
         y = "Predicted Probability") +
    theme_minimal()
  print(p_q4)
}

# Q4 Employee Scenarios Prediction
employee_scenarios_q4 <- data.frame(
  WorkHrs = c(50, 45, 40, 35, 30),
  Earners = c(1, 2, 2, 2, 2),
  Training = factor(c("3.Some Imp'tant", "2.Imp'tant", "1.V Imp'tant", "1.V Imp'tant", "1.V Imp'tant"), 
                    levels = c("4.Not Imp'tant", "3.Some Imp'tant", "2.Imp'tant", "1.V Imp'tant")),
  Engagement = c(3.5, 3.0, 2.5, 2.0, 1.5)
)
employee_scenarios_q4$predicted_prob <- predict(logit_model_q4, newdata = employee_scenarios_q4, type = "response")
employee_scenarios_q4$predicted_outcome <- ifelse(employee_scenarios_q4$predicted_prob > 0.5, 1, 0)
cat("Q4 Employee Scenarios Predictions for Employment > 15 Years:\n")
print(employee_scenarios_q4)

                                      
                                                                                            