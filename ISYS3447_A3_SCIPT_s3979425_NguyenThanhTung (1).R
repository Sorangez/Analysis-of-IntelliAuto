#install.packages("dplyr")
library("dplyr")

# Import the dataset
data <- ISYS3447_A3_IntelliAuto


# Factorize variables
data$Occup_n <- as.factor(data$Occup_n)
data$Sex <- as.factor(data$Sex)
data$MemUnion <- as.factor(data$MemUnion)
data$AwareI4 <- as.factor(data$AwareI4)

# Relevel factors
data$FutPromo <- factor(data$FutPromo, 
                        levels = c("5.V Unlikely", "4.Unlikely", "3.Not sure", "2.Likely", "1.V Likely"), 
                        ordered = FALSE)
data$FutPromo <- relevel(data$FutPromo, ref = "3.Not sure")

data$SexPromo <- factor(data$SexPromo, 
                        levels = c("2.Worse", "3.No Effect", "1.Better"), 
                        ordered = FALSE)
data$SexPromo <- relevel(data$SexPromo, ref = "3.No Effect")

# Add the derived column 'lessThan5' (to predict leaving within 5 years)
data$lessThan5 <- ifelse(data$EmpYears < 5, 1, 0)
data$lessThan5 <- as.factor(data$lessThan5)

# Define the logistic regression model using lessThan5 as the dependent variable
model <- glm(lessThan5 ~ WorkHrs + Age + WrkYears + Educ_Yrs + NumPromo + SexPromo, 
             data = data, 
             family = binomial)

# Print the summary to check p-values and t-values
summary(model)

# Calculate the predicted probabilities for leaving within 5 years
data$lessThan5 <- predict(model, data, type = "response")

# Convert probabilities to percentages for better readability
data$lessThan5yrs <- data$lessThan5 * 100

# Sort the data by the predicted probabilities of leaving within 5 years in decreasing order
data_sorted <- arrange(data, desc(lessThan5yrs))

# Select key columns (including columns not included in the model)
data_sorted %>%
  select(IdNum, WorkHrs, Occup_n, Age, Educ_Yrs, Sex, MemUnion, WrkYears, EmpYears, NumPromo, FutPromo, SexPromo, AwareI4, lessThan5yrs) %>%
  print(n=30)


# Extra
# Count the number of employees in each occupation
employee_count <- data %>%
  group_by(Occup_n) %>%
  summarise(count = n())

# Print the result
print(employee_count)
