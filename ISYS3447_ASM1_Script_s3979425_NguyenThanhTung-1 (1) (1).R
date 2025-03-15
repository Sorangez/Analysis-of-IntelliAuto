# INSTRUCTIONS !!!
# PLEASE IMPORT DATASET FROM EXCEL FILE BEFORE RUNNING THE SCRIPT.
# ENSURE THE EXCEL FILE IS NAMED ISYS3447_A1_IntelliAuto.
# ENSURE YOU HAVE NEEDED LIBRARIES INSTALLED.
# IF REQUIRED PACKAGED ARE NOT INSTALLED, RUN THE COMMANDS BELLOW WITHOUT THE #.
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("ggpmisc")

# Script written by Nguyen Thanh Tung s3979425

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpmisc)

# Function to load and clean data
load_and_clean_data <- function(df) {
  # Create a mapping for occupation names
  occupation_mapping <- c(
    "1.Manag" = "Managerial",
    "2.Prof'nal" = "Professional",
    "3.Tech/Sales" = "Technical/Sales",
    "4.Admin" = "Admin support",
    "5.Service" = "Service",
    "6.Prod'n" = "Production",
    "7.Laborer" = "Laborer"
  )
  
  # Replace the values in the Occup_n column
  df$Occup_n <- recode(df$Occup_n, !!!occupation_mapping)
  
  return(df)
}

# Load your data
df <- ISYS3447_A1_IntelliAuto

# Clean the data
df <- load_and_clean_data(df)

# Question 1
# Function to calculate statistics and plot working hours
plot_working_hours_stats <- function(df) {
  # Ensure WorkHrs is numeric
  if (!is.numeric(df$WorkHrs)) {
    df$WorkHrs <- as.numeric(df$WorkHrs)
    if (any(is.na(df$WorkHrs))) {
      warning("Some values in WorkHrs could not be converted to numeric and were set to NA")
    }
  }
  
  # Calculate occupation stats including mean
  occupation_stats <- df %>%
    group_by(Occup_n) %>%
    summarise(
      Mean = mean(WorkHrs, na.rm = TRUE),
      Median = median(WorkHrs, na.rm = TRUE),
      SD = sd(WorkHrs, na.rm = TRUE),
      Min = min(WorkHrs, na.rm = TRUE),
      Max = max(WorkHrs, na.rm = TRUE)
    )
  
  print(occupation_stats)
  
  # Plot histogram of working hours
  p_workhrs_hist <- ggplot(df, aes(x = WorkHrs)) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black") +
    geom_text(stat = 'bin', binwidth = 2, aes(label = ..count..), vjust = -0.5, color = "black") +
    labs(title = "Distribution of Working Hours", x = "Hours", y = "Frequency") +
    theme_minimal()
  
  print(p_workhrs_hist)  
  
  # Plot working hours by occupation
  p_workhrs_by_occupation <- ggplot(df, aes(x = Occup_n, y = WorkHrs, fill = Occup_n)) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
    stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "blue", fill = "blue") +
    labs(title = "Working Hours by Occupation", x = "Occupation", y = "Working Hours") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
    scale_fill_discrete(name = "Occupation")
  
  print(p_workhrs_by_occupation)
}

# Question 2
# Function to create age groups
create_age_groups <- function(df) {
  df <- df %>%
    mutate(AgeGroup = case_when(
      Age <= 34 ~ "Low (0-34)",
      Age <= 50 ~ "Middle (35-50)",
      TRUE ~ "High (51+)"
    ))
  
  df$AgeGroup <- factor(df$AgeGroup, levels = c("Low (0-34)", "Middle (35-50)", "High (51+)"))
  
  return(df)
}

# Function to plot age group counts
plot_age_group_counts <- function(df) {
  age_group_counts <- df %>%
    group_by(AgeGroup) %>%
    summarise(Count = n())
  
  p_age_group_bar <- ggplot(age_group_counts, aes(x = AgeGroup, y = Count, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    labs(title = "Employee Distribution by Age Group", x = "Age Group", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Age Group") +
    geom_text(aes(label = Count), vjust = -0.5)
  
  print(p_age_group_bar)
  
  return(age_group_counts)
}

# Function to plot scatter plot of ages
plot_age_scatter <- function(df, age_group_counts) {
  p_scatter_lines <- ggplot(df, aes(x = Age)) +
    geom_point(aes(y = 1), position = position_jitter(height = 0.1), alpha = 0.6) +  # Scatter plot (jitter) for individual points
    geom_vline(xintercept = c(34, 50), linetype = "dashed", color = "red", size = 1) +  # Vertical lines for age group boundaries
    labs(title = paste("Scatter Plot of Age\n",
                       "Low (0-34): ", age_group_counts$Count[1], " employees, ",
                       "Middle (35-50): ", age_group_counts$Count[2], " employees, ",
                       "High (51+): ", age_group_counts$Count[3], " employees", sep = ""),
         x = "Age", y = "") +
    scale_y_continuous(breaks = NULL) + 
    theme_minimal()
  
  print(p_scatter_lines)
}

# Function to summarize and plot distributions by age group
plot_distributions_by_age_group <- function(df) {
  # Summarize data by age group and gender
  gender_summary <- df %>%
    group_by(AgeGroup, Sex) %>%
    summarise(Count = n())
  
  # Summarize data by age group and union membership
  union_summary <- df %>%
    group_by(AgeGroup, MemUnion) %>%
    summarise(Count = n())
  
  # Summarize data by age group and occupation
  occupation_age_summary <- df %>%
    group_by(AgeGroup, Occup_n) %>%
    summarise(Count = n())
  
  # Gender distribution by age group
  p_gender <- ggplot(gender_summary, aes(x = AgeGroup, y = Count, fill = Sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Gender Distribution by Age Group", x = "Age Group", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Gender") +
    geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)
  
  print(p_gender)
  
  # Union membership distribution by age group
  p_union <- ggplot(union_summary, aes(x = AgeGroup, y = Count, fill = MemUnion)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Union Membership Distribution by Age Group", x = "Age Group", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Union Membership") +
    geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)
  
  print(p_union)
  
  # Occupation distribution by age group
  p_occupation_age_group <- ggplot(occupation_age_summary, aes(x = Occup_n, y = Count, fill = AgeGroup)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Occupation Distribution by Age Group", x = "Occupation", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Age Group") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5)  # Add labels to the bars
  
  print(p_occupation_age_group)
}

# Question 3
# Function to plot relationship between Age and Working Hours, Working Years, Employment Years
plot_relationship_with_age <- function(df) {
  # Ensure relevant columns are numeric
  df$WorkHrs <- as.numeric(df$WorkHrs)
  df$WrkYears <- as.numeric(df$WrkYears)
  df$EmpYears <- as.numeric(df$EmpYears)
  
  # Calculate overall correlation coefficients
  overall_corr_workhrs <- cor(df$Age, df$WorkHrs, use = "complete.obs")
  overall_corr_workyears <- cor(df$Age, df$WrkYears, use = "complete.obs")
  overall_corr_empyears <- cor(df$Age, df$EmpYears, use = "complete.obs")
  
  # Calculate correlation coefficients for each age group
  corr_workhrs <- df %>% group_by(AgeGroup) %>% summarise(corr = cor(Age, WorkHrs, use = "complete.obs"))
  corr_workyears <- df %>% group_by(AgeGroup) %>% summarise(corr = cor(Age, WrkYears, use = "complete.obs"))
  corr_empyears <- df %>% group_by(AgeGroup) %>% summarise(corr = cor(Age, EmpYears, use = "complete.obs"))
  
  # Plot relationship between Age and Working Hours
  p_age_workhrs <- ggplot(df, aes(x = Age, y = WorkHrs, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    annotate("text", x = 40, y = max(df$WorkHrs, na.rm = TRUE), 
             label = paste("Overall Corr:", round(overall_corr_workhrs, 2)), hjust = 1) +
    geom_text(data = corr_workhrs, aes(label = paste("Corr:", round(corr, 2)), 
                                       x = 40, y = max(df$WorkHrs, na.rm = TRUE) - 5 * as.numeric(AgeGroup)), 
              hjust = 1, vjust = 1) +
    labs(title = "Relationship between Age and Working Hours", x = "Age", y = "Working Hours") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group")
  
  print(p_age_workhrs)
  
  # Plot relationship between Age and Working Years
  p_age_workyears <- ggplot(df, aes(x = Age, y = WrkYears, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    annotate("text", x = 40, y = max(df$WrkYears, na.rm = TRUE), 
             label = paste("Overall Corr:", round(overall_corr_workyears, 2)), hjust = 1) +
    geom_text(data = corr_workyears, aes(label = paste("Corr:", round(corr, 2)), 
                                         x = 40, y = max(df$WrkYears, na.rm = TRUE) - 5 * as.numeric(AgeGroup)), 
              hjust = 1, vjust = 1) +
    labs(title = "Relationship between Age and Working Years", x = "Age", y = "Working Years") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group")
  
  print(p_age_workyears)
  
  # Plot relationship between Age and Employment Years
  p_age_empyears <- ggplot(df, aes(x = Age, y = EmpYears, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    annotate("text", x = 40, y = max(df$EmpYears, na.rm = TRUE), 
             label = paste("Overall Corr:", round(overall_corr_empyears, 2)), hjust = 1) +
    geom_text(data = corr_empyears, aes(label = paste("Corr:", round(corr, 2)), 
                                        x = 40, y = max(df$EmpYears, na.rm = TRUE) - 5 * as.numeric(AgeGroup)), 
              hjust = 1, vjust = 1) +
    labs(title = "Relationship between Age and Employment Years", x = "Age", y = "Employment Years") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group")
  
  print(p_age_empyears)
}

# Function to plot relationship between Age and Working Hours, Working Years, Employment Years by Occupation
plot_relationship_with_age_by_occupation <- function(df) {
  # Ensure relevant columns are numeric
  df$WorkHrs <- as.numeric(df$WorkHrs)
  df$WrkYears <- as.numeric(df$WrkYears)
  df$EmpYears <- as.numeric(df$EmpYears)
  
  # Calculate correlation coefficients for each occupation group
  corr_workhrs_occ <- df %>% group_by(Occup_n) %>% summarise(corr = cor(Age, WorkHrs, use = "complete.obs"))
  corr_workyears_occ <- df %>% group_by(Occup_n) %>% summarise(corr = cor(Age, WrkYears, use = "complete.obs"))
  corr_empyears_occ <- df %>% group_by(Occup_n) %>% summarise(corr = cor(Age, EmpYears, use = "complete.obs"))
  
  # Plot relationship between Age and Working Hours by Occupation
  p_age_workhrs_occ <- ggplot(df, aes(x = Age, y = WorkHrs, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Occup_n) +
    labs(title = "Relationship between Age and Working Hours by Occupation", x = "Age", y = "Working Hours") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group") +
    theme(legend.position = "bottom")
  
  print(p_age_workhrs_occ)
  
  # Plot relationship between Age and Working Years by Occupation
  p_age_workyears_occ <- ggplot(df, aes(x = Age, y = WrkYears, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Occup_n) +
    labs(title = "Relationship between Age and Working Years by Occupation", x = "Age", y = "Working Years") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group") +
    theme(legend.position = "bottom")
  
  print(p_age_workyears_occ)
  
  # Plot relationship between Age and Employment Years by Occupation
  p_age_empyears_occ <- ggplot(df, aes(x = Age, y = EmpYears, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Occup_n) +
    labs(title = "Relationship between Age and Employment Years by Occupation", x = "Age", y = "Employment Years") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group") +
    theme(legend.position = "bottom")
  
  print(p_age_empyears_occ)
  
  # Annotate correlation coefficients
  corr_annotations_workhrs <- df %>%
    group_by(Occup_n) %>%
    summarise(label = paste("Corr:", round(cor(Age, WorkHrs, use = "complete.obs"), 2)))
  
  corr_annotations_workyears <- df %>%
    group_by(Occup_n) %>%
    summarise(label = paste("Corr:", round(cor(Age, WrkYears, use = "complete.obs"), 2)))
  
  corr_annotations_empyears <- df %>%
    group_by(Occup_n) %>%
    summarise(label = paste("Corr:", round(cor(Age, EmpYears, use = "complete.obs"), 2)))
  
  # Plot relationship between Age and Working Hours by Occupation with correlation annotations
  p_age_workhrs_occ <- ggplot(df, aes(x = Age, y = WorkHrs, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Occup_n) +
    geom_text(data = corr_annotations_workhrs, aes(x = Inf, y = Inf, label = label), 
              hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
    labs(title = "Relationship between Age and Working Hours by Occupation", x = "Age", y = "Working Hours") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group") +
    theme(legend.position = "bottom")
  
  print(p_age_workhrs_occ)
  
  # Plot relationship between Age and Working Years by Occupation with correlation annotations
  p_age_workyears_occ <- ggplot(df, aes(x = Age, y = WrkYears, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Occup_n) +
    geom_text(data = corr_annotations_workyears, aes(x = Inf, y = Inf, label = label), 
              hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
    labs(title = "Relationship between Age and Working Years by Occupation", x = "Age", y = "Working Years") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group") +
    theme(legend.position = "bottom")
  
  print(p_age_workyears_occ)
  
  # Plot relationship between Age and Employment Years by Occupation with correlation annotations
  p_age_empyears_occ <- ggplot(df, aes(x = Age, y = EmpYears, color = AgeGroup)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Occup_n) +
    geom_text(data = corr_annotations_empyears, aes(x = Inf, y = Inf, label = label), 
              hjust = 1.1, vjust = 1.1, size = 3, color = "black") +
    labs(title = "Relationship between Age and Employment Years by Occupation", x = "Age", y = "Employment Years") +
    theme_minimal() +
    scale_color_discrete(name = "Age Group") +
    theme(legend.position = "bottom")
  
  print(p_age_empyears_occ)
}

# Question 4
# Function to convert Awareness column to factor
convert_awareness_to_factor <- function(df) {
  df$AwareI4 <- factor(df$AwareI4, levels = c("No", "Yes"), labels = c("Not Aware", "Aware"))
  return(df)
}

# Function to plot overall awareness
plot_overall_awareness <- function(df) {
  # Ensure Awareness is a factor
  df <- convert_awareness_to_factor(df)
  
  # Plot overall awareness of Industry 4.0
  p_overall_awareness <- ggplot(df, aes(x = AwareI4, fill = AwareI4)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
    labs(title = "Overall Awareness of Industry 4.0", x = "Awareness of Industry 4.0", y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("Not Aware" = "orange", "Aware" = "skyblue"))
  
  print(p_overall_awareness)
  
}

# Function to  plot relationship between Awareness of Industry 4.0 and other variables
plot_relationship_with_awareness <- function(df) {
  # Ensure Awareness is a factor
  df <- convert_awareness_to_factor(df)
  
  # Plot relationship between Awareness of Industry 4.0 and Gender
  p_awareness_gender <- ggplot(df, aes(x = Sex, fill = AwareI4)) +
    geom_bar(position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
    labs(title = "Relationship between Awareness of Industry 4.0 and Gender", x = "Gender", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Awareness of Industry 4.0")
  
  print(p_awareness_gender)
  
  # Plot relationship between Awareness of Industry 4.0 and Union membership
  p_awareness_union <- ggplot(df, aes(x = MemUnion, fill = AwareI4)) +
    geom_bar(position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
    labs(title = "Relationship between Awareness of Industry 4.0 and Union Membership", x = "Union Membership", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Awareness of Industry 4.0")
  
  print(p_awareness_union)
  
  # Plot relationship between Awareness of Industry 4.0 and Occupation
  p_awareness_occupation <- ggplot(df, aes(x = Occup_n, fill = AwareI4)) +
    geom_bar(position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
    labs(title = "Relationship between Awareness of Industry 4.0 and Occupation", x = "Occupation", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Awareness of Industry 4.0") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_awareness_occupation)
}

# Misc
# Function to plot awareness by age group
plot_awareness_by_age_group <- function(df) {
  # Ensure Awareness is a factor
  df <- convert_awareness_to_factor(df)
  
  # Plot awareness of Industry 4.0 by Age Group
  p_awareness_age_group <- ggplot(df, aes(x = AgeGroup, fill = AwareI4)) +
    geom_bar(position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
    labs(title = "Awareness of Industry 4.0 by Age Group", x = "Age Group", y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("Not Aware" = "orange", "Aware" = "skyblue")) +
    scale_fill_discrete(name = "Awareness of Industry 4.0")
  
  print(p_awareness_age_group)
}

# Function to plot gender distribution by occupation
plot_gender_by_occupation <- function(df) {
  # Plot gender distribution by occupation
  p_gender_occupation <- ggplot(df, aes(x = Occup_n, fill = Sex)) +
    geom_bar(position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
    labs(title = "Gender Distribution by Occupation", x = "Occupation", y = "Count") +
    theme_minimal() +
    scale_fill_discrete(name = "Gender") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_gender_occupation)
}

# Main script
df <- create_age_groups(df)
plot_working_hours_stats(df)
age_group_counts <- plot_age_group_counts(df)
plot_age_scatter(df, age_group_counts)
plot_distributions_by_age_group(df)
plot_relationship_with_age(df)
plot_relationship_with_age_by_occupation(df)
plot_overall_awareness(df)
plot_relationship_with_awareness(df)
plot_awareness_by_age_group(df)
plot_gender_by_occupation(df)


                            