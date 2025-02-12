install.packages("reshape2")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("caret")
install.packages("naniar")

library(reshape2)
library(dplyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(naniar)

file_path <- "C:/Users/F. M SHARIAR/Desktop/9th Semester/Data Science/Mid/Project/Midterm_Dataset_Section(C).xlsx"
my_data <- read_excel(file_path)
View(my_data)
data<-my_data

print(head(data))

gg_miss_var(my_data)

data_no_missing <- na.omit(data)
print(head(data_no_missing))

data_median_mode <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
print(head(data_median_mode))
print(any(is.na(data_median_mode %>% select(-loan_status))))

data_mean_mode <- data %>%
  mutate(across(where(is.numeric) & !all_of("loan_status"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
print(head(data_mean_mode))

data_min_unknown <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), min(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .)))
print(head(data_min_unknown))

data <- data_median_mode

print(unique(data$person_home_ownership))

ownership_corrections <- c("RENTT" = "RENT", "OOWN" = "OWN")
data <- data %>%
  mutate(person_home_ownership = toupper(person_home_ownership)) %>%
  mutate(person_home_ownership = case_when(
    person_home_ownership %in% names(ownership_corrections) ~ ownership_corrections[person_home_ownership],
    person_home_ownership %in% c("RENT", "OWN", "MORTGAGE", "OTHER") ~ person_home_ownership,
    TRUE ~ "UNKNOWN"
  ))
print(unique(data$person_home_ownership))

data$loan_status <- as.factor(data$loan_status)

class_counts <- table(data$loan_status)
minority_class <- names(which.min(class_counts))
majority_class <- names(which.max(class_counts))

minority_data <- data %>% filter(loan_status == minority_class)
oversampled_minority_data <- minority_data %>% slice_sample(n = class_counts[majority_class], replace = TRUE)
balanced_data <- bind_rows(oversampled_minority_data, data %>% filter(loan_status == majority_class))

majority_data <- data %>% filter(loan_status == majority_class)
undersampled_majority_data <- majority_data %>% slice_sample(n = class_counts[minority_class])
balanced_data_undersample <- bind_rows(undersampled_majority_data, data %>% filter(loan_status == minority_class))

print(table(balanced_data$loan_status))

print(table(balanced_data_undersample$loan_status))

ggplot(data, aes(x = loan_status, fill = loan_status)) +
  geom_bar() +
  labs(title = "Loan Status Distribution (Original)", x = "Loan Status", y = "Count") +
  theme_minimal()

ggplot(balanced_data, aes(x = loan_status, fill = loan_status)) +
  geom_bar() +
  labs(title = "Loan Status Distribution (Oversample)", x = "Loan Status", y = "Count") +
  theme_minimal()

ggplot(balanced_data_undersample, aes(x = loan_status, fill = loan_status)) +
  geom_bar() +
  labs(title = "Loan Status Distribution (Undersampled)", x = "Loan Status", y = "Count") +
  theme_minimal()

data <- data %>% distinct()

numeric_columns <- sapply(data, is.numeric)
data[, numeric_columns] <- lapply(data[, numeric_columns], function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  x
})

data$person_income <- (data$person_income - min(data$person_income, na.rm = TRUE)) / 
  (max(data$person_income, na.rm = TRUE) - min(data$person_income, na.rm = TRUE))

data$loan_category <- cut(data$loan_amnt, breaks=c(0, 5000, 20000, Inf), labels=c("Low", "Medium", "High"))
data$person_gender_numeric <- as.numeric(factor(data$person_gender))

ggplot(data, aes(x = person_income, fill = loan_status)) + geom_histogram(bins = 30, alpha = 0.5) +
  labs(title = "Histogram of Person Income by Loan Status", x = "Person Income", y = "Count") +
  theme_minimal()

numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data, use = "complete.obs")
cor_melted <- melt(cor_matrix)
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = person_home_ownership, fill = loan_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Loan Status by Home Ownership", x = "Home Ownership", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

write.csv(data, "C:/Users/F. M SHARIAR/Desktop/9th Semester/Data Science/Mid/Project/Project1.csv", row.names = FALSE)

str(data)

summary(data)
