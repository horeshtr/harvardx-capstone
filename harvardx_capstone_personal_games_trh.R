##########################################################
#   HarvardX PH125.9x - Data Science: Capstone
#     Personal Project Submission - Video Games
#       Travis Horesh
#         November 2022
##########################################################

# --------------------------------------------------------------------- #
# Read in raw dataset from downloaded txt file
# --------------------------------------------------------------------- #

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridaet", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(httr)
library(dplyr)

# Video Game Sales with Ratings via Kaggle:
# https://www.kaggle.com/datasets/rush4ratio/video-game-sales-with-ratings

data_raw <- read.csv(
  file = "/Users/GreenTea/Projects/harvardx-capstone/Video_Games_Sales_as_at_22_Dec_2016.csv",
  na.strings = c("", " ")
)
data <- as_tibble(data_raw)

glimpse(data)
head(data)


# --------------------------------------------------------------------- #
#   Data Cleaning and Transformation
# --------------------------------------------------------------------- #

# Discovered during initial exploration that Year_of_Release needed to be formatted as numeric
#   and User_Score needed to be formatted as numeric and aligned with the same precision as 
#   the Critic_Score values

data <- data %>% mutate(Year_of_Release = as.integer(Year_of_Release),
                        User_Score = as.integer(as.double(User_Score) * 10))

# Remove all NAs
data_clean <- data %>% filter_all(~!is.na(.))

print(data_clean, width = 1000)
  
### NA Investigation Notes:
  # User_Score appears to be the least complete column
  # Decided to subset data to only complete records for simplicity

# Add a column for the count of platforms on which each game was replaced
data_clean <- data_clean %>%
  group_by(Name) %>%
  mutate(n_platforms = n()) %>%
  ungroup(Name)

# Examine whether game records are repeated for each platform
multi_platform <- data_clean %>% 
  group_by(Name) %>% 
  summarize(n_rec = n()) %>%
  filter(n_rec > 1) %>%
  select(Name)

# created a subset of games that were released across multiple platforms
data_multi_platform <- data_clean %>% 
  filter(Name %in% multi_platform$Name) %>% 
  arrange(Name, desc(Year_of_Release))

print(data_multi_platform, width = 1000)


# Examine whether there are different years of release across different platforms
multi_year <- data_clean %>%
  select(Name, Year_of_Release) %>% 
  group_by(Name) %>% 
  summarize(n_years = n_distinct(Year_of_Release),
            years_span =(max(Year_of_Release) - min(Year_of_Release))) %>%
  filter(n_years > 1) %>%
  arrange(desc(n_years))

data_clean[data_clean$Name %in% multi_year$Name, ]  
data_clean[data_clean$Name == 'MotoGP', ]
  # we see that some games are released across multiple platforms, but over a period of years

# Examine whether there are multiple developers for a single game 
multi_dev <- data_clean %>%
  select(Name, Developer) %>% 
  group_by(Name) %>% 
  summarize(n_devs = n_distinct(Developer)) %>%
  filter(n_devs > 1) %>%
  arrange(desc(n_devs))

data_clean[data_clean$Name %in% multi_dev$Name, ]  
print(data_clean[data_clean$Name == 'Need For Speed: Undercover', ] , width = 1000)
  # we can see that some games are produced by a different developer for each different platform

# Examine whether there are multiple publishers for a single game 
multi_pub <- data_clean %>%
  select(Name, Publisher) %>% 
  group_by(Name) %>% 
  summarize(n_pubs = n_distinct(Publisher)) %>%
  filter(n_pubs > 1) %>%
  arrange(desc(n_pubs))

data_clean[data_clean$Name %in% multi_pub$Name, ]  
data_clean[data_clean$Name == 'MotoGP', ]
  # we can see that some games are released by a different publisher for each different platform

# Review counts in categorical variables
data_clean %>% count(Rating)
# Decided to relabel games with AO, K-A, and RP rating, which have only 1 record each
data_clean <- data_clean %>%
  mutate(Rating = ifelse(! Rating %in% c("E","E10+","M","T"), "Other", Rating))
data_clean %>% count(Rating)

# Examine top publishers
top_10_pubs <- (data_clean %>% group_by(Publisher) %>%
                  summarize(total_sales = sum(Global_Sales)) %>% arrange(desc(total_sales)) %>% 
                  top_n(10) %>% distinct(Publisher))$Publisher

# Examine top developers
top_10_devs <- (data_clean %>% group_by(Developer) %>%
                  summarize(total_sales = sum(Global_Sales)) %>% arrange(desc(total_sales)) %>% 
                  top_n(10) %>% distinct(Developer))$Developer


data_clean <- data_clean %>% 
  mutate(top_pub = ifelse(Publisher %in% top_10_pubs, 1, 0),
         top_dev = ifelse(Developer %in% top_10_devs, 1, 0))

print(data_clean, width = 1000)

# --------------------------------------------------------------------- #
#   Exploratory Analyses
# --------------------------------------------------------------------- #

# Count distinct values for each categorical column
data_clean_chr <- data_clean[, sapply(data_clean , is.character)]
chr_col_index <- match(data_clean_chr, data_clean)
n_dist <- function(column){
  n_distinct(data_clean[,column])
}
num_dist_values <- sapply(chr_col_index, n_dist)
num_dist_values <- matrix(num_dist_values, ncol = ncol(data_clean_chr))
colnames(num_dist_values) <- colnames(data_clean_chr)
num_dist_values <- as_tibble(num_dist_values)
num_dist_values

# Measure completeness of data in each column
col_index <- c(1:ncol(data_clean))
col_complete <- function(column){
  (nrow(data_clean) - sum(is.na(data_clean[,column]))) / nrow(data_clean)
}
column_integrity <- sapply(col_index, col_complete)
column_integrity <- matrix(column_integrity, ncol = ncol(data_clean))
colnames(column_integrity) <- colnames(data_clean)
column_integrity <- as_tibble(column_integrity)
print(column_integrity, width = 1000)
column_integrity[colSums(column_integrity) >= 0.9]

# Measure correlation of all numeric columns
data_clean_num <- data_clean[, sapply(data_clean,is.double) | sapply(data_clean,is.integer)]
res <- cor(data_clean_num, use = "complete.obs")
round(res, 2)
# Nothing appears to be highly correlated, but the most correlated variables are:
#   n_platforms, Critic_Score, Critic_Count, and strangely, User_Count

# Summary of Release Years
summary(data_clean$Year_of_Release)

# Summary Stats on Global_Sales
summary(data_clean$Global_Sales)

# --------------------------------------------------------------------- #
#   Plots and Further Analyses 
# ----------------------------------------------------------------------#

# Distribution of Global Sales
data_clean %>% 
  ggplot(aes(x = Global_Sales)) +
  geom_histogram()

# Distribution of Global Sales on Logarithmic Scale
data_clean %>% 
  ggplot(aes(x = Global_Sales)) +
  geom_histogram() +
  scale_x_log10()

# Distribution by Year of Release
data_clean %>% group_by(Year_of_Release) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(Year_of_Release, n), stat = "identity", 
           fill = "gray75") + theme(axis.text.x = element_text(angle = 90))

# Sales by Platform
data_clean %>%
  ggplot(aes(x = Platform, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
    # Consider collapsing into major platforms


# Sales by Genre
data_clean %>%
  ggplot(aes(x = Genre, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# Sales by Rating
data_clean %>%
  ggplot(aes(x = Rating, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# Comparison of Critic_Score and User_Score versus Global_Sales
line_colors <- c("Critic score" = "#20A387FF", "User score" = "#95D840FF")
data_clean %>%
  ggplot() +
  geom_smooth(aes(Critic_Score, Global_Sales, color = "Critic score")) + 
  geom_smooth(aes(User_Score, Global_Sales, color = "User score")) +
  labs(color = "") + xlab("Score") + ylab("Global Sales") + 
  scale_color_manual(values = line_colors)


# --------------------------------------------------------------------- #
#   Create validation, train, and test datasets 
# --------------------------------------------------------------------- #

# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding") 
val_index <- createDataPartition(y = data_clean$Global_Sales, times = 1, p = 0.1, list = FALSE)
data_split <- data_clean[-val_index,]
validation <- data_clean[val_index,]

# Confirm items are in both the validation and main data sets
total_data_split <- rbind(data_split, validation)
for (f in 1:length(names(total_data_split))) {
  levels(data_split[, f]) <- levels(total_data_split[, f])
}


# Split data into training and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data_split$Global_Sales, times = 1, p = 0.1, list = FALSE)
train_set <- data_split[-test_index,]
test_set <- data_split[test_index,]

#Confirm items are in both the train and test sets
total_data_train <- rbind(train_set, test_set)
for (f in 1:length(names(total_data_train))) {
  levels(train_set[, f]) <- levels(total_data_train[, f])
}

glimpse(train_set)


# --------------------------------------------------------------------- #
#   Model development using lm() and predict() functions 
# ----------------------------------------------------------------------#

##########################
# Fit lm()
##########################
# Fit a model based on numeric variables with highest correlation to Global Sales
model_fit <- lm(formula = Global_Sales ~ Critic_Score + User_Score + Genre +
                      Year_of_Release + n_platforms + Critic_Count + User_Count + Rating +
                      top_pub + top_dev , data = train_set, na.action = na.omit)
summary(model_fit)

############################################
# Fit lm() using logarithmic Global Sales
############################################
# Fit a model based on numeric variables with highest correlation to Global Sales
model_fit_log <- lm(formula = log(Global_Sales) ~ Critic_Score + User_Score + Genre +
                  Year_of_Release + n_platforms + Critic_Count + User_Count + Rating +
                  top_pub + top_dev , data = train_set, na.action = na.omit)
summary(model_fit_log)

##########################
# Fit lm() minimalist
##########################
# Fit a model based on numeric variables with highest correlation to Global Sales
model_fit_min <- lm(formula = Global_Sales ~ Critic_Score + Genre + Year_of_Release + Platform + 
                  Rating + top_pub + top_dev , data = train_set, na.action = na.omit)
summary(model_fit_min)



##########################
# RMSE Function
##########################
# Create the function for calculating Root Mean Squared Error (RMSE)
RMSE <- function(true_sales, predicted_sales){ 
  sqrt(mean((true_sales - predicted_sales) ^ 2))
}


# --------------------------------------------------------------------- #
#   Model development using RMSE 
# ----------------------------------------------------------------------#

# Overall average of Global_Sales in training set
mu_train_sales <- mean(train_set$Global_Sales)
mu_train_sales


##########################
# Naive Model
##########################
# Rating difference between overall average of the train_set and true ratings of test_set
naive_rmse <- RMSE(test_set$Global_Sales, mu_train_sales)
rmse_results <- tibble(Method = "Naive Model", RMSE = naive_rmse)


#######################################################
# Predict using results from various linear models
#######################################################

# Test Set on Main Model
test_predict <- predict.lm(model_fit, test_set)
summary(test_predict)

test_set_rmse <- RMSE(test_set$Global_Sales, test_predict)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_test", RMSE = test_set_rmse)

# Test Set using Log Model
test_predict_log <- predict.lm(model_fit_log, test_set)
summary(test_predict_log)

test_set_log_rmse <- RMSE(log(test_set$Global_Sales), test_predict_log)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_log_test", RMSE = test_set_log_rmse)

# Test Set using Minimal Model
test_predict_min <- predict.lm(model_fit_min, test_set)
summary(test_predict_min)

test_set_min_rmse <- RMSE(test_set$Global_Sales, test_predict_min)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_min_test", RMSE = test_set_min_rmse)

    # The more complete model using logarithmic sales data performed the best based on RMSE, with
    # both the non-log complete model and the "minimalist" model beating the naive model on RMSE


# Validation Set using complete logarithmic model
val_predict_log <- predict.lm(model_fit_log, validation)
summary(val_predict_log)

val_set_log_rmse <- RMSE(log(validation$Global_Sales), val_predict_log)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_log_val", RMSE = val_set_log_rmse)

# Validation Set using complete model
val_predict <- predict.lm(model_fit, validation)
summary(val_predict)

val_set_rmse <- RMSE(log(validation$Global_Sales), val_predict)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_val", RMSE = val_set_rmse)

    # We see that the complete model using logarithmic sales data performed better, even better than
    # on the test set data, based on RMSE, while the non-log complete model actually performed worse


###############################################
# Results Table
###############################################
# Here are the aggregated results of all models above

options(pillar.sigfig = 5, pillar.bold = TRUE)
rmse_results

rmse_results[which.min(rmse_results$RMSE),]

# Code to clear rmse results table:
# rmse_results <- rmse_results %>% slice(-c(1:nrow(rmse_results)))