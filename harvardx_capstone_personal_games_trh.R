##########################################################
#   HarvardX PH125.9x - Data Science: Capstone
#     Personal Project Submission - Video Games
#       Travis Horesh
#         November 2022
##########################################################

#######################################################################
# Read in raw dataset from downloaded txt file
#######################################################################

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


#######################################################################
#   Data Cleaning and Transformation
#######################################################################

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

# Later realized that game records are repeated for each platform
multi_platform <- data_clean %>% 
  group_by(Name) %>% 
  summarize(n_rec = n()) %>%
  filter(n_rec > 1) %>%
  select(Name)

data_multi_platform <- data_clean %>% 
  filter(Name %in% multi_platform$Name) %>% 
  arrange(Name, desc(Year_of_Release))
print(data_multi_platform, width = 1000)
# ***What about different years of release across different platforms?
# ***What about multiple developers or publishers? 

# Group platform-specific records into a single aggregated record for each game 
data_grouped <- data_clean %>% 
  group_by(Name) %>% 
  summarize(Platform = paste0(Platform, collapse = "/"),
            n_platforms = n(),
            First_Released = min(Year_of_Release),
            Original_Genre = first(Genre),
            Original_Publisher = first(Publisher),
            Original_Rating = first(Rating),
            Critic_Score = round(weighted.mean(Critic_Score, (Critic_Count / sum(Critic_Count))), 0),
            Critic_Count = round(mean(Critic_Count), 0),
            User_Score = round(weighted.mean(User_Score, (User_Count / sum(User_Count))), 0),
            User_Count = round(mean(User_Count), 0),
            Global_Sales = sum(Global_Sales))
print(data_grouped, width = 1000)

#######################################################################
#   Create validation, train, and test datasets 
#######################################################################

# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding") 
val_index <- createDataPartition(y = data_grouped$Global_Sales, times = 1, p = 0.1, list = FALSE)
data_main <- data_grouped[-val_index,]
validation_temp <- data_grouped[val_index,]

# Confirm items are in both the validation and main data sets
# validation <- validation_temp %>%
#   semi_join(data_grouped, by = "Name") #%>%
  # semi_join(data_main, by = "Platform") %>% 
  # semi_join(data_main, by = "Publisher") %>%
  # semi_join(data_main, by = "Developer") %>%
  # semi_join(data_main, by = "Genre") %>%

#Add the rows removed from the test_set back into train_set
# removed_val <- anti_join(validation_temp, validation)
# data_main <- rbind(data_grouped, removed_val)

# Remove unneeded objects
#rm(val_index, validation_temp, removed_val)


# Split data into training and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data_main$Global_Sales, times = 1, p = 0.1, list = FALSE)
train_set <- data_main[-test_index,]
test_set_temp <- data_main[test_index,]

#Confirm items are in both the train and test sets
# test_set <- test_set_temp %>%
#   semi_join(train_set, by = "Name")

#Add the rows removed from the test_set back into train_set and remove unneeded objects
# removed_test <- anti_join(test_set_temp, test_set)
# train_set <- rbind(train_set, removed_test)

# Remove unneeded objects
#rm(test_set_temp, test_index, removed_test)


#######################################################################
#   Exploratory Analyses
#######################################################################

# Count distinct values for each categorical column
data_main_chr <- data_main[, sapply(data_main,is.character)]
chr_col_index <- match(data_main_chr, data_main)
n_dist <- function(column){
  n_distinct(data_main[,column])
}
num_dist_values <- sapply(chr_col_index, n_dist)
num_dist_values <- matrix(num_dist_values, ncol = ncol(data_main_chr))
colnames(num_dist_values) <- colnames(data_main_chr)
num_dist_values <- as_tibble(num_dist_values)
num_dist_values

# Measure completeness of data in each column
col_index <- c(1:ncol(data_main))
col_complete <- function(column){
  (nrow(data_main) - sum(is.na(data_main[,column]))) / nrow(data_main)
}
column_integrity <- sapply(col_index, col_complete)
column_integrity <- matrix(column_integrity, ncol = ncol(data_main))
colnames(column_integrity) <- colnames(data_main)
column_integrity <- as_tibble(column_integrity)
print(column_integrity, width = 1000)
column_integrity[colSums(column_integrity) >= 0.9]

# Measure correlation of all numeric columns
data_main_num <- data_main[, sapply(data_main,is.double) | sapply(data_main,is.integer)]
res <- cor(data_main_num, use = "complete.obs")
round(res, 2)
  # Nothing appears to be highly correlated, but the most correlated variables are:
  #   n_platforms, Critic_Score, Critic_Count, and strangely, User_Count

# Youngest and Oldest Release Years
min(data_main$Year_of_Release)
max(data_main$Year_of_Release)

# Summary Stats on Global_Sales
min(data_main$Global_Sales)
max(data_main$Global_Sales)
mean(data_main$Global_Sales)
median(data_main$Global_Sales)


#### Need to filter out strata with few points to avoid highly variable estimates ####
###   Essentially, performing the work of regularization? ###

# I think I'm trying to look at the count of games with sales in a given strata to see
#   how far off my model results are; for example, if most games are selling 1.2 M units, but 
#   my model is only accurate within +/- 2.6 M units, that's not very helpful.

# Is this more effort than RMSE model with regularization? 

# Should I be looking at Sales, Scores, or something else to filter on?
data_grouped %>%
  group_by(Critic_Score) %>%
  summarize(Critic_Score = Critic_Score, n = n())
n_distinct(data_grouped$User_Score)

# Stratification by Sales
strata <- seq(floor(min(data_grouped$Global_Sales)), ceiling(max(data_grouped$Global_Sales)), 0.05)
sales_strata_dat <- data_grouped %>% 
  mutate(sales_strata_rnd = round(Global_Sales, 1),
         sales_strata_interval = strata[findInterval(Global_Sales, strata)]) %>%
  group_by(sales_strata_rnd) %>%
  mutate(n = n()) %>% filter(n >= 100)
# %>% filter(n >= 100)
  
sales_strata_dat %>%  
  ggplot(aes(sales_strata_rnd, label = ..count..)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "gray75") +
  geom_text(stat="bin", position = "stack") +
  geom_density(color = "black", fill = "gray", alpha = 0.6)
  # geom_point(alpha = 0.5) +
  # geom_smooth(method = "lm") +
  # facet_wrap( ~ sales_strata)

print(sales_strata_dat, width = 1000)


# --------------------------------------------------------------------- #
#   Distribution and Other Plots 
# ----------------------------------------------------------------------#

# Distribution by Global Sales
data_main %>% 
  ggplot(aes(x = Global_Sales)) +
  geom_histogram()


# Sales by Genre
data_main %>%
  ggplot(aes(x = Original_Genre, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# Sales by Rating
data_main %>%
  ggplot(aes(x = Original_Rating, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# scatter plot of Global_Sales vs. Critic_Score
data_main %>%
  ggplot(aes(x = Critic_Score, y = log(Global_Sales))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal() 

cor(data_grouped$Critic_Score, data_grouped$Global_Sales)
cor.test(data_grouped$Critic_Score, data_grouped$Global_Sales)

# scatter plot of Global_Sales vs. User_Score
data_main %>%
  ggplot(aes(x = User_Score, y = log(Global_Sales))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal() 

cor(data_grouped$User_Score, data_grouped$Global_Sales)
cor.test(data_grouped$User_Score, data_grouped$Global_Sales)


# --------------------------------------------------------------------- #
#   Model development using lm() and predict() functions 
# ----------------------------------------------------------------------#

##########################
# Fit lm()
##########################
# Fit a model based on numeric variables with highest correlation to Global Sales
num_model_fit <- lm(formula = Global_Sales ~ n_platforms + Critic_Score + Critic_Count + User_Count, 
                data = train_set, na.action = na.omit)
summary(num_model_fit)

# Investigate correlation between individual categorical variables and Global Sales
# Platform
platform_model_fit <- lm(formula = Global_Sales ~ Platform , data = train_set, na.action = na.omit)
summary(platform_model_fit)
(summary(platform_model_fit))$adj.r.squared

# Original_Genre
genre_model_fit <- lm(formula = Global_Sales ~ Original_Genre , 
                         data = train_set, na.action = na.omit)
summary(genre_model_fit)
(summary(genre_model_fit))$adj.r.squared

# Original_Publisher
publisher_model_fit <- lm(formula = Global_Sales ~ Original_Publisher , 
                         data = train_set, na.action = na.omit)
summary(publisher_model_fit)
(summary(publisher_model_fit))$adj.r.squared

# Original_Rating
rating_model_fit <- lm(formula = Global_Sales ~ Original_Rating , 
                       data = train_set, na.action = na.omit)
summary(rating_model_fit)
(summary(rating_model_fit))$adj.r.squared


# Fit a model that combines continuous and categorical variables most correlated to Global Sales
model_fit <- lm(formula = Global_Sales ~ n_platforms + User_Count + Critic_Score + Critic_Count, 
                data = train_set, na.action = na.omit)
summary(model_fit)


# --------------------------------------------------------------------- #
#   Model development using RMSE 
# ----------------------------------------------------------------------#

# Create the function for calculating Root Mean Squared Error (RMSE)
RMSE <- function(true_sales, predicted_sales){ 
  sqrt(mean((true_sales - predicted_sales) ^ 2))
}


# Overall average of Global_Sales in training set
mu_train_sales <- mean(train_set$Global_Sales)
mu_train_sales


##########################
# Naive Model
##########################
# Rating difference between overall average of the train_set and true ratings of test_set
naive_rmse <- RMSE(test_set_temp$Global_Sales, mu_train_sales)
rmse_results <- tibble(Method = "Naive Model", RMSE = naive_rmse)


##########################
# Major Correlated Effects
##########################

# Critic_Count Effects
critic_count_effects <- train_set %>%
  group_by(Critic_Count) %>% 
  summarize(b_cc = mean(Global_Sales - mu_train_sales))

# n_platforms Effects
n_platforms_effects <- train_set %>% 
  left_join(critic_count_effects, by = "Critic_Count", na_matches = "never") %>%
  group_by(n_platforms) %>% 
  summarize(b_np = mean(Global_Sales - mu_train_sales - b_cc))

# User_Count Effects
user_count_effects <- train_set %>% 
  left_join(critic_count_effects, by = "Critic_Count", na_matches = "never") %>%
  left_join(n_platforms_effects, by = "n_platforms", na_matches = "never") %>%
  group_by(User_Count) %>% 
  summarize(b_uc = mean(Global_Sales - mu_train_sales - b_cc - b_np))

# Critic_Score Effects
critic_score_effects <- train_set %>% 
  left_join(critic_count_effects, by = "Critic_Count", na_matches = "never") %>%
  left_join(n_platforms_effects, by = "n_platforms", na_matches = "never") %>%
  left_join(user_count_effects, by = "User_Count", na_matches = "never") %>%
  group_by(Critic_Score) %>% 
  summarize(b_cs = mean(Global_Sales - mu_train_sales - b_cc - b_np - b_uc))


# 1) predictions using combined effects
predicted_ratings_1 <- test_set_temp %>% 
  left_join(critic_count_effects, by = "Critic_Count", na_matches = "never") %>%
  left_join(n_platforms_effects, by = "n_platforms", na_matches = "never") %>%
  left_join(user_count_effects, by = "User_Count", na_matches = "never") %>%
  left_join(critic_score_effects, by = "Critic_Score", na_matches = "never") %>%
  mutate(predictions = mu_train_sales + b_cc + b_np + b_uc + b_cs) %>%
  pull(predictions)

# check for missing values
sum(is.na(predicted_ratings_1))
predicted_ratings_1[is.na(predicted_ratings_1)==1]
# Chose to replace NAs with the overall mean of Global_Sales
predicted_ratings_1c <- ifelse(is.na(predicted_ratings_1), mu_train_sales, predicted_ratings_1)
# Created an alternate predicted_ratings with all NAs replaced by 0
predicted_ratings_2c <- ifelse(is.na(predicted_ratings_1), 0, predicted_ratings_1)

# results for mu_train_sales
combined_rmse <- RMSE(test_set_temp$Global_Sales, predicted_ratings_1c)
rmse_results <- rmse_results %>% add_row(Method = "Combined Effects", RMSE = combined_rmse)

# results for NAs removed
combined_clean_rmse <- RMSE(test_set_temp$Global_Sales, predicted_ratings_2c)
rmse_results <- rmse_results %>% 
  add_row(Method = "Combined Effects No NAs", RMSE = combined_clean_rmse)



####################################
# Predict using results from lm()
###################################

# Test Set
test_predict <- predict.lm(model_fit, test_set_temp)
summary(test_predict)

test_set_rmse <- RMSE(test_set_temp$Global_Sales, test_predict)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_test", RMSE = test_set_rmse)

# Validation Set
val_predict <- predict.lm(model_fit, validation_temp)
summary(val_predict)

val_set_rmse <- RMSE(validation_temp$Global_Sales, val_predict)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_val", RMSE = val_set_rmse)


###############################################
# Results Table
###############################################
# Here are the aggregated results of all models above

options(pillar.sigfig = 5, pillar.bold = TRUE)
rmse_results

rmse_results[which.min(rmse_results$RMSE),]

# Code to clear rmse results table:
# rmse_results <- rmse_results %>% slice(-c(1:nrow(rmse_results)))