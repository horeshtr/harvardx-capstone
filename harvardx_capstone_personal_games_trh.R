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

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(httr)


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
#   and User_Score needed to be formatted as numeric

data <- data %>% mutate(Year_of_Release = as.integer(Year_of_Release),
                        User_Score = as.double(User_Score))
  # Note: "Rating" is the maturity rating

### NEED TO INVESTIGATE NAs


#######################################################################
#   Create validation, train, and test datasets 
#######################################################################

# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding") 
val_index <- createDataPartition(y = data$Global_Sales, times = 1, p = 0.1, list = FALSE)
data_main <- data[-val_index,]
validation_temp <- data[val_index,]

# Confirm items are in both the validation and main data sets
validation <- validation_temp %>%
  semi_join(data_main, by = "Name") %>%
  semi_join(data_main, by = "Platform") %>% 
  semi_join(data_main, by = "Publisher") %>%
  semi_join(data_main, by = "Developer")

#Add the rows removed from the test_set back into train_set and remove unneeded objects
removed_val <- anti_join(validation_temp, validation)
data_main <- rbind(data_main, removed_val)

rm(val_index, validation_temp, removed_val)


# Split data into training and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data_main$Global_Sales, times = 1, p = 0.1, list = FALSE)
train_set <- data_main[-test_index,]
test_set_temp <- data_main[test_index,]

#Confirm items are in both the train and test sets
test_set <- test_set_temp %>%
  semi_join(train_set, by = "Name") %>%
  semi_join(train_set, by = "Platform") %>% 
  semi_join(train_set, by = "Publisher") %>%
  semi_join(train_set, by = "Developer")

#Add the rows removed from the test_set back into train_set and remove unneeded objects
removed_test <- anti_join(test_set_temp, test_set)
train_set <- rbind(train_set, removed_test)

rm(test_set_temp, test_index, removed_test)


#######################################################################
#   Exploratory Analyses of data_main
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
column_integrity[colSums(column_integrity) >= 0.9]

# Measure correlation of all numeric columns
data_main_num <- data_main[, sapply(data_main,is.double) | sapply(data_main,is.integer)]
num_cols <- data_main_num[,-c(1,3)]
res <- cor(num_cols, use = "complete.obs")
round(res, 2)

# Overall average of Critic Scores
mu_train_critic <- mean(train_set$Critic_Score)
mu_train_critic

# Overall average of User Scores
mu_train_user <- mean(train_set$User_Score)
mu_train_user

# Youngest and Oldest Release Years
min(data_main$Year_of_Release)
max(data_main$Year_of_Release)
