##########################################################
#   HarvardX PH125.9x - Data Science: Capstone
#     Personal Project Submission
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


# Arabica Coffee Grading Dataset by Diego Volpatto via Kaggle:
# https://www.kaggle.com/datasets/volpatto/coffee-quality-database-from-cqi?select=arabica_data_cleaned.csv

data_raw <- read.csv(
  file = "/Users/GreenTea/Projects/harvardx-capstone/arabica_data_cleaned.txt",
  na.strings = c("", " ")
)
data <- as_tibble(data_raw)

glimpse(data)
head(data)

#######################################################################
#   Data Cleaning and Transformation
#######################################################################

# remove unneeded columns
data <- data %>% select(-Species, -ICO.Number, -Altitude, -Number.of.Bags, -Bag.Weight, 
                        -In.Country.Partner, -Expiration, -Certification.Body, -Certification.Contact, 
                        -Certification.Address)
head(data)

# convert text dates to actual dates
data <- data %>%
  mutate(
    Harvest.Year = as.numeric(str_sub(data$Harvest.Year, start = -5)),
    Grading.Date = mdy(Grading.Date)
  )
head(data)

# convert altitude values to numbers
data <- data %>%
  mutate(
    altitude_low_meters = ifelse(is.na(altitude_low_meters), altitude_low_meters, as.numeric(altitude_low_meters)),
    altitude_high_meters = ifelse(is.na(altitude_high_meters), altitude_high_meters, as.numeric(altitude_high_meters)),
    altitude_mean_meters = ifelse(is.na(altitude_mean_meters), altitude_mean_meters, as.numeric(altitude_mean_meters))
  )
head(data[,"altitude_low_meters"])

# convert altitude values to meters if unit_of_measurement is in feet
conv_coeff <- 0.3048    # meters per foot
replace_index <- data$unit_of_measurement == "ft"
data$altitude_low_meters[replace_index] <- (ifelse(is.na(data$altitude_low_meters[replace_index]), 
                                                   data$altitude_low_meters[replace_index], 
                                                   data$altitude_low_meters[replace_index] * conv_coeff                                                   ))
data$altitude_high_meters[replace_index] <- (ifelse(is.na(data$altitude_high_meters[replace_index]), 
                                                    data$altitude_high_meters[replace_index], 
                                                    data$altitude_high_meters[replace_index] * conv_coeff))
data$altitude_mean_meters[replace_index] <- (ifelse(is.na(data$altitude_mean_meters[replace_index]), 
                                                    data$altitude_mean_meters[replace_index], 
                                                    data$altitude_mean_meters[replace_index] * conv_coeff))

# NEED TO CONVERT unit_of_measurement FROM "ft" TO "m"


#######################################################################
#   Create validation, train, and test datasets 
#######################################################################

# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding") 
val_index <- createDataPartition(y = data$Total.Cup.Points, times = 1, p = 0.1, list = FALSE)
data_sub <- data[-val_index,]
validation <- data[val_index,]

############################################################################################
#Confirm all unique values from each required column are in both the datasets
validation_test <- validation %>%
  semi_join(data_sub, by = "X") %>%
  semi_join(data_sub, by = "Owner") %>%
  semi_join(data_sub, by = "Country.of.Origin") %>%
  semi_join(data_sub, by = "Region")

removed_test <- anti_join(validation_test, data_sub)
data_sub_test <- rbind(data_sub, removed_test)

# Need to figure out how to get all values across all data sets

############################################################################################

# Add rows removed from validation set back into main dataset
removed <- anti_join(validation, data_sub)
data_sub <- rbind(data_sub, removed)

# Remove unneeded objects
rm(val_index, removed)

# Split data into training and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data_sub$Total.Cup.Points, times = 1, p = 0.1, list = FALSE)
train_set <- data_sub[-test_index,]
test_set <- data_sub[test_index,]

#Add the rows removed from the test_set back into train_set and remove unneeded objects
removed <- anti_join(test_set, data_sub)
train_set <- rbind(train_set, removed)

# Remove unneeded objects
rm(test_index, removed)


# Create the function for calculating Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}



#######################################################################
#   Exploratory Analyses
#######################################################################

# Count distinct values for each column
col_index <- c(1:ncol(data_sub))
n_dist <- function(column){
  n_distinct(data_sub[,column])
}
num_dist_values <- sapply(col_index, n_dist)
num_dist_values <- matrix(num_dist_values, ncol = ncol(data_sub))
colnames(num_dist_values) <- colnames(data_sub)
num_dist_values <- as_tibble(num_dist_values)

# Measure completeness of data in each column
col_complete <- function(column){
  (nrow(data_sub) - sum(is.na(data_sub[,column]))) / nrow(data_sub)
}
column_integrity <- sapply(col_index, col_complete)
column_integrity <- matrix(column_integrity, ncol = ncol(data_sub))
colnames(column_integrity) <- colnames(data_sub)
column_integrity <- as_tibble(column_integrity)
column_integrity[colSums(column_integrity) >= 0.9]

# Measure correlation of all numeric columns
data_sub_num <- data_sub[, sapply(data_sub,is.double) | sapply(data_sub,is.integer)]
num_cols <- data_sub_num[,-c(1,3)]
res <- cor(num_cols, use = "complete.obs")
round(res, 2)

### What about correlation with categorical values (e.g., country, farm, etc.)??

# Overall average of Total.Cup.Points
mu_train <- mean(train_set$Total.Cup.Points)
mu_train

# Latest grading date
max(data_sub$Grading.Date)
    ### Could increase size of data set by scraping more recent coffees

# scatter plot of Total Cup Points vs. Cupper Points
cupper_points_fit <- lm(Total.Cup.Points ~ Cupper.Points, data = data_sub)
data_sub %>%
  ggplot(aes(Cupper.Points, Total.Cup.Points)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = cupper_points_fit$coefficients[2],
              intercept = cupper_points_fit$coefficients[1],
              color = "red")

n_distinct(data_sub$Cupper.Points)
mean(data_sub$Cupper.Points)
sd(data_sub$Cupper.Points)
cor(data_sub$Total.Cup.Points, data_sub$Cupper.Points)

# box plot of Total Cup Points vs. Category One Defects
data_sub %>%
  ggplot(aes(Total.Cup.Points, Category.One.Defects, group = Category.One.Defects)) +
  geom_boxplot()

# box plot of Total Cup Points vs. Category Two Defects
data_sub %>%
  ggplot(aes(Total.Cup.Points, Category.Two.Defects, group = Category.Two.Defects)) +
  geom_boxplot()

# box plot of Total Cup Points vs. Quakers
data_sub %>%
  ggplot(aes(Total.Cup.Points, Quakers, group = Quakers)) +
  geom_boxplot()

# box plot of Total Cup Points vs. Moisture level
data_sub %>%
  ggplot(aes(Total.Cup.Points, Moisture, group = Moisture)) +
  geom_boxplot() 

# scatter plot of Total Cup Points vs. Moisture level
moisture_fit <- lm(Total.Cup.Points ~ Moisture, data = data_sub)
data_sub %>%
  ggplot(aes(Moisture, Total.Cup.Points)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = moisture_fit$coefficients[2], 
              intercept = moisture_fit$coefficients[1],
              color = "red")


# scatter plot of Total Cup Points vs. Mean Elevation
elevation_fit <- lm(Total.Cup.Points ~ altitude_mean_meters, data = data_sub)
data_sub[is.na(data_sub$altitude_mean_meters)==0 & data_sub$altitude_mean_meters < 5000,] %>%
  ggplot(aes(altitude_mean_meters, Total.Cup.Points)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = elevation_fit$coefficients[2], 
              intercept = elevation_fit$coefficients[1],
              color = "red")


# --------------------------------------------------------------------- #
#   Model development using RMSE and various factors 
# ----------------------------------------------------------------------#

##########################
# Naive Model
##########################
# Rating difference between overall average of the train_set and true ratings of test_set
naive_rmse <- RMSE(test_set$Total.Cup.Points, mu_train)
rmse_results <- tibble(Method = "Naive Model", RMSE = naive_rmse)


#############################
# Country of Origin Effects
#############################
# country-specific effect 
country_effects <- train_set %>%
  group_by(Country.of.Origin) %>% 
  summarize(b_c = mean(Total.Cup.Points - mu_train))

# 1) predictions 
predicted_ratings_1 <- test_set %>% 
  left_join(country_effects, by = 'Country.of.Origin', na_matches = "never") %>%
  mutate(predictions = mu_train + b_c) %>%
  pull(predictions)

# check for missing values
sum(is.na(country_effects))
sum(is.na(predicted_ratings_1))
predicted_ratings_1[is.na(predicted_ratings_1)==1]
# Chose to replace them with the overall mean Total Cup Points
predicted_ratings_c1 <- ifelse(is.na(predicted_ratings_1), br, predicted_ratings_1)

# results
country_rmse <- RMSE(test_set$Total.Cup.Points, predicted_ratings_c1)
rmse_results <- rmse_results %>% add_row(Method = "Country Only", RMSE = country_rmse)


#############################
# Cupper Points Effects
#############################
# country-specific effect 
cupper_effects <- train_set %>%
  group_by(Cupper.Points) %>% 
  summarize(b_c = mean(Total.Cup.Points - mu_train))

# 2) predictions 
predicted_ratings_2 <- test_set %>% 
  left_join(cupper_effects, by = 'Cupper.Points', na_matches = "never") %>%
  mutate(predictions = mu_train + b_c) %>%
  pull(predictions)

# check for missing values
sum(is.na(cupper_effects))
sum(is.na(predicted_ratings_2))
predicted_ratings_2[is.na(predicted_ratings_2)==1]
# Chose to replace them with the overall mean Total Cup Points
predicted_ratings_c2 <- ifelse(is.na(predicted_ratings_2), br, predicted_ratings_2)

# results
cupper_rmse <- RMSE(test_set$Total.Cup.Points, predicted_ratings_c2)
rmse_results <- rmse_results %>% add_row(Method = "Cupper Points Only", RMSE = cupper_rmse)


# --------------------------------------------------------------------- #
#   Results Table
# ----------------------------------------------------------------------#
options(pillar.sigfig = 5, pillar.bold = TRUE)
rmse_results


# --------------------------------------------------------------------- #
#   Model development using lm() and predict() functions 
# ----------------------------------------------------------------------#

##########################
# Fit lm()
##########################
model_fit <- lm(formula = Total.Cup.Points ~ Country.of.Origin + Region + Moisture + Category.One.Defects +
            Category.Two.Defects + Cupper.Points, data = data_sub, na.action = na.omit)
summary(model_fit)

