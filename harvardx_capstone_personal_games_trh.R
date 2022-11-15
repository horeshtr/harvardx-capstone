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
            Original_Developer = first(Developer),
            Original_Rating = first(Rating),
            Critic_Score = round(weighted.mean(Critic_Score, (Critic_Count / sum(Critic_Count))), 0),
            Critic_Count = round(mean(Critic_Count), 0),
            User_Score = round(weighted.mean(User_Score, (User_Count / sum(User_Count))), 0),
            User_Count = round(mean(User_Count), 0),
            Global_Sales = sum(Global_Sales))
print(data_grouped, width = 1000)

# --------------------------------------------------------------------- #
#   Exploratory Analyses
# --------------------------------------------------------------------- #

# Count distinct values for each categorical column
data_group_chr <- data_grouped[, sapply(data_grouped , is.character)]
chr_col_index <- match(data_group_chr, data_grouped)
n_dist <- function(column){
  n_distinct(data_grouped[,column])
}
num_dist_values <- sapply(chr_col_index, n_dist)
num_dist_values <- matrix(num_dist_values, ncol = ncol(data_group_chr))
colnames(num_dist_values) <- colnames(data_group_chr)
num_dist_values <- as_tibble(num_dist_values)
num_dist_values

# Measure completeness of data in each column
col_index <- c(1:ncol(data_grouped))
col_complete <- function(column){
  (nrow(data_grouped) - sum(is.na(data_grouped[,column]))) / nrow(data_grouped)
}
column_integrity <- sapply(col_index, col_complete)
column_integrity <- matrix(column_integrity, ncol = ncol(data_grouped))
colnames(column_integrity) <- colnames(data_grouped)
column_integrity <- as_tibble(column_integrity)
print(column_integrity, width = 1000)
column_integrity[colSums(column_integrity) >= 0.9]

# Measure correlation of all numeric columns
data_group_num <- data_grouped[, sapply(data_grouped,is.double) | sapply(data_grouped,is.integer)]
res <- cor(data_group_num, use = "complete.obs")
round(res, 2)
# Nothing appears to be highly correlated, but the most correlated variables are:
#   n_platforms, Critic_Score, Critic_Count, and strangely, User_Count

# Youngest and Oldest Release Years
min(data_grouped$Year_of_Release)
max(data_grouped$Year_of_Release)

# Summary Stats on Global_Sales
min(data_grouped$Global_Sales)
max(data_grouped$Global_Sales)
mean(data_grouped$Global_Sales)
median(data_grouped$Global_Sales)

# Review counts in categorical variables
data_grouped %>% count(Original_Rating)
    # should drop or regroup K-A and RP, which have 1 record each

data_grouped %>% count(Original_Publisher)

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
data_grouped %>% 
  ggplot(aes(x = Global_Sales)) +
  geom_histogram()

# Distribution by logarithmic Global Sales
data_grouped %>% 
  ggplot(aes(x = Global_Sales)) +
  geom_histogram() +
  scale_x_log10()

# Distribution by Year of Release
data_grouped %>% group_by(First_Released) %>% 
  count() %>% ggplot() + 
  geom_bar(aes(First_Released, n), stat = "identity", 
           fill = "gray75") + theme(axis.text.x = element_text(angle = 90))

# Sales by Platform
data_grouped %>%
  ggplot(aes(x = Platform, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
    # Consider collapsing into major platforms


# Sales by Genre
data_grouped %>%
  ggplot(aes(x = Original_Genre, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# Sales by Rating
data_grouped %>%
  ggplot(aes(x = Original_Rating, y = Global_Sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) 


# scatter plot of Global_Sales vs. Critic_Score
line_colors <- c("Critic score" = "#20A387FF", "User score" = "#95D840FF")
data_grouped %>%
  ggplot() +
  geom_smooth(aes(Critic_Score, Global_Sales, color = "Critic score")) + 
  geom_smooth(aes(User_Score, Global_Sales, color = "User score")) +
  labs(color = "") + xlab("Score") + ylab("Global sales") + 
  scale_color_manual(values = line_colors)

cor(data_grouped$Critic_Score, data_grouped$Global_Sales)
cor.test(data_grouped$Critic_Score, data_grouped$Global_Sales)

# scatter plot of Global_Sales vs. User_Score
data_grouped %>%
  ggplot(aes(x = User_Score, y = log(Global_Sales))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_minimal() 

# Examine top publishers
top_10_pubs <- (data_grouped %>% group_by(Original_Publisher) %>%
                     summarize(total_sales = sum(Global_Sales)) %>% arrange(desc(total_sales)) %>% 
                     top_n(10) %>% distinct(Original_Publisher))$Original_Publisher

# Examine top developers
top_10_devs <- (data_grouped %>% group_by(Original_Developer) %>%
                  summarize(total_sales = sum(Global_Sales)) %>% arrange(desc(total_sales)) %>% 
                  top_n(10) %>% distinct(Original_Developer))$Original_Developer


data_grouped <- data_grouped %>% 
  mutate(top_pub = ifelse(Original_Publisher %in% top_10_pubs, TRUE, FALSE),
         top_dev = ifelse(Original_Developer %in% top_10_devs, TRUE, FALSE))


# --------------------------------------------------------------------- #
#   Create validation, train, and test datasets 
# --------------------------------------------------------------------- #

# Validation set will be 10% of the data
set.seed(1, sample.kind="Rounding") 
val_index <- createDataPartition(y = data_grouped$Global_Sales, times = 1, p = 0.1, list = FALSE)
data_split <- data_grouped[-val_index,]
validation <- data_grouped[val_index,]

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


# --------------------------------------------------------------------- #
#   Model development using lm() and predict() functions 
# ----------------------------------------------------------------------#

##########################
# Fit lm()
##########################
# Fit a model based on numeric variables with highest correlation to Global Sales
model_fit <- lm(formula = log(Global_Sales) ~ Critic_Score + User_Score + Original_Genre +
                      First_Released + n_platforms + Critic_Count + User_Count + Original_Rating +
                      top_pub + top_dev,data = train_set, na.action = na.omit)
summary(model_fit)


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


####################################
# Predict using results from lm()
###################################

# Test Set
test_predict <- predict.lm(model_fit, test_set)
summary(test_predict)

test_set_rmse <- RMSE(log(test_set$Global_Sales), test_predict)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_test", RMSE = test_set_rmse)

# Validation Set
val_predict <- predict.lm(model_fit, validation)
summary(val_predict)

val_set_rmse <- RMSE(log(validation$Global_Sales), val_predict)
rmse_results <- rmse_results %>% add_row(Method = "lm_fit_val", RMSE = val_set_rmse)

## log(Global_Sales) versus regular? 

###############################################
# Results Table
###############################################
# Here are the aggregated results of all models above

options(pillar.sigfig = 5, pillar.bold = TRUE)
rmse_results

rmse_results[which.min(rmse_results$RMSE),]

# Code to clear rmse results table:
# rmse_results <- rmse_results %>% slice(-c(1:nrow(rmse_results)))