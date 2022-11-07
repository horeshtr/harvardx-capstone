##########################################################
#   HarvardX PH125.9x - Data Science: Capstone
#     Travis Horesh
#       June - November 2022
##########################################################

#######################################################################
# Create edx dataset and validation dataset (final hold-out test set)
#######################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of the MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Remove unneeded objects
rm(dl, ratings, movies, test_index, temp, movielens, removed)


#############################################################################
#     QUIZ QUESTIONS

# # Q1
# # How many rows and columns are there in the edx dataset?
# nrow(edx)
# ncol(edx)
# 
# # Q2
# # How many zeros were given as ratings in the edx dataset?
# # How many threes were given as ratings in the edx dataset?
# nrow(edx[edx$rating == 0])
# nrow(edx[edx$rating == 3])
# 
# # Q3
# # How many different movies are in the edx dataset?
# nrow(distinct(edx, movieId))
# 
# # Q4
# # How many different users are in the edx dataset?
# nrow(distinct(edx, userId))
# 
# # Q5
# # How many movie ratings are in each of the following genres in the edx dataset?
# # Drama, Comedy, Thriller, Romance
# nrow(edx[grepl("Drama", edx$genres)])
# nrow(edx[grepl("Comedy", edx$genres)])
# nrow(edx[grepl("Thriller", edx$genres)])
# nrow(edx[grepl("Romance", edx$genres)])
# 
# # Q6
# # Which movie has the greatest number of ratings?
# edx %>%
#   group_by(title) %>%
#   summarize(n = n()) %>%
#   arrange(desc(n))
# 
# # Q7
# # What are the five most given ratings in order from most to least?
# edx %>%
#   group_by(rating) %>%
#   summarize(n = n()) %>%
#   arrange(desc(n))

# Q8
# True or False: In general, half star ratings are less common than whole star ratings (e.g., there are 
#   fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
# Answer = TRUE

#################################################################################

# --------------------------------------------------------------------- #
#   Create train and test sets from edx dataset
# ----------------------------------------------------------------------#

# Split edx data into training and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set_temp <- edx[test_index,]

#Confirm userId and movieId are in both the train and test sets
test_set <- test_set_temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Add the rows removed from the test_set back into train_set and remove unneeded objects
removed <- anti_join(test_set_temp, test_set)
train_set <- rbind(train_set, removed)
rm(test_set_temp, test_index, removed)

# Create the function for calculating Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}


# --------------------------------------------------------------------- #
#   Data Transformation
# ----------------------------------------------------------------------#

# Select only the primary genre from the concatenated list of genres associated with each movie
# Simplify the timestamp column by extracting only the date and formatting it as a calendar date
# Extract the year the rating was given from the timestamp
# Extract the year of release from the movie title
edx <- edx %>%
  mutate(
    pri_genre = str_extract(genres, "^[:alpha:]+|"),
    date = date(as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone())),
    rating_year = year(as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone())),
    release_year = as.numeric(str_sub(edx$title, start = -5, end = -2))
  )

  # The origin, or anchor date, was selected based on page 645 of the textbook, which stated that 
  # the timestamp variable represents the time and data in which the rating was provided. 
  # The units are seconds since January 1, 1970.

# Calculate the age (in years) of the rating and of the movie
edx <- edx %>%
  mutate(
    rating_age_yr = year(Sys.Date()) - rating_year,
    movie_age_yr = year(Sys.Date()) - release_year
  )

# Complete the same mutations for the train, test, and validation datasets
# train_set
train_set <- train_set %>%
  mutate(
    pri_genre = str_extract(genres, "^[:alpha:]+|"),
    date = as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone()),
    rating_year = year(as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone())),
    release_year = as.numeric(str_sub(train_set$title, start = -5, end = -2))
  )

train_set <- train_set %>%
  mutate(
    rating_age_yr = year(Sys.Date()) - rating_year,
    movie_age_yr = year(Sys.Date()) - release_year
  )

# test_set
test_set <- test_set %>%
  mutate(
    pri_genre = str_extract(genres, "^[:alpha:]+|"),
    date = as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone()),
    rating_year = year(as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone())),
    release_year = as.numeric(str_sub(test_set$title, start = -5, end = -2))
  )

test_set <- test_set %>%
  mutate(
    rating_age_yr = year(Sys.Date()) - rating_year,
    movie_age_yr = year(Sys.Date()) - release_year
  )

# validation set
validation <- validation %>%
  mutate(
    pri_genre = str_extract(genres, "^[:alpha:]+|"),
    date = as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone()),
    rating_year = year(as.POSIXct(timestamp, origin = "1970-01-01", tz = Sys.timezone())),
    release_year = as.numeric(str_sub(validation$title, start = -5, end = -2))
  )

validation <- validation %>%
  mutate(
    rating_age_yr = year(Sys.Date()) - rating_year,
    movie_age_yr = year(Sys.Date()) - release_year
  )


# --------------------------------------------------------------------- #
#   Simple exploratory analysis of edx, train, and test datasets
# ----------------------------------------------------------------------#

# Investigate datasets
glimpse(edx)
glimpse(train_set)
glimpse(test_set)

# Determine the number of distinct values for users, movies, etc. in the edx dataset
edx %>% summarize(
  n_users = n_distinct(userId), 
  n_movies = n_distinct(movieId),
  n_genres = n_distinct(genres),
  n_pri_genre = n_distinct(pri_genre)
)

# Determine the number of distinct values of users and movies for the test and train datasets
train_set %>% summarize(
  n_users_train = n_distinct(userId), 
  n_movies_train = n_distinct(movieId),
  n_genres_train = n_distinct(genres),
  n_pri_genre_train = n_distinct(pri_genre)
)
test_set %>% summarize(
  n_users_test = n_distinct(userId), 
  n_movies_test = n_distinct(movieId),
  n_genres_test = n_distinct(genres),
  n_pri_genre_test = n_distinct(pri_genre)
)

# Develop some summary statistics for the datasets
edx %>% summarize(
  mean_rating = mean(rating),
  sd = sd(rating), 
  median_rating = median(rating),
  min_movie_age = min(movie_age_yr),
  max_movie_age = max(movie_age_yr),
  avg_movie_age = mean(movie_age_yr),
  min_rating_age = min(rating_age_yr),
  max_rating_age = max(rating_age_yr),
  avg_rating_age = mean(rating_age_yr)
)

train_set %>% summarize(
  mean_rating = mean(rating),
  sd = sd(rating), 
  median_rating = median(rating),
  min_movie_age = min(movie_age_yr),
  max_movie_age = max(movie_age_yr),
  avg_movie_age = mean(movie_age_yr),
  min_rating_age = min(rating_age_yr),
  max_rating_age = max(rating_age_yr),
  avg_rating_age = mean(rating_age_yr)
)

test_set %>% summarize(
  mean_rating = mean(rating),
  sd = sd(rating), 
  median_rating = median(rating),
  min_movie_age = min(movie_age_yr),
  max_movie_age = max(movie_age_yr),
  avg_movie_age = mean(movie_age_yr),
  min_rating_age = min(rating_age_yr),
  max_rating_age = max(rating_age_yr),
  avg_rating_age = mean(rating_age_yr)
)


# --------------------------------------------------------------------- #
#   Algorithm development using edx dataset
# ----------------------------------------------------------------------#

##########################
# Overall Average
##########################
# baseline rating ("mu") = mean over all user-movie ratings
br <- mean(train_set$rating)
br
sum(is.na(train_set$rating))


##########################
# Overall Median
##########################
# baseline rating ("mu") = mean over all user-movie ratings
median_rating <- median(train_set$rating)
median_rating


##########################
# Naive Model
##########################
# Rating difference between overall average of the train_set and true ratings of test_set
naive_rmse <- RMSE(test_set$rating, br)
rmse_results <- tibble(Method = "Naive Model", RMSE = naive_rmse)


##########################
# Movie Effects
##########################
# movie-specific effect = rating difference between specific movie and average movie
movie_effects <- train_set %>%
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - br))

# 1) predictions using movie-specific effects
predicted_ratings_1 <- test_set %>% 
  left_join(movie_effects, by = 'movieId', na_matches = "never") %>%
  mutate(predictions = br + b_m) %>%
  pull(predictions)

# check for missing values
sum(is.na(movie_effects))
sum(is.na(predicted_ratings_1))
predicted_ratings_1[is.na(predicted_ratings_1)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_c1 <- ifelse(is.na(predicted_ratings_1), br, predicted_ratings_1)

# results
movie_rmse <- RMSE(test_set$rating, predicted_ratings_c1)
rmse_results <- rmse_results %>% add_row(Method = "Movie Only", RMSE = movie_rmse)


##########################
# User Effects
##########################
# calculate the user-specific effect
user_effects <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - br))

# 2) predictions using user-specific effects
predicted_ratings_2 <- test_set %>%
  left_join(user_effects, by = 'userId', na_matches = "never") %>% 
  mutate(predictions = br + b_u) %>%
  pull(predictions)

# check for missing values
sum(is.na(user_effects))
sum(is.na(predicted_ratings_2))
predicted_ratings_2[is.na(predicted_ratings_2)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_2c <- ifelse(is.na(predicted_ratings_2), br, predicted_ratings_2)

# results
user_rmse <- RMSE(test_set$rating, predicted_ratings_2c)
rmse_results <- rmse_results %>% add_row(Method = "User Only", RMSE = user_rmse)


##########################
# All Distinct Genres
##########################

# I investigated other ways of using genre to inform or improve the model.
#   Ultimately, though, I decided not to proceed but wanted to preserve the code.
#   Commented out due to resource drain caused by separate_rows() function.

# # Separate combined genres column
# edx_genres <- edx %>% separate_rows(genres, sep = "\\|")
# 
# # Summarize number of genres and average rating from complete edx dataset
# edx_genres_sum <- edx_genres %>%
#   group_by(genres) %>% 
#   summarize(ratings_count = n(), avg_rating = mean(rating))
# edx_genres_sum
# 
# # Count the number of genres associated with each movie
# edx_genre_count <- edx_genres %>% 
#   select(movieId, genres) %>%
#   distinct() %>%
#   group_by(movieId) %>%
#   summarize(num_genres = n())
# edx_genre_count
# 
# # Determine the average rating based on the number of associated genres 
# movie_avg <- edx %>%
#   group_by(movieId) %>% 
#   summarize(avg_rating = mean(rating)) %>%
#   left_join(edx_genre_count, by = 'movieId')
# 
# # Plot average rating versus number of associated genres
# movie_avg %>%
#   ggplot(aes(group = num_genres, x = num_genres, y = avg_rating)) + geom_boxplot()


##########################
# Primary Genre Effects
##########################
# Here, I wanted to see whether the first genre listed in the genres column for each movie
#   played any more of a role in predicting the rating than the concatenated list of genres

# Calculate the genre-specific effect = difference between specific genre and overall average
#   when grouped by the "primary" genre, which is the first genre listed for each movie
genre_effects <- train_set %>%
  group_by(pri_genre) %>%
  summarize(b_g = mean(rating - br))
genre_effects

# 3) predictions using primary genre-specific effects
predicted_ratings_3 <- test_set %>% 
  left_join(genre_effects, by = 'pri_genre', na_matches = "never") %>%
  mutate(predictions = br + b_g) %>%
  pull(predictions
    )

# check for NAs
sum(is.na(genre_effects))
sum(is.na(predicted_ratings_3))
predicted_ratings_3[is.na(predicted_ratings_3)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_3c <- ifelse(is.na(predicted_ratings_3), br, predicted_ratings_3)

# results
genre_rmse <- RMSE(test_set$rating, predicted_ratings_3c)
rmse_results <- rmse_results %>% add_row(Method = "Primary Genre Only", RMSE = genre_rmse)


##########################
# Genre Combo Effects
##########################
# genre combo effect = rating difference between specific combination of genres and average rating
genre_combo <- train_set %>% 
  group_by(genres) %>%
  summarize(b_gc = mean(rating - br))
genre_combo

# 3a) predictions using genre combo effects
predicted_ratings_3a <- test_set %>%
  left_join(genre_combo, by = 'genres', na_matches = "never") %>%
  mutate(predictions = br + b_gc) %>%
  pull(predictions)

# check for NAs
sum(is.na(genre_combo))
sum(is.na(predicted_ratings_3a))
predicted_ratings_3a[is.na(predicted_ratings_3a)==1]
# Chose to replace them with the overall mean of 3.513
predicted_ratings_3ac <- ifelse(is.na(predicted_ratings_3a), br, predicted_ratings_3a)

# results
genre_combo_rmse <- RMSE(test_set$rating, predicted_ratings_3ac)
rmse_results <- rmse_results %>% add_row(Method = "Genre Combo Only", RMSE = genre_combo_rmse)


##########################
# Movie Age Effects
##########################
# Calculate the specific effect due to the age of the movie (since its release)
movie_age_effects <- train_set %>% 
  group_by(movie_age_yr) %>% 
  summarize(b_ma = mean(rating - br))

# 4) predictions using movie age effects
predicted_ratings_4 <- br + test_set %>%
  left_join(movie_age_effects, by = 'movie_age_yr', na_matches = "never") %>% 
  pull(b_ma)

# check for missing values
sum(is.na(movie_age_effects))
sum(is.na(predicted_ratings_4))
predicted_ratings_4[is.na(predicted_ratings_4)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_4c <- ifelse(is.na(predicted_ratings_4), br, predicted_ratings_4)

# results
movie_age_rmse <- RMSE(test_set$rating, predicted_ratings_4c)
rmse_results <- rmse_results %>% add_row(Method = "Movie Age Only", RMSE = movie_age_rmse)


##########################
# Rating Age Effects
##########################
# Calculate the specific effect due to the age of the rating
rating_age_effects <- train_set %>%
  group_by(rating_age_yr) %>%
  summarize(b_ra = mean(rating - br))

# 5) predictions using rating age effects
predicted_ratings_5 <- br + test_set %>%
  left_join(rating_age_effects, by = 'rating_age_yr', na_matches = "never") %>%
  pull(b_ra)

# check for missing values
sum(is.na(rating_age_effects))
sum(is.na(predicted_ratings_5))
predicted_ratings_5[is.na(predicted_ratings_5)==1]
# Chose to replace them with the overall mean of 3.513
predicted_ratings_5c <- ifelse(is.na(predicted_ratings_5), br, predicted_ratings_5)

# results
rating_age_rmse <- RMSE(test_set$rating, predicted_ratings_5c)
rmse_results <- rmse_results %>% add_row(Method = "Rating Age Only", RMSE = rating_age_rmse)


##########################
# Combined Effects
##########################
# 6) predictions using movie and user

# Create new user effects data with movie effects included
user_effects_n <- train_set %>% 
  left_join(movie_effects, by = "movieId", na_matches = "never") %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - br - b_m))

# predictions
predicted_ratings_6 <- test_set %>% 
  left_join(movie_effects, by='movieId', na_matches = "never") %>% 
  left_join(user_effects_n, by='userId', na_matches = "never") %>% 
  mutate(predictions = br + b_m + b_u) %>% 
  pull(predictions)

# check for NAs
sum(is.na(predicted_ratings_6))
predicted_ratings_6[is.na(predicted_ratings_6)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_6c <- ifelse(is.na(predicted_ratings_6), br, predicted_ratings_6)

# results
m_u_rmse <- RMSE(test_set$rating, predicted_ratings_6c)
rmse_results <- rmse_results %>% add_row(Method = "Movie and User", RMSE = m_u_rmse)


# 7) predictions using movie and primary genre

# Create new primary genre effects data with movie effects included
genre_effects_n <- train_set %>% 
  left_join(movie_effects, by = 'movieId', na_matches = "never") %>%
  group_by(pri_genre) %>% 
  summarize(b_g = mean(rating - br - b_m))

# predictions
predicted_ratings_7 <- test_set %>% 
  left_join(movie_effects, by='movieId', na_matches = "never") %>% 
  left_join(genre_effects_n, by='pri_genre', na_matches = "never") %>% 
  mutate(predictions = br + b_m + b_g) %>% 
  pull(predictions)

# check for NAs
sum(is.na(predicted_ratings_7))
predicted_ratings_7[is.na(predicted_ratings_7)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_7c <- ifelse(is.na(predicted_ratings_7), br, predicted_ratings_7)

# results
m_g_rmse <- RMSE(test_set$rating, predicted_ratings_7c)
rmse_results <- rmse_results %>% add_row(Method = "Movie and Primary Genre", RMSE = m_g_rmse)


# 8) predictions using movie, user, and primary genre
genre_effects_n2 <- train_set %>% 
  left_join(movie_effects, by = 'movieId', na_matches = "never") %>%
  left_join(user_effects_n, by = "userId", na_matches = "never") %>%
  group_by(pri_genre) %>% 
  summarize(b_g = mean(rating - br - b_m))

predicted_ratings_8 <- test_set %>% 
  left_join(movie_effects, by='movieId', na_matches = "never") %>% 
  left_join(user_effects_n, by='userId', na_matches = "never") %>% 
  left_join(genre_effects_n2, by = 'pri_genre', na_matches = "never") %>%
  mutate(predictions = br + b_m + b_u + b_g) %>% 
  pull(predictions)

# check for NAs
sum(is.na(predicted_ratings_8))
predicted_ratings_8[is.na(predicted_ratings_8)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_8c <- ifelse(is.na(predicted_ratings_8), br, predicted_ratings_8)

# results
m_u_pg_rmse <- RMSE(test_set$rating, predicted_ratings_8c)
rmse_results <- rmse_results %>% add_row(Method = "Movie, User, Primary Genre", RMSE = m_u_pg_rmse)


# 9) predictions using movie, user, and genre combo

# Create new genre combo effects data with movie and user effects included
genre_combo_n <- train_set %>% 
  left_join(movie_effects, by = 'movieId', na_matches = "never") %>%
  left_join(user_effects_n, by = 'userId', na_matches = "never") %>%
  group_by(genres) %>% 
  summarize(b_gc = mean(rating - br - b_m - b_u))

# predictions
predicted_ratings_9 <- test_set %>% 
  left_join(movie_effects, by='movieId', na_matches = "never") %>% 
  left_join(user_effects_n, by='userId', na_matches = "never") %>% 
  left_join(genre_combo_n, by = 'genres', na_matches = "never") %>%
  mutate(predictions = br + b_m + b_u + b_gc) %>% 
  pull(predictions)

# check for NAs
sum(is.na(predicted_ratings_9))
predicted_ratings_9[is.na(predicted_ratings_9)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_9c <- ifelse(is.na(predicted_ratings_9), br, predicted_ratings_9)

# results
m_u_gc_rmse <- RMSE(test_set$rating, predicted_ratings_9c)
rmse_results <- rmse_results %>% add_row(Method = "Movie, User, Genre Combo", RMSE = m_u_gc_rmse)


# 10) predictions using movie, user and movie age

# Create new movie age effects data with movie and user effects included
movie_age_effects_n <- train_set %>% 
  left_join(movie_effects, by = 'movieId', na_matches = "never") %>%
  left_join(user_effects_n, by = 'userId', na_matches = "never") %>%
  group_by(movie_age_yr) %>% 
  summarize(b_ma = mean(rating - br - b_m - b_u))

# predictions
predicted_ratings_10 <- test_set %>% 
  left_join(movie_effects, by='movieId', na_matches = "never") %>% 
  left_join(user_effects_n, by='userId', na_matches = "never") %>% 
  left_join(movie_age_effects_n, by = 'movie_age_yr') %>%
  mutate(predictions = br + b_m + b_u + b_ma) %>% 
  pull(predictions)

# check for NAs
sum(is.na(predicted_ratings_10))
predicted_ratings_10[is.na(predicted_ratings_10)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_10c <- ifelse(is.na(predicted_ratings_10), br, predicted_ratings_10)

# results
mum_rmse <- RMSE(test_set$rating, predicted_ratings_10c)
rmse_results <- rmse_results %>% add_row(Method = "Movie, User, Movie Age", RMSE = mum_rmse)


###############################################
# Regularization - Penalized Least Squares
###############################################

# Rather than attempt regularization on all model variations, I selected the best performing models
#   thus far (Movie Only, Movie-User, and Movie-User-Genre Combo) and repeated them with regular-
#   ization and lambda selection using cross-validation. 

##### Movie Only Effect #####

# Choose lambda using cross-validation
lambdas_m <- seq(0, 10, 0.25)

rmses <- sapply(lambdas_m, function(l){

    movie_reg_cv <- train_set %>%
      group_by(movieId) %>%
      summarize(b_m = sum(rating - br) / (n() + l))
  
    predicted_ratings <- test_set %>%
      left_join(movie_reg_cv, by = 'movieId', na_matches = "never") %>% 
      mutate(predictions = br + b_m) %>%
      pull(predictions)
  
    return(RMSE(test_set$rating, predicted_ratings)) 
})
qplot(lambdas_m, rmses)

lambda_m <- lambdas_m[which.min(rmses)]

movie_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - br) / (n() + lambda_m))

# 11) predictions using regularized movie-specific 
predicted_ratings_11 <- test_set %>% 
  left_join(movie_reg, by = "movieId", na_matches = "never") %>% 
  mutate(predictions = br + b_m) %>%
  pull(predictions)

# Check for NAs
sum(is.na(predicted_ratings_11))
predicted_ratings_11[is.na(predicted_ratings_11)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_11c <- ifelse(is.na(predicted_ratings_11), br, predicted_ratings_11)

# results for regularized effects model
m_reg_rmse <- RMSE(test_set$rating, predicted_ratings_11c)
rmse_results <- rmse_results %>% add_row(Method = "Movie Regularized", RMSE = m_reg_rmse)


##### Movie-User Effect #####

# Choose lambda using cross-validation
lambdas_m_u <- seq(0, 10, 0.25)

rmses <- sapply(lambdas_m_u, function(l){
  
  b_m <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - br) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - br) / (n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    left_join(b_u, by = 'userId', na_matches = "never") %>% 
    mutate(predictions = br + b_m + b_u) %>%
    pull(predictions)
  return(RMSE(test_set$rating, predicted_ratings)) 
})

qplot(lambdas_m_u, rmses)

lambda_m_u <- lambdas_m_u[which.min(rmses)]

movie_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - br) / (n() + lambda_m_u))

user_reg <- train_set %>%
  left_join(movie_reg, by= "movieId", na_matches = "never") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - br) / (n() + lambda_m_u))

# 12) predictions using regularized movie and user-specific 
predicted_ratings_12 <- test_set %>% 
  left_join(movie_reg, by= "movieId", na_matches = "never") %>%
  left_join(user_reg, by = "userId", na_matches = "never") %>% 
  mutate(predictions = br + b_m + b_u) %>%
  pull(predictions)

# Check for NAs
sum(is.na(predicted_ratings_12))
predicted_ratings_12[is.na(predicted_ratings_12)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_12c <- ifelse(is.na(predicted_ratings_12), br, predicted_ratings_12)

# results for regularized effects model
m_u_reg_rmse <- RMSE(test_set$rating, predicted_ratings_12c)
rmse_results <- rmse_results %>% add_row(Method = "Movie-User Regularized", RMSE = m_u_reg_rmse)


##### Movie-User-Genre Combo Effect #####

# Choose lambda using cross-validation
lambdas_g <- seq(0, 10, 0.25)

rmses <- sapply(lambdas_g, function(l){
  
  b_m <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - br) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - br - b_m) / (n() + l))
  
  b_g <- train_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    left_join(b_u, by = 'userId', na_matches = "never") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - br - b_m - b_u) / (n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    left_join(b_u, by = 'userId', na_matches = "never") %>% 
    left_join(b_g, by = 'genres', na_matches = "never") %>%
    mutate(predictions = br + b_m + b_u + b_g) %>%
    pull(predictions)
  
  return(RMSE(test_set$rating, predicted_ratings)) 
})

qplot(lambdas_g, rmses)

lambda_g <- lambdas_g[which.min(rmses)]

movie_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - br) / (n() + lambda_g))

user_reg <- train_set %>%
  left_join(movie_reg, by= "movieId", na_matches = "never") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - br) / (n() + lambda_g))

genre_combo_reg <- train_set %>%
  left_join(movie_reg, by= "movieId", na_matches = "never") %>%
  left_join(user_reg, by = 'userId', na_matches = "never") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - br - b_m - b_u) / (n() + lambda_g))

# 13) predictions using combined movie, user, and genre combination effects with regularization
predicted_ratings_13 <- test_set %>% 
  left_join(movie_reg, by = "movieId", na_matches = "never") %>% 
  left_join(user_reg, by ='userId', na_matches = "never") %>% 
  left_join(genre_combo_reg, by = 'genres', na_matches = "never") %>%
  mutate(predictions = br + b_m + b_u + b_g) %>%
  pull(predictions)

# Check for NAs
sum(is.na(predicted_ratings_13))
predicted_ratings_13[is.na(predicted_ratings_13)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_13c <- ifelse(is.na(predicted_ratings_13), br, predicted_ratings_13)

# results for regularized effects model
m_u_g_reg_rmse <- RMSE(test_set$rating, predicted_ratings_13c)
rmse_results <- rmse_results %>% 
  add_row(Method = "Movie, User, Genre Combo Regularized", RMSE = m_u_g_reg_rmse)


###############################################
# Test_Set Results
###############################################
# Here are the aggregated results of all models above based on the test_set data

options(pillar.sigfig = 5, pillar.bold = TRUE)
rmse_results

rmse_results[which.min(rmse_results$RMSE),]

# Code to clear rmse results table:
# rmse_results <- rmse_results %>% slice(-c(1:nrow(rmse_results)))


# --------------------------------------------------------------------- #
#   Predictions using validation dataset
# ----------------------------------------------------------------------#
# Similar to regularization with the test_set data, I did not want to repeat all models using the
#   validation dataset. Instead, the best models (Movie Only, Movie-User-Genre Combo, and Movie-
#   User-Genre Combo with regularization) were repeated using the validation data.


##### Movie Only #####

# Movie-specific effects 
movie_effects_val <- train_set %>%
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - br))

# 1) predictions using movie-specific effects
predicted_ratings_val1 <- validation %>% 
  left_join(movie_effects_val, by = 'movieId', na_matches = "never") %>%
  mutate(predictions = br + b_m) %>%
  pull(predictions)

# check for missing values
sum(is.na(movie_effects_val))
sum(is.na(predicted_ratings_val1))
predicted_ratings_val1[is.na(predicted_ratings_val1)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_val1c <- ifelse(is.na(predicted_ratings_val1), br, predicted_ratings_val1)

# results
movie_rmse_val <- RMSE(validation$rating, predicted_ratings_val1c)
rmse_results <- rmse_results %>% add_row(Method = "Movie Only", RMSE = movie_rmse_val)


##### Movie-User-Genre Combo #####

# Create new genre combo effects data with movie and user effects included
genre_combo_nv <- train_set %>% 
  left_join(movie_effects, by = 'movieId', na_matches = "never") %>%
  left_join(user_effects_n, by = 'userId', na_matches = "never") %>%
  group_by(genres) %>% 
  summarize(b_gc = mean(rating - br - b_m - b_u))

# 2) predictions using movie, user, and genre combo effects
predicted_ratings_val2 <- validation %>% 
  left_join(movie_effects, by='movieId', na_matches = "never") %>% 
  left_join(user_effects_n, by='userId', na_matches = "never") %>% 
  left_join(genre_combo_nv, by = 'genres', na_matches = "never") %>%
  mutate(predictions = br + b_m + b_u + b_gc) %>% 
  pull(predictions)

# check for NAs
sum(is.na(predicted_ratings_val2))
predicted_ratings_val2[is.na(predicted_ratings_val2)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_val2c <- ifelse(is.na(predicted_ratings_val2), br, predicted_ratings_val2)

# results
m_u_gc_rmse_val <- RMSE(validation$rating, predicted_ratings_val2c)
rmse_results <- rmse_results %>% add_row(Method = "Movie, User, Genre Combo", 
                                         RMSE = m_u_gc_rmse_val)


##### Movie-User-Genre Combo with Regularization #####

# Choose lambda using cross-validation
lambdas_g <- seq(0, 10, 0.25)

rmses <- sapply(lambdas_g, function(l){
  
  b_m <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - br) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - br - b_m) / (n() + l))
  
  b_g <- train_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    left_join(b_u, by = 'userId', na_matches = "never") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - br - b_m - b_u) / (n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_m, by = 'movieId', na_matches = "never") %>%
    left_join(b_u, by = 'userId', na_matches = "never") %>% 
    left_join(b_g, by = 'genres', na_matches = "never") %>%
    mutate(predictions = br + b_m + b_u + b_g) %>%
    pull(predictions)
  
  return(RMSE(test_set$rating, predicted_ratings)) 
})

qplot(lambdas_g, rmses)

lambda_g <- lambdas_g[which.min(rmses)]

movie_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - br) / (n() + lambda_g))

user_reg <- train_set %>%
  left_join(movie_reg, by= "movieId", na_matches = "never") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - br) / (n() + lambda_g))

genre_combo_reg <- train_set %>%
  left_join(movie_reg, by= "movieId", na_matches = "never") %>%
  left_join(user_reg, by = 'userId', na_matches = "never") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - br - b_m - b_u) / (n() + lambda_g))

# 3) predictions using combined movie, user, and genre combination effects with regularization
predicted_ratings_val3 <- validation %>% 
  left_join(movie_reg, by = "movieId", na_matches = "never") %>% 
  left_join(user_reg, by ='userId', na_matches = "never") %>% 
  left_join(genre_combo_reg, by = 'genres', na_matches = "never") %>%
  mutate(predictions = br + b_m + b_u + b_g) %>%
  pull(predictions)

# Check for NAs
sum(is.na(predicted_ratings_val3))
predicted_ratings_val3[is.na(predicted_ratings_val3)==1]
# Chose to replace them with the overall mean of 3.513 
predicted_ratings_val3c <- ifelse(is.na(predicted_ratings_val3), br, predicted_ratings_val3)

# results for regularized effects model
m_u_g_reg_rmse_val <- RMSE(validation$rating, predicted_ratings_val3c)
rmse_results <- rmse_results %>% 
  add_row(Method = "Movie, User, Genre Combo Regularized", 
          RMSE = m_u_g_reg_rmse_val)



###############################################
# Validation Set Results
###############################################
# Here are the aggregated results of all models above based on the validation data

options(pillar.sigfig = 5, pillar.bold = TRUE)
rmse_results

rmse_results[which.min(rmse_results$RMSE),]

# rmse_results <- rmse_results %>% slice(-c(1:nrow(rmse_results)))


###############################################
# Combined Results Table
###############################################
# Finally, these are the combined test_set and validation set model results arranged side-by-side
#   for easier comparison.

options(pillar.sigfig = 5, pillar.bold = TRUE)
final_results_table <- 
  tibble(
    Model = c("Naive", "Movie Only", "User Only", "Primary Genre Only", "Genre Combo Only",
              "Movie Age Only", "Rating Age Only", "Movie and User", "Movie and Primary Genre",
              "Movie, User, Primary Genre", "Movie, User, Genre Combo", "Movie, User, Movie Age",
              "Movie Regularized", "Movie-User Regularized", 
              "Movie, User, Genre Combo Regularized"),
    RMSE_Test = c(naive_rmse, movie_rmse, user_rmse, genre_rmse, genre_combo_rmse,
                  movie_age_rmse, rating_age_rmse, m_u_rmse, m_g_rmse,
                  m_u_pg_rmse, m_u_gc_rmse, mum_rmse,
                  m_reg_rmse, m_u_reg_rmse, 
                  m_u_g_reg_rmse),
    RMSE_Validation = c(NA, movie_rmse_val, NA, NA, NA,
                        NA, NA, NA, NA,
                        NA, m_u_gc_rmse_val, NA,
                        NA, NA,
                        m_u_g_reg_rmse_val)
  )

final_results_table

# final_results_table <- final_results_table %>% slice(-c(1:nrow(rmse_results)))


###############################################
#   Project Plots 
###############################################
# The code below consists of all the visuals created over the course of the project to help inform
#   model development, performance, and understanding of the data involved.


#Training set distributions

# primary genre
hist_pri_genre <- train_set %>%
  group_by(pri_genre) %>%
  summarize(num_ratings = n()) %>%
  ggplot(aes(x = reorder(pri_genre, (-num_ratings)), y = num_ratings)) +
  geom_col(color = "black", fill = "gray75") +
  labs(x = "Primary Genre", y = "Number of Ratings") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
hist_pri_genre

# rating year
hist_rating_year <- train_set %>%
  ggplot(aes(x = rating_year)) +
  geom_histogram(bins = n_distinct(train_set$rating_year), color = "black", fill = "gray75")
hist_rating_year

# release year
hist_release_year <- train_set %>%
  ggplot(aes(x = release_year)) +
  geom_histogram(bins = n_distinct(train_set$release_year), color = "black", fill = "gray75")
hist_release_year

# Plot distribution for ratings to inform NA replacement in predicted_ratings
p_overall <- train_set %>%
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 10, color = "black", fill = "gray75") +
  geom_vline(xintercept = br, color = "#20A387FF", size = 1, linetype = "dashed") + 
  geom_vline(xintercept = median_rating, color = "#95D840FF", size = 1, linetype = "dotted")

p_o_annotations <- data.frame(
  x = c(round(mean(train_set$rating), 3), round(median(train_set$rating), 3)),
  y = c(1850000, 2000000),
  label = c( "mean: ", "median: ")
)

p_overall <- p_overall +
  geom_text(
    data = p_o_annotations,
    aes(x = x, y = y, label = paste(label, x)),
    color = c("#20A387FF", "#95D840FF"),
    nudge_x = c(-0.6, 0.6)
  ) + 
  labs(
    title = "Distribution of overall ratings",
    x = "Rating Value",
    y = "Count"
  )

p_overall

# Plot distribution of overall average rating for each movie
train_set %>%
  group_by(movieId) %>% 
  summarize(avg_m_rating = mean(rating)) %>%
  ggplot(aes(avg_m_rating)) +
  geom_histogram(bins = 10, color = "black", fill = "gray75")

# Plot distribution of overall average rating by user
train_set %>% 
  group_by(userId) %>% 
  summarize(avg_u_rating = mean(rating)) %>% 
  filter(n()>=10) %>%
  ggplot(aes(avg_u_rating)) +
  geom_histogram(bins = 30, color = "black", fill = "gray75")


###############################################
#   Save Key Environment Objects 
###############################################
# Save specific objects from this environment for use in the R Markdown file

save(p_overall, final_results_table, file = "capstone_movielens_enviro.rda")