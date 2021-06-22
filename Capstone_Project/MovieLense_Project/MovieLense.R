#Create train and validation sets
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

#Installing required packages:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#Installing required libraries:
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Data Loading
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later: using 4.1.0
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Data Pre-processing
# Validation set will be 10% of MovieLens data
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Extracting year as a column from title in the edx & validation dataset
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))


#Data Analysis and Exploration
#General overview of dataset

dim(edx)

head(edx)

summary(edx)

#Summarize unique users and movies
edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId))

#Top 10 Movies ranked in order of the number of Ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# Ratings' Distribution
v_ratings <- as.vector(edx$rating)
unique(v_ratings)

edx %>% ggplot(aes(v_ratings)) +
  geom_histogram(bins = 10, color = "black") +
  scale_x_continuous(breaks = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0,3000000,500000))) +
  ggtitle("Ratings' Distribution")


# Ratings' Distribution by Movie:
edx %>% count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ylab("Number of movies") +
  ggtitle("Ratings' Distribution by Movie")


# Top 20 Movies which rated only once by User:
edx %>% group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()


# Ratings' Distribution by User:
edx %>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ylab("Number of users") +
  ggtitle("Ratings' Distribution by User")


### Modelling Approach

# Function to calculate RMSE value:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2, na.rm = T))
}


## Average Rating Model

# Using Mean only
mu <- mean(edx$rating)

model_1_rmse <- RMSE(validation$rating, mu)

# Save RMSE result in data frame
rmse_result <- data.frame(Model = "Average Rating Model", RMSE = model_1_rmse)

rmse_result %>% knitr::kable()


## Movie Effect Model
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., 
                     color = I("black"), ylab = "Number of movies", 
                     main = "Number of movies with the computed b_i")

predicted_ratings_2 <- mu + validation %>%
  left_join(movie_avgs, by='movieId') %>% 
  pull(b_i)

model_2_rmse <- RMSE(validation$rating, predicted_ratings_2)

# Save RMSE result in data frame
rmse_result <- bind_rows(rmse_result, 
                         tibble(Model="Movie Effect Model", 
                                RMSE = model_2_rmse))

rmse_result %>% knitr::kable()


## User Effect Model
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

user_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., 
                    color = I("black"), ylab = "Number of Users", 
                    main = "Number of users with the computed b_u")

predicted_ratings_3 <- validation %>%
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)

model_3_rmse <- RMSE(validation$rating, predicted_ratings_3)

# Save RMSE result in data frame
rmse_result <- bind_rows(rmse_result, 
                         tibble(Model = "Movie and User Effects Model", 
                                RMSE = model_3_rmse))

rmse_result %>% knitr::kable()


## Regularized Movie and User Effect Model
lambdas = seq(0, 10, 0.25)

# Note: Below function could take a couple of minutes
model_4_rmses <- sapply(lambdas, function(l){    
  
  mu <- mean(edx$rating) 
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings_4 <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(validation$rating, predicted_ratings_4))
})

# Plot rmse vs lambda to select the optimal lambda
qplot(lambdas, model_4_rmses)

lambda <- lambdas[which.min(model_4_rmses)]
lambda

# Save RMSE result in data frame
rmse_result <- bind_rows(rmse_result, 
                         tibble(Model = "Regularized Movie and User Effect Model", 
                                RMSE = min(model_4_rmses)))

rmse_result %>% knitr::kable()


## Regularized Movie, User and Year Effect Model
lambdas = seq(0, 10, 0.25)

# Note: Below function could take a couple of minutes
model_5_rmses <- sapply(lambdas, function(l){    
  
  mu <- mean(edx$rating) 
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l))
  
  predicted_ratings_5 <- validation %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'year') %>%
    mutate(pred = mu + b_i + b_u + b_y) %>% 
    pull(pred)
  
  return(RMSE(validation$rating, predicted_ratings_5))
})

# Plot rmse vs lambda to select the optimal lambda
qplot(lambdas, model_5_rmses)

lambda <- lambdas[which.min(model_5_rmses)]
lambda

# Save RMSE result in data frame
rmse_result <- bind_rows(rmse_result, 
                         tibble(Model = "Regularized Movie, User and Year Effect Model", 
                                RMSE = min(model_5_rmses)))

rmse_result %>% knitr::kable()


# RMSE results of all Models
rmse_result %>% knitr::kable()
