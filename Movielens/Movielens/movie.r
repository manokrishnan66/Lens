
################################
# Data Loading
################################

# Note: this process could take a couple of minutes
tinytex::install_tinytex()
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(ggplot2)
library(lubridate)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

library(readr)
movies <- read_delim("ml-10M100K/movies.dat", 
                     "::", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
View(movies)
ratings <- read_delim("ml-10M100K/ratings.dat", 
                     "::", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)


movies <- movies %>% select(-X2,-X4)
colnames(movies) <- c("movieId", "title", "genres")

ratings <- ratings %>% select(-X2,-X4,-X6)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

levels(movies$movieId)
str(movies)
str(ratings)

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId)[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

ratings <- as.data.frame(ratings) %>% mutate(userId = as.numeric(userId),
                                              movieId = as.numeric(movieId),
                                              rating = as.numeric(rating),
                                             timestamp = as.numeric(timestamp))

str(movielens)

movielens <- left_join(ratings, movies, by = "movieId")

################################
# Split Raw Data: Train and validation Sets
################################

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings
validation_CM <- validation  # save the rating information
validation <- validation %>% select(-rating)#=======


   # Checking sample data using head
  head(edx)
  
  # Summarizing the dataset
  summary(edx)

  # Total unique movies and Users 
  edx %>%
    summarize(n_users = n_distinct(userId), 
              n_movies = n_distinct(movieId))
  

  ################################
  # Modifying the Year
  ################################
  
  # Modify the year as a column in the edx & validation datasets
  
  edx <- edx%>%separate(title,c("name", "year"), "\\s*\\((?=\\d+\\)$)|\\)$")
  validation <- validation%>%separate(title,c("name", "year"), "\\s*\\((?=\\d+\\)$)|\\)$")

  ################################
  # Data Visualization and Data Exploration
  ################################
  
  #Distribution of Ratings
  
  edx %>% 
    ggplot(aes(rating)) + geom_histogram(binwidth=0.25, fill = "red") +
    scale_x_discrete(limits = c(seq(0,5,1))) +
    scale_y_continuous(breaks = c(seq(0, 3000000, 250000))) +
    ggtitle("Distribution of Rating")
  
  # Ratings per movie
  
  edx %>%
    count(movieId) %>%
    ggplot(aes(n)) +
    geom_histogram(bins = 30, color = "maroon", fill = "green") +
    scale_x_log10() +
    geom_hline(yintercept=125, color = "brown",size=1) +
    xlab("No. of ratings") +
    ylab("No.of movies") +
    ggtitle("No. ratings per movie")
  
  # Number of ratings by Number of Users
  
  edx %>%
    count(userId) %>%
    ggplot(aes(n)) +
    geom_histogram(bins = 30, color = "white", fill = "maroon") +
    scale_x_log10() +
    xlab("Number of ratings") + 
    ylab("Number of users") +
    ggtitle("Number of ratings given by users")
  

  # Total movie ratings per genre
  
  edx%>%
    group_by(genres) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  
  # Mean movie ratings by users
  
  edx %>%
    group_by(userId) %>%
    filter(n() >= 100) %>%
    summarize(b = mean(rating)) %>%
    ggplot(aes(b)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    xlab("Mean-rating") +
    ylab("No of users") +
    ggtitle("Mean movie ratings by users") +
    scale_x_discrete(limits = c(seq(0,5,1))) +
    theme_light()
  
  ################################
  # Data Analysis and modelling
  ################################
  
  
  # Initiate RMSE results to compare various models
  rmse_results <- data_frame()
  
  # Compute the dataset's mean rating
  mu <- mean(edx$rating)  
  
  mu
  
  # simple model taking into account the movie effect b_i
  #subtract the rating minus the mean for each rating the movie received
  
  movie_av <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))
  
  movie_av %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("red"))
  
  # simple model taking into account the user effect b_u
  
  user_av <- edx %>% 
    left_join(movie_av, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
  
  user_av %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("green"))
  
  #### Model Validation #####
  ## Naive Model: just the mean ##
  naive_rmse <- RMSE(validation_CM$rating,mu)
  ## Test results based on simple prediction
  naive_rmse
  ## Check results
  rmse_results <- data_frame(method = "Using mean only", RMSE = naive_rmse)
  rmse_results
  ## Save prediction in data frame
  
  ## Movie Effects Model##
  
  predrat_movie_norm <- validation %>% 
    left_join(movie_av, by='movieId') %>%
    mutate(pred = mu + b_i) 
  model_1_rmse <- RMSE(validation_CM$rating,predrat_movie_norm$pred)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie Effect Model",  
                                       RMSE = model_1_rmse ))
  # save rmse results in a table
  rmse_results %>% knitr::kable()
  
  ## Movie and User Effects Model ##
  # Use test set,join movie averages & user averages
  # Prediction equals the mean with user effect b_u & movie effect b_i
  predrat_user_norm <- validation %>% 
    left_join(movie_av, by='movieId') %>%
    left_join(user_av, by='userId') %>%
    mutate(pred = mu + b_i + b_u) 
  
  # test and save rmse results 
  
  model_2_rmse <- RMSE(validation_CM$rating,predrat_user_norm$pred)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Movie and User Effect Model",  
                                       RMSE = model_2_rmse ))
  rmse_results %>% knitr::kable()
  
  ## Regularized movie and user effect model ##
  
  # lambda is a tuning parameter
  # Use cross-validation to choose it.
  lambdas <- seq(0, 10, 0.25)
  # For each lambda,find b_i & b_u, followed by rating prediction & testing
  # note:the below code could take some time  
  rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <- edx %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    predrat <- 
      validation %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    return(RMSE(predrat,validation_CM$rating))
    
  })
  
  # Plot rmses vs lambdas to select the optimal lambda                                                             
  qplot(lambdas, rmses)  
  
  # The optimal lambda                                                             
  lambda <- lambdas[which.min(rmses)]
  
  # Compute regularized estimates of b_i using lambda
  movie_av_reg <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
  
  # Compute regularized estimates of b_u using lambda
  
  user_av_reg <- edx %>% 
    left_join(movie_av_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())
  
  # Predict ratings
  
  predrat_reg <- validation %>% 
    left_join(movie_av_reg, by='movieId') %>%
    left_join(user_av_reg, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>% 
    .$pred
  
  
  # Test and save results
  
  model_3_rmse <- RMSE(validation_CM$rating,predrat_reg)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie and User Effect Model",
                                       RMSE = model_3_rmse ))
  rmse_results %>% knitr::kable()
  rmse_results
  
  
  
  ## Regularized movie, user and year effect model ##
  
  # lambda is a tuning parameter
  # lambda is a tuning parameter
  # Use cross-validation to choose it.
  lambdas <- seq(0, 10, 0.25)
  
  # For each lambda,find b_i1, b_u1 & b_y1, followed by rating prediction & testing
  
  rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i1 <- edx %>% 
      group_by(movieId) %>%
      summarize(b_i1 = sum(rating - mu)/(n()+l))
    
    b_u1 <- edx %>% 
      left_join(b_i1, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u1 = sum(rating - b_i1 - mu)/(n()+l))
    
    b_y1 <- edx %>% 
      left_join(b_i1, by="movieId") %>%
      left_join(b_u1, by="userId") %>%
      group_by(year) %>%
      summarize(b_y1 = sum(rating - b_u1 - b_i1 - mu)/(n()+l))
    
    predrat1 <- 
      validation %>% 
      left_join(b_i1, by = "movieId") %>%
      left_join(b_u1, by = "userId") %>%
      left_join(b_y1, by = "year") %>%
      mutate(pred = mu + b_i1 + b_u1 + b_y1) %>%
      pull(pred)
    
    return(RMSE(predrat1, validation_CM$rating))
  })
  
  # Plot rmses vs lambdas to select the optimal lambda                                                             
  qplot(lambdas, rmses)  
  
  # The optimal lambda                                                             
  lambda <- lambdas[which.min(rmses)]
  
  # Compute regularized estimates of b_i1 using lambda
  movie_av_reg1 <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i1 = sum(rating - mu)/(n()+lambda), n_i1 = n())
  
  # Compute regularized estimates of b_u1 using lambda
  
  user_av_reg1 <- edx %>% 
    left_join(movie_av_reg1, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u1 = sum(rating - mu - b_i1)/(n()+lambda), n_u1 = n())
  
  # Compute regularized estimates of b_y1 using lambda
  
  year_av_reg1 <- edx %>% 
    left_join(movie_av_reg1, by='movieId') %>%
    left_join(user_av_reg1, by='userId') %>%
    group_by(year) %>%
    summarize(b_y1 = sum(rating - mu - b_i1 - b_u1)/(n()+lambda), n_y1 = n())
  
  
  # Predict ratings
  
  predrat_reg1 <- validation %>% 
    left_join(movie_av_reg1, by='movieId') %>%
    left_join(user_av_reg1, by='userId') %>%
    left_join(year_av_reg1, by = 'year') %>%
    mutate(pred = mu + b_i1 + b_u1 + b_y1) %>% 
    .$pred
  
  
  # Test and save results
  
  model_4_rmse <- RMSE(validation_CM$rating,predrat_reg1)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie User and Year Effect Model",
                                       RMSE = model_4_rmse ))
  rmse_results %>% knitr::kable()
  rmse_results
  
  