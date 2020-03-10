
################################
# Create edx set, validation set
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

##-------
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
##----------
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

edx %>% select(genres) %>% distinct()
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId)[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

ratings <- as.data.frame(ratings) %>% mutate(userId = as.numeric(userId),
                                              movieId = as.numeric(movieId),
                                              rating = as.numeric(rating),
                                             timestamp = as.numeric(timestamp))

str(movielens)

movielens <- left_join(ratings, movies, by = "movieId")

edx <- as.data.frame(edx) %>% mutate(year = as.numeric(year))


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

#==================================
# Remove unneeded files to free some RAM
rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
   # Checking sample data using head
  head(edx)
  
  # Summarizing the dataset
  summary(edx)

  # Total unique movies and Users 
  edx %>%
    summarize(n_users = n_distinct(userId), 
              n_movies = n_distinct(movieId))
  
  ##Data Pre-processing###
  
  # Modify the year as a column in the edx & validation datasets
  
  edx <- edx%>%separate(title,c("name", "year"), "\\s*\\((?=\\d+\\)$)|\\)$")
  validation <- validation%>%separate(title,c("name", "year"), "\\s*\\((?=\\d+\\)$)|\\)$")

  str(edx)
  str(validation)
  # edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
  # validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
  
  head(split_edx)
  head(validation)
  
  # Modify the genres variable in the edx & validation dataset (column separated)
  
  # split_edx  <- edx  %>% separate_rows(genres, sep = "\\|")
  # split_valid <- validation  %>% mutate(year = as.numeric(str_sub(validation$title,-5,-2))) %>% separate_rows(genres, sep = "\\|")
  # split_valid_CM <- validation_CM %>% mutate(year = as.numeric(str_sub(validation_CM$title,-5,-2))) %>% separate_rows(genres, sep = "\\|")
  
  split_edx$genres
  
  #Visualization
  #Distribution of Ratings
    
  edx %>% 
    ggplot(aes(rating)) + geom_histogram(binwidth=0.25, fill = "red") +
    scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
    scale_y_continuous(breaks = c(seq(0, 3000000, 250000))) +
    ggtitle("Distribution of Rating")
  
  # Ratings per movie
  edx %>%
    count(movieId) %>%
    ggplot(aes(n)) +
    geom_histogram(bins = 30, color = "maroon", fill = "green") +
    scale_x_log10() +
    xlab("No. of ratings") +
    ylab("No.of movies") +
    ggtitle("No. ratings per movie")
  

  
  # Movies rated only once
  edx %>%
    group_by(movieId) %>%
    summarize(count = n()) %>%
    filter(count == 1) %>%
    left_join(edx, by = "movieId") %>%
    group_by(title) %>%
    summarize(rating = rating, n_rating = count) %>%
    slice(1:20) %>%
    knitr::kable()
  
  # Number of users by Number of Users
  edx %>%
    count(userId) %>%
    ggplot(aes(n)) +
    geom_histogram(bins = 30, color = "white", fill = "maroon") +
    scale_x_log10() +
    xlab("Number of ratings") + 
    ylab("Number of users") +
    ggtitle("Number of ratings given by users")
  
  str(edx)
  
  str(split_edx$genres)
  
  # Total movie ratings per genre
 data.frame( edx%>%
    group_by(genres) %>%
    summarize(count = n()) %>%
    arrange(desc(count)))
  
  # Genres popularity per year
  genres_popularity <- edx %>%
    na.omit() %>% # omit missing values
    select(movieId, year, genres) %>% # select columns we are interested in
    mutate(genres = as.factor(genres)) %>% # turn genres in factors
    group_by(year, genres) %>% # group data by year and genre
    summarise(number = n()) %>% # count
    complete(year = full_seq(year, 1), genres, fill = list(number = 0)) # add missing years/genres
  
  ## The effects of release year and genres on ratings
  # Rating vs genres
  
  edx %>% group_by(genres) %>%
    summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
    mutate(genres = reorder(genres, avg)) %>%
    ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
    geom_point() +
    geom_errorbar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Rating vs release year 
  
  class(edx$year)
  edx %>% filter(userId==7)
  edx %>% filter(grepl('24', title))
  
  df <- data.frame(file_name = c("Mystery Science Theater 3000",
                                 "24 (2007)", 
                                 "24"))
  
  
  df %>% separate(file_name,
                  c("title","Year"))
  
  edx %>% group_by(year) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(year, rating)) +
    geom_point() +
    scale_x_continuous(limit=c(1910,2010)) +
    geom_smooth()
  
  ####Data Analysis####
  
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
  movie_av %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("black"))
  
  # simple model taking into account the user effect b_u
  
  user_av <- edx %>% 
    left_join(movie_av, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
  user_av %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))
  
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
    
    b_y <- edx %>% 
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%
      group_by(year) %>%
      summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l))
    
    
    predrat <- 
      validation %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      left_join(b_y, by = "year") %>%
      mutate(pred = mu + b_i + b_u + b_y) %>%
      pull(pred)
    
    return(RMSE(predrat, validation$rating))
  })
  
  
  # Plot rmses vs lambdas to select the optimal lambda                                                             
  qplot(lambdas, rmses)  
  
  
  # The optimal lambda                                                             
  lambda <- lambdas[which.min(rmses)]
  lambda
  #lambda <- 5.25
  
  # Compute regularized estimates of b_i using lambda
  movie_av_reg <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())
  
  # Compute regularized estimates of b_u using lambda
  
  user_av_reg <- edx %>% 
    left_join(movie_av_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())
  
  year_av_reg <- edx %>% 
    left_join(movie_av_reg, by='movieId') %>%
    left_join(user_av_reg, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
  
  # Predict ratings
  
  predrat_reg <- validation %>% 
    left_join(movie_av_reg, by='movieId') %>%
    left_join(user_av_reg, by='userId') %>%
    left_join(year_av_reg, by = 'year') %>%
    mutate(pred = mu + b_i + b_u + b_y) %>% 
    .$pred
  
  # Test and save results
  
  model_3_rmse <- RMSE(validation_CM$rating,predrat_reg)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Regularized Movie and User Effect Model",  
                                       RMSE = model_3_rmse ))
  rmse_results %>% knitr::kable()
  rmse_results
  
  ##Regularized with all effects Model ##
  
  # The approach utilized in the above model is implemented below with the added genres and year effects
  # b_y and b_g represent the year & genre effects, respectively
  
  lambdas <- seq(0, 20, 1)
  
  # Note: the below code could take some time 
  
  rmses <- sapply(lambdas, function(l){
    
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
      summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())
    
      predrat <- validation %>% 
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      left_join(b_y, by = 'year') %>%
      mutate(pred = mu + b_i + b_u + b_y) %>% 
      .$pred
    
    return(RMSE(validation_CM$rating,predrat))
  })
  
  # Compute new predictions using the optimal lambda
  # Test and save results 
  
  qplot(lambdas, rmses)  
  lambda_2 <- lambdas[which.min(rmses)]
  lambda_2
  
  movie_reg_avgs_2 <- split_edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda_2), n_i = n())
  
  user_reg_avgs_2 <- split_edx %>% 
    left_join(movie_reg_avgs_2, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_2), n_u = n())
  
  year_reg_avgs <- split_edx %>%
    left_join(movie_reg_avgs_2, by='movieId') %>%
    left_join(user_reg_avgs_2, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_2), n_y = n())
  
  
  genre_reg_avgs <- split_edx %>%
    left_join(movie_reg_avgs_2, by='movieId') %>%
    left_join(user_reg_avgs_2, by='userId') %>%
    left_join(year_reg_avgs, by = 'year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_2), n_g = n())
  
  predicted_ratings <- split_valid %>% 
    left_join(movie_reg_avgs_2, by='movieId') %>%
    left_join(user_reg_avgs_2, by='userId') %>%
    left_join(year_reg_avgs, by = 'year') %>%
    left_join(genre_reg_avgs, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
    .$pred
  
  model_4_rmse <- RMSE(split_valid_CM$rating,predicted_ratings)
  rmse_results <- bind_rows(rmse_results,
                            data_frame(method="Reg Movie, User, Year, and Genre Effect Model",  
                                       RMSE = model_4_rmse ))
  rmse_results %>% knitr::kable()
  
  ## Results ##
  
  # RMSE resutls overview
  rmse_results %>% knitr::kable()
  
  # Round predicted ratings & confirm that they're between 0.5 & 5
  
  predicted_ratings <- round(predicted_ratings*2)/2
  
  predicted_ratings[which(predicted_ratings<1)] <- 0.5
  predicted_ratings[which(predicted_ratings>5)] <- 5
  
  ##### Chosen Model: (Model 4)######
  
  lambda_3<-14
  # Redo model 4 analysis
  
  movie_reg_avgs_2 <- split_edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda_3), n_i = n())
  
  user_reg_avgs_2 <- split_edx %>% 
    left_join(movie_reg_avgs_2, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_3), n_u = n())
  
  year_reg_avgs <- split_edx %>%
    left_join(movie_reg_avgs_2, by='movieId') %>%
    left_join(user_reg_avgs_2, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+lambda_3), n_y = n())
  
  genre_reg_avgs <- split_edx %>%
    left_join(movie_reg_avgs_2, by='movieId') %>%
    left_join(user_reg_avgs_2, by='userId') %>%
    left_join(year_reg_avgs, by = 'year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n()+lambda_3), n_g = n())
  
  ## Adding all effects to the validation set & predicting the ratings
  ## Group by userId & movieID
  ## Compute each prediction's mean 
  
  predicted_ratings <- split_valid %>% 
    left_join(movie_reg_avgs_2, by='movieId') %>%
    left_join(user_reg_avgs_2, by='userId') %>%
    left_join(year_reg_avgs, by = 'year') %>%
    left_join(genre_reg_avgs, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
    group_by(userId,movieId) %>% summarize(pred_2 = mean(pred))
  
  ## Round predicted_ratings & confirm that they're between 0.5 & 5
  
  predicted_ratings <- round(predicted_ratings*2)/2
  predicted_ratings$pred_2[which(predicted_ratings$pred_2<1)] <- 0.5
  predicted_ratings$pred_2[which(predicted_ratings$pred_2>5)] <- 5
  
  ## View the final predictions 
  
  View(predicted_ratings)
  
  df <- data.frame(a = c("1", "2", NA, NA), b = c(NA, NA, "3", "4"), c = c(NA, "5", "6", NA), stringsAsFactors = F)
  library(dplyr)
  df <- df %>%
    mutate(a = coalesce(a,b))
  
  df$w <- rowMeans(data[, c("a", "b")], na.rm=TRUE)
  
  df %>% unite("w", a:b, na.rm = TRUE, remove = FALSE)
  
  df <- data.frame(a = c("American President, The (1995)", "Dracula", "Ace Ventura", "Lawnmower Man 2"), b = c(NA, "When Nature Calls (1995)", "When Nature Calls (1995)", "Beyond Cyberspace (1996)"),stringsAsFactors = F)
  df$a
  
  data <- data.frame('a' = c('A','B','C','D','E'),
                     'x' = c(1,2,NA,NA,NA),
                     'y' = c(NA,NA,3,NA,NA),
                     'z' = c(NA,NA,NA,4,5))
  
  df %>% unite(a,b)