#################################################
# Create edx set and validation set             #
#################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
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

# Clear memory
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#################################################
# END EDX import dataset                        #
#################################################

# Store edx and validation dataset
# (do not have to reload them each time ...)
# edx_init <- edx
# validation_init <- validation

#################################################
# Create train - test dataset                   #
#################################################
# edx <- edx_init
# validation <- validation_init

pre_process_data_FT <- function(df){
  # Extract year as a new column
  df$year = as.numeric(substr(df$title, nchar(df$title)-4, nchar(df$title)-1))
  df$title = substr(df$title, 0 , nchar(df$title)-7)

  # Create new genres columns
  # Select all genres (uniques)
  list_genre <- unique(separate_rows(data = df["genres"], genres, sep = "\\|"))
  nb_genres <- nrow(list_genre)
  # Create empty matrix
  genres_col <- as.data.frame(matrix(0, ncol = nb_genres, nrow = nrow(df)))
  # Set col names
  colnames(genres_col) <- as.list(list_genre)[[1]]
  # Add columns to edx dataframe
  df <- cbind(df, genres_col)

  rm(genres_col)
  gc()
  df
  set_genre <- function(row){
    genres <- strsplit(row["genres"], "\\|")[[1]]
    row[genres] <- 1
    return(row)
  }
  df <- as.data.frame(t(apply(df, MARGIN=1, FUN=set_genre)))

  # remove genre columns
  df <- df[setdiff(names(df), c("genres"))]

  df <- apply(df, 2, as.numeric)
  return(df)
}

# Resource consuming : you can try and see if RMSE is improved
# edx <- pre_process_data_FT(edx)
# validation <- pre_process_data_FT(validation)



set.seed(2019, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") 

# Add rows removed from validation dataset back into edx dataset
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Create a set of lambdas to test for regularization
lambdas <- seq(0, 10, 0.5)

#For each value of lambda in lambdas, calculate the RMSE
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- test_set%>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  print(l)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# Plot RMSES vs Lambda
qplot(lambdas, rmses)  
# Pick the value of lambda that minimizes rmse
lambda <- lambdas[which.min(rmses)]
paste("Lambda that minimizes rmse: ", lambda)


# Use the best lambda value to train the model on the edx dataset
l <- lambda
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))


# Use the model to predict ratings for the validation data set
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate the final_rmse
final_rmse <- RMSE(predicted_ratings, validation$rating)
print(paste0("RMSE = ", final_rmse))

# Clear useless variables
rm(l, predicted_ratings, removed, RMSE, temp, test_index, test_set, train_set)


# Save workspace for R Markdown
save.image(file = "processed_work_space.RData")

