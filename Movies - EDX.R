#################################################
# Create edx dateset and validation set         #
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
edx_init <- edx
validation_init <- validation


#################################################
# Project beginning                             #
#################################################

# Install package if missing
required_packages <- c("tidyr", "dplyr", "rpart","rpart.plot", "randomForest", "ggplot2")
packages_missing <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(packages_missing)) install.packages(packages_missing)
rm(required_packages, packages_missing)

# Load libraries
library(tidyr)        # Separate row
library(dplyr)        # Semin-join ...
library(rpart)        # Decision tree
library(rpart.plot)   # Tree plot
library(randomForest) # RandomForest
library(ggplot2)      # Display


# edx <- edx_init
# validation <- validation_init




#################################################
# Data overview                                 #
#################################################

head(edx)
# head(validation)
summary(edx[seq(1,1000000),])
# summary(validation)

data_plot1 <- edx[seq(1,200000),] %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(Count = n()) %>%
  head(10) %>%
  as.data.frame()

data_plot1$genres <- factor(data_plot1$genres, levels=data_plot1$genres[order(data_plot1$Count)])

plot1 <- ggplot(data_plot1, aes(x=reorder(genres, -Count), y=Count, fill=Count)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(high = "green", low = "red") + 
  ggtitle("Number ") + 
  scale_x_discrete(name ="Genre")
plot1

plot2 <- edx %>%
             ggplot(aes(rating)) + 
             geom_histogram(bins = 20,color="darkblue", fill="darkblue")+
             ggtitle("Distribution of Rating") + 
             theme(plot.title = element_text(color="black", size=13, face="bold"))
plot2


#################################################
# PRE PROCESS                                   #
#################################################
# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# CLASS : results RMSE
setClass(Class="Results",
         representation(
           linear_model="numeric",
           decision_tree="numeric",
           random_forest="numeric"
         )
)
Results <- new("Results",
               linear_model=-1,
               decision_tree=-1,
               random_forest=-1)


pre_process_data_FT <- function(df){
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
  
  set_genre <- function(row){
    genres <- strsplit(row["genres"], "\\|")[[1]]
    row[genres] <- 1
    return(row)
  }
  df <- as.data.frame(t(apply(df, MARGIN=1, FUN=set_genre)))

  # remove genre columns
  df <- df[setdiff(names(df), c("genres","title", "timestamp"))]
  
  
  # Replace - per _ in column names
  columns_names <- colnames(df)
  columns_names <- columns_names[! columns_names %in% c('Sci-Fi', "Film-Noir")]
  colnames(df) <- append(columns_names, c('Sci_Fi', "Film_Noir"))
  
  df <- as.data.frame(apply(df, 2, as.numeric))
  
  return(df)
}

group_genres_columns <- function(df, genre_cols){
  # Let's groupd the genres columns (eg. Drama, Action columns)
  # with few occurence in an "Other" category
  df <- as.data.frame(apply(df, 2, as.numeric))
  q <- quantile(apply(df[genre_cols], 2, sum))
  
  column_below_q1 <- genre_cols[q < q[[2]]]
  
  keep <- function(row){
    return (if(sum(row) > 0) 1 else 0)
  }
  df$other <- apply(df[column_below_q1], 1, keep)
  df <- df[, !names(df) %in% column_below_q1, drop=F]
  return(df)
}

create_year_column <- function(df){
  # Extract year as a new column
  df$year = as.numeric(substr(df$title, nchar(df$title)-4, nchar(df$title)-1))
  df$title = substr(df$title, 0 , nchar(df$title)-7)
  return (df)
}

# Resource consuming : you can try and see if RMSE is improved
# edx <- pre_process_data_FT(edx)
# validation <- pre_process_data_FT(validation)

edx <- create_year_column(edx)
validation <- create_year_column(validation)




#################################################
# Linear model                                  #
#################################################
# Renaing variable as usual analysis
train_set <- edx        # [-test_index, ]
test_set  <- validation # edx[test_index, ]

# Prepare test_set for linear model
test_set_lm <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") 

# Compute RMSE
compute_RMSE <- function(lambda, train_set, test_set){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
            group_by(movieId) %>%
            summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train_set %>% 
            left_join(b_i, by="movieId") %>%
            group_by(userId) %>%
            summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  predicted_ratings <- test_set %>% 
                          left_join(b_i, by = "movieId") %>%
                          left_join(b_u, by = "userId") %>%
                          mutate(pred = mu + b_i + b_u) %>%
                          pull(pred)
  
  print(paste("Lambda:", lambda))
  
  return(RMSE(predicted_ratings, test_set$rating))
}


# Create a set of lambdas to test for regularization
lambdas <- seq(0, 10, 0.5)

#For each value of lambda in lambdas, calculate the RMSE
rmses <- sapply(lambdas, compute_RMSE, train_set=train_set, test_set=test_set_lm)

# Plot RMSES vs Lambda
plot3 <- ggplot(as.data.frame(cbind(lambdas, rmses)), aes(lambdas, rmses)) + 
              geom_line(color='steelblue', size=2) + 
              ggtitle("RMSE vs lambda")
plot3

# Pick the value of lambda that minimizes rmse
lambda <- lambdas[which.min(rmses)]
paste("Lambda that minimizes rmse: ", lambda)


Results@linear_model <- compute_RMSE(lambda, train_set, test_set_lm)
print(paste0("RMSE linear model = ", Results@linear_model))

## Markdown
mu <- mean(train_set$rating)

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))



#################################################
# Decision Tree                                 #
#################################################
# Renaing variable as usual analysis
train_set <- edx       
test_set  <- validation

# Reduce size on training and test dataset
train_set <- train_set[sample(nrow(train_set), 100000), ]
test_set <- test_set[sample(nrow(test_set), 50000),]

# Pre-process
train_set <- pre_process_data_FT(train_set)
test_set <- pre_process_data_FT(test_set)

genre_names <- colnames(train_set)[seq(4, length(colnames(train_set)))]

train_set <- group_genres_columns(train_set, genre_names)
test_set <- group_genres_columns(test_set, genre_names)

feature_names <- colnames(train_set)[! colnames(train_set) %in% c('rating')]

# Prediction formula 
predictor_formula <- paste("rating ~", paste(feature_names, collapse = " + "))
predictor_formula


# Build model
model_tree <- rpart(predictor_formula, data=train_set)

# printcp(model_tree) # display the results 
# plotcp(model_tree)  # visualize cross-validation results 
# summary(model_tree) # detailed summary of splits

rpart.plot(model_tree, cex.main=2,
           main="Classification Tree - Rating",
           box.palette="RdBu", shadow.col="gray", nn=TRUE)

# Evaluate model
preds <- predict(object = model_tree, newdata = test_set)
Results@decision_tree <- RMSE(as.numeric(test_set$rating), as.numeric(preds))
paste("RMSE: ", Results@decision_tree)






#################################################
# Random Forest                                 #
#################################################
# Renaing variable as usual analysis
train_set <- edx        
test_set  <- validation

# Reduce size on training and test dataset
train_set <- train_set[sample(nrow(train_set), 10000), ]
test_set <- test_set[sample(nrow(test_set), 5000),]

# Create feature dataset for training
train_x <- train_set[setdiff(names(train_set), c("rating"))]

# Pre-process data
train_x <- pre_process_data_FT(train_x)
test_set <- pre_process_data_FT(test_set)

# Build model - can take a some minutes
model_randomForest <- randomForest(x = train_x, y = train_set$rating)

# Evaluate model
preds <- predict(object = model_randomForest, newdata = test_set, type = c("class"))

# Results
Results@random_forest <- RMSE(as.numeric(test_set$rating), as.numeric(preds))
paste("RMSE: ", Results@random_forest)


#################################################
# Display results                               #
#################################################

res <- data.frame(Methods=c('linear model', 'decision tree', 'random forest'),
                  RMSE= c(Results@linear_model, Results@decision_tree, Results@random_forest))
res$Methods <- factor(res$Methods, levels=res$Methods[order(res$RMSE)])
# data_plot1$genres <- factor(data_plot1$genres, levels=data_plot1$genres[order(data_plot1$Count)])


plot4 <- ggplot(res, aes(x = Methods, y=RMSE, fill=RMSE)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept=0.87750) + 
  annotate("text", x = 1, y = 0.92, label = "0.87750") + 
  geom_text(aes(label=RMSE), label=sprintf("%0.5f", round(res$RMSE, digits = 5)), vjust=1.6, color="white", size=3.5) +
  ggtitle("RMSE per method")


#################################################
# Save environnement                            #
#################################################

# Clear useless variables
rm(test_set, train_set, model_randomForest, test_set_lm, train_x, preds)

# Save workspace for R Markdown
save.image(file = "processed_work_space.RData")

