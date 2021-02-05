## Importing necessary packages
if (!require("rvest")) install.packages("rvest", quiet=TRUE) ; require("rvest")
if (!require("XML")) install.packages("XML", quiet=TRUE) ; require("XML")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")
if (!require("tidyverse")) install.packages("tidyverse", quiet=TRUE) ; require("tidyverse")

# Function to retrieve Review Score 
getReviewScore <- function(x){
  stars_html <- html_nodes(x,'.ratings-imdb-rating')
  stars <- html_text(stars_html)
  stars <- str_extract(stars[1], "\\d\\.\\d")
  return(stars)
}

# Initialize dataframe to store the movie id
all_movies <- data.frame(titles=character(),
                         imdb_ratings=integer(),
                         movie_id=character(),
                         stringsAsFactors=FALSE)

# scrape movie infos, return to all_movies dataframe
# output column names: titles	imdb_ratings	movie_id
for (i in 1:111){
  url <- paste0("https://www.imdb.com/search/title?title_type=feature&release_date=2019-04-01,2020-04-01&count=100&start=",i,"01&ref_=adv_nxt")
  url <- URLdecode(url)
  webpage <- read_html(url)
  films_html <- html_nodes(webpage,'.mode-advanced')
  print(i)
  Sys.sleep(5)
  for (k in 1:length(films_html)){
    
    titles_html <- html_nodes(films_html[[k]],'.lister-item-header a')
    titles <- html_text(titles_html)
    
    stars_html <- html_nodes(films_html[[k]],'.ratings-imdb-rating')
    stars <- html_text(stars_html)
    imdb_ratings <- str_extract(stars[1], "\\d\\.\\d")
    
    href_html <- html_nodes(films_html[[k]],'a')%>% html_attr('href')
    movie_id <- strsplit(href_html[[1]],"/")[[1]][3]
    
    this_movie <- as.data.frame(cbind(titles,imdb_ratings,movie_id))
    all_movies <- rbind(all_movies,this_movie)
  }
  
  if(nrow(all_movies)%%1000==0){write.csv(all_movies,'all_movies.csv')}
}
# export to csv so we only have to run it once.
write.csv(all_movies,'all_movies.csv')

#read the csv
all_movies<-read.csv(file="all_movies.csv", header=TRUE, sep=",")


# scrape movie review using movie id as a key, return data to combine with all_reviews dataframe

all_reviews <- data.frame(id=character(),
                          comment_titles=character(),
                          ratings=integer(),
                          comments_body=character(), 
                          stringsAsFactors=FALSE)

all_ids <- as.character(all_movies$movie_id)

for (id in all_ids){
  url <- paste0("https://www.imdb.com/title/",id,"/reviews?ref_=tt_urv")
  url <- URLdecode(url)
  webpage <- read_html(url)
  check_review<- html_nodes(webpage,'.article')[[1]]%>%html_text()
  zero_review<- str_detect(check_review,"0 Reviews")
  if(zero_review==TRUE)
    
  {Sys.sleep(5)}
  else{
    films_html <- html_nodes(webpage,'.lister-item-content')
    for (k in 1:length(films_html)){
      comment_titles_html <- html_nodes(films_html[[k]],'.title')
      comment_titles <- html_text(comment_titles_html)
      comment_titles <-str_trim(gsub("\r?\n|\r", " ", comment_titles))
      
      ratings_html <- html_nodes(films_html[[k]],'.ipl-ratings-bar')
      ratings <- html_text(ratings_html)
      ratings<- str_extract(ratings, "(\\d)+")
      if(identical(ratings, character(0))){ratings<-0} #replace missing rating with 0
      
      comments_body_html <- html_nodes(films_html[[k]],'.show-more__control')
      comments_body <- html_text(comments_body_html)
      comments_body <-str_trim(gsub("\r?\n|\r", " ", comments_body))
      
      this_review <- as.data.frame(cbind(id,comment_titles,ratings,comments_body))
      all_reviews <- rbind(all_reviews,this_review)

      write.csv(c(id,match(id,all_ids)),'last_id.csv')
    }
  }

  if(nrow(all_reviews)%%250==0){write.csv(all_reviews,'all_reviews.csv')}
}
# export to csv because its heavy to run
write.csv(all_reviews,'C:/Users/siras/Desktop/Uni/Applied datascience/project')


## PreProcessing

library('dplyr')
packages_needed <- c('SnowballC','slam','tm',
                     'RWeka','Matrix','rvest',
                     'XML','stringr','stringi',
                     'dplyr','tidyverse', "caret")

for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}

# loading data 
library(readr)
all_reviews <- read_csv("all_reviews.csv")

# Check rating distribution
table(all_reviews$ratings)

# Removing reviews with no label (rating==0)
onlyExtreme <- all_reviews %>%
  filter(!ratings %in% 0)

# Making target variable definition  
onlyExtreme <- onlyExtreme %>% 
  mutate(count_pos = ifelse(ratings > 5, 1, 0))

#Random sampling from each category to balance the data
new_data_pos <- all_reviews %>% 
  filter(count_pos == '1')

new_data_neg <- all_reviews %>% 
  filter(count_pos == '0')

# Taking random sample (8000) of both negative and positive reviews
new_data_pos_sample <- sample_n(new_data_pos, 8000, replace = FALSE, prob = NULL)
new_data_neg_sample <- sample_n(new_data_neg, 8000, replace = FALSE, prob = NULL)

# Putting the two datasets together again. 
data <- rbind(new_data_pos_sample, new_data_neg_sample)

nlp_reviews <- sapply(data$comments_body,function(x) iconv(x, 'utf8', 'ascii',""))
nlp_reviews <- VCorpus(VectorSource(nlp_reviews))

# Removing punctation, numbers and whitespaces 
nlp_reviews <- tm_map(nlp_reviews, removePunctuation)
nlp_reviews <- tm_map(nlp_reviews, removeNumbers)
nlp_reviews <- tm_map(nlp_reviews, stripWhitespace)
nlp_reviews <- tm_map(nlp_reviews,content_transformer(tolower))
forremoval <- stopwords('english')
nlp_reviews_baseline <- tm_map(nlp_reviews, removeWords,forremoval)

# Making term-document matrix 
tdm <- DocumentTermMatrix(nlp_reviews_baseline)

#removing sparseterms
tdm <- removeSparseTerms(tdm, 0.99)

# Creating dataframe 
target_rating <- as.factor(data$count_pos)

library(tidytext)
df <- as.data.frame(as.matrix(tdm), stringsAsFactors=False)
df <- cbind(df,target_rating)

# Creating test and train data
samplesize <- floor(0.7*nrow(df))
set.seed(200)
train_x <- sample(seq_len(nrow(df)),size = samplesize)
train <- df[train_x,]
test <- df[-train_x,]


# Baseline models 
# Log model 
log_model <- train(
  target_rating ~ ., 
  data = train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = trainControl(method = "cv", number = 10, repeats = 5),
)

# testing log 
pred_class_log_model <- predict(log_model, test)
confusionMatrix(
  data = relevel(pred_class_log_model, ref = "1"), 
  reference = relevel(test$target_rating, ref = "1")
  

# Support vector machines 

svmModel <- train(
  target_rating ~ ., 
  data = train, 
  method = "svmRadial",
  trControl = trainControl(method = "cv", number = 10, repeats = 5)
)

# Testing SVM 
pred_class_svmModel <- predict(svmModel, test)
confusionMatrix(
  data = relevel(pred_class_svmModel, ref = "1"), 
  reference = relevel(test$target_rating, ref = "1"))

# Random forest that couldn't run.
#forest_model <- train(
# target_rating ~ ., 
#data = train, 
#method = "rf",
#)

# Extended preproccesing - using same dataset as baseline models before removing stopwords and keeping no, not and nor 
nlp_reviews_extended <- nlp_reviews 
forremoval <- stopwords('english')
nlp_reviews_extended <- tm_map(nlp_reviews_extended, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")]))

# Creating train and test set 

tdm_extended <- DocumentTermMatrix(nlp_reviews_extended)
library(tidytext)
target_rating <- as.factor(data$count_pos)
df_ext <- as.data.frame(as.matrix(tdm_extended), stringsAsFactors=False)
df_ext <- cbind(df,target_rating)

# Creating test and train data
samplesize <- floor(0.7*nrow(df_ext))
set.seed(200)
train_x <- sample(seq_len(nrow(df_ext)),size = samplesize)
train_ext <- df[train_x,]
test_ext <- df[-train_x,]

train_ext <- VCorpus(VectorSource(train_ext))
test_ext <- VCorpus(VectorSource(test_ext))

# Using bigram, TF-IDF weighting and remove 

# creating function - making test and training set with n-grams and tf-idf weighting. 
Ngram <- function(trainset,testset,mingram,maxgram){

  outputlist <- list()
  
  # training
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mingram, max = maxgram))
  train <- DocumentTermMatrix(trainset, control = list(tokenize = Tokenizer,
                                                     weighting = function(x) weightTfIdf(x)))
  # test
  test <- DocumentTermMatrix(testset, control = list(tokenize = Tokenizer,
                                                       weighting = function(x) weightTfIdf(x)))
  
  # Due to possibility of test and training having different columns after extending preprocessing, we make sure that train and test have same columns. 
  Intersect <- test[,intersect(colnames(test), colnames(train))]
  diffCol <- train[,setdiff(colnames(train),colnames(test))]
  newCols <- as.simple_triplet_matrix(matrix(0,nrow=test$nrow,ncol=diffCol$ncol))
  newCols$dimnames <- diffCol$dimnames
  testNew<-cbind(Intersect,newCols)
  testNew<- testNew[,colnames(train)]
  
  ## Convert term document matrices to common sparse matrices to apply efficient SVD algorithm
  dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}
  outputlist<- list(train=dtm.to.sm(tr),test=dtm.to.sm(testNew))
  
  return(outputlist)
}

# 1 for unigram and 2 for bigram 
unigram <- Ngram(train_ext,test_ext,1,2)

SVD_function <- function(inputset,k){
  outputlist <- list()
  
  outputlist[[i]]<-list()
  
  trainer <- irlba(t(inputset[[1]]), nu=k, nv=k)
  
  tester <- as.data.frame(as.matrix(inputset[[2]] %*% trainer$u %*%  solve(diag(trainer$d))))
  
  outputlist<- list(train = as.data.frame(trainer$v), test= tester)
  
  return(outputlist)
}

#running SVD function on our dataset with k=50.
svdUnigram <- SVD_all(unigram,50)

#Assigning test and training data set
train  <- cbind(target_rating,svdUnigram[[1]])
test <- cbind(target_rating,svdUnigram[[2]])


###logistic regression

#Preprocess the data with zv, center and scaling.
tr_recipe <- recipe(target_rating ~ ., data = train) %>%
  step_YeoJohnson(all_numeric()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())


# Second, train the blueprint on training data
prepare <- prep(tr_recipe, training = train)
prepare

# Lastly, we can apply our blueprint to new data
baked_train <- bake(prepare, new_data = train)
baked_test <- bake(prepare, new_data = test)
baked_train

set.seed(123)
lr <- train(
  tr_recipe, 
  data = baked_train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = trainControl(method = "cv", number = 10, repeats = 5) 
)

pred_class <- predict(lr, baked_test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(baked_test$target_rating, ref = "1")
)

# Logistic regression preprocessed:
train$rating_model <- as.factor(train$target_rating)

set.seed(123)
logModel <- train(
  target_rating ~ ., 
  data = train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = trainControl(method = "cv", number = 10, repeats = 5)
)

pred_class <- predict(logModel, test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(test$target_rating, ref = "1")
)

### Random Forest Model
if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")


library(dplyr)
library(ggplot2)
library(ranger)
library(h2o)

# Create and train rf model
rf <- ranger(
  target_rating ~ ., 
  data = train,
  respect.unordered.factors = "order",
  seed = 123
)

# Show RMSE by taking square root of prediction error
(default_rmse <- sqrt(rf$prediction.error))

# create tuning grid
hyper_grid <- expand.grid(
  mtry = floor(21 * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)


for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = target_rating ~ ., 
    data            = train, 
    num.trees       = 20 * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
  )
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}


# check top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

### implement ntree 1001, so that it can decide between 0 or 1
RF_model_train <- randomForest(x=train[,1:50],y=train[,51],importance=TRUE,ntree=1001, mtry=8, sample.fraction=.5, min.node.size=3)
RF_predict <- predict(RF_model_train,test[,1:50])[,51]
# This returns the probabilities, which is more useful for the evaluation measures


if (!require("caret")) install.packages("caret", quiet=TRUE) ; require("caret")
if (!require("e1071")) install.packages("e1071", quiet=TRUE) ; require("e1071")
pred <- as.factor(ifelse(RF_predict>0.5,1,0))
confusionMatrix(pred, test[,1])

# Support vector machines
tune.linear <- tune(svm, target_rating ~ ., data = train, kernel = "radial", ranges = list(cost = 10^seq(-2, 1, by = .25)))
tune.linear1 <- tune(svm, target_rating ~ ., data = train, kernel = "radial", ranges = list(gamma = seq(0, 1, by = .2)))

tune.nonlinear <- tune(svm,target_rating  ~ ., data = train, kernel = "radial", ranges = list(cost = 10^seq(-1, 2, by = 0.5), gamma=c(0.005,0.01,0.05,0.1)))

svm.nonlinear <- svm(target_rating ~ ., kernel = "radial", data = train)

tune.nonlinear$best.parameter$gamma
tune.nonlinear$best.parameter$cost
svm.nonlinear <- svm(target_rating ~ ., kernel = "radial", data = train, cost = tune.nonlinear$best.parameters$cost, gamma= tune.nonlinear$best.parameters$gamma)

test.predictions <- predict(svm.nonlinear, test)
table(test$target_rating, test.predictions)

confusionMatrix(
  data = relevel(test.predictions, ref = 1), 
  reference = relevel(test$target_rating, ref = 1)
)

## Visualizations

### Relationships between words

#Count bigrams etc.

#Creating TF_iDF on unigrams

comments_frame <-mutate(comments_frame, sentiment = as.factor(data$count_pos))

words <- comments_frame %>%
  unnest_tokens(word, text) %>%
  count(sentiment, word, sort = TRUE)


total_words <- words %>% 
  group_by(sentiment) %>% 
  summarize(total = sum(n))

words <- left_join(words, total_words, by = "sentiment")

words


freq_by_rank <- words %>%
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank


words <- words %>%
  bind_tf_idf(word, sentiment, n) %>% 
  arrange(desc(tf_idf))


words


#Making tf-idf on bigrams

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


bigram <- comments_frame %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

unigram <- comments_frame %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 1)

count <- bigram %>%
  count(bigram, sort = TRUE)

#Trigrams
comments_frame %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


#TF-IDF
bigrams_separated <- bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")


bigram_tf_idf <- bigrams_united %>%
  count(sentiment, bigram) %>%
  bind_tf_idf(sentiment, bigram, n) %>%
  arrange(desc(tf_idf))


bigram_tf_idf

### WordCloud Building for positve and negative reviews.
builWordCloud <- function( x = buildACorpus() ) {
  x <- as.matrix(x)
  View(x)
  sorted <- sort(rowSums(x), decreasing=TRUE)
  dfnames <- names(sorted)
  myDF <- data.frame(word=dfnames, freq=sorted)
  View(myDF)
  png(filename='IMDbWordCloud.png')
  wordcloud(myDF$word, myDF$freq, min.freq=400, scale=c(3.5,0.5), colors=brewer.pal(8,"Dark2"))
  dev.off()
}


builWordCloud()



