##### Importing necessary packages #####
packages_needed <- c('rvest','XML','stringr',
                     'tidyverse','SnowballC', 'slam', 'tm', 'RWeka', 'Matrix',
                     'stringi', 'dplyr', 'caret', 'readr', 'tidytext', 'randomForest', 
                     'ggplot2', 'h2o', 'ranger', 'e1071')
for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}

##### PreProcessing #####
# loading data 
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



##### Baseline NLP #####

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

df <- as.data.frame(as.matrix(tdm), stringsAsFactors=False)
df <- cbind(df,target_rating)

# Creating test and train data
samplesize <- floor(0.7*nrow(df))
set.seed(200)
train_x <- sample(seq_len(nrow(df)),size = samplesize)
train <- df[train_x,]
test <- df[-train_x,]


##### Baseline model fitting #####

#CV
tr_control <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = prSummary)

# Logistic regression 
log_model <- train(
  target_rating ~ ., 
  data = train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = tr_control
)

# Support vector machines 
svmModel <- train(
  target_rating ~ ., 
  data = train, 
  method = "svmRadial",
  trControl = tr_control
)


# Random forest
forest_model <- train(
                target_rating ~ ., 
                data = train, 
                method = "rf",
                trControl = tr_control
)

# testing Logistic regression 
pred_class_log_model <- predict(log_model, test)
confusionMatrix(
  data = relevel(pred_class_log_model, ref = "1"), 
  reference = relevel(test$target_rating, ref = "1"))

# Testing SVM 
pred_class_svmModel <- predict(svmModel, test)
confusionMatrix(
  data = relevel(pred_class_svmModel, ref = "1"), 
  reference = relevel(test$target_rating, ref = "1"))

# Testing Random forest 
pred_class_forest_model <- predict(forest_model, test)
confusionMatrix(
  data = relevel(pred_class_forest_model, ref = "1"), 
  reference = relevel(test$target_rating, ref = "1"))




##### Extended preproccesing #####
#using same dataset as baseline models before removing stopwords and keeping no, not and nor 
nlp_reviews_extended <- nlp_reviews 
forremoval <- stopwords('english')
nlp_reviews_extended <- tm_map(nlp_reviews_extended, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")]))

# Creating train and test set 
tdm_extended <- DocumentTermMatrix(nlp_reviews_extended)
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
extended_train  <- cbind(target_rating,svdUnigram[[1]])
extended_test <- cbind(target_rating,svdUnigram[[2]])


##### Extended NLP model fitting #####

#logistic regression
#Preprocess the data with zv, center and scaling.
blueprint <- recipe(target_rating ~ ., data = train) %>%
  step_YeoJohnson(all_numeric()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())


# Second, train the blueprint on training data
prepare <- prep(blueprint, training = train)
prepare

# Lastly, we can apply our blueprint to new data
baked_train <- bake(prepare, new_data = train)
baked_test <- bake(prepare, new_data = test)
baked_train

set.seed(123)
LR <- train(
  blueprint, 
  data = extended_train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = tr_control
)

#Support Vector Machines
grid_search <- expand.grid(
  cost = 10^seq(-2, 1, by = .25),
  gamma = seq(0, 1, by = .2)
)
SVM <- train(
  blueprint, 
  data = extended_train,
  method = "svmRadial",
  tuneGrid = hyper_grid,
  trControl = tr_control
)

### Random Forest Model
# create tuning grid
hyper_grid <- expand.grid(
  mtry = floor(21 * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8)
)


for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = blueprint, 
    data            = train, 
    num.trees       = 20 * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
  )
}

seed = 123
rf <- ranger(
  blueprint, 
  data = extended_train,
  respect.unordered.factors = "order",
  tuneGrid = hyper_grid,
  trControl = tr_control
)


#Testing logistic regression
LR_pred_class <- predict(LR, extended_test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(extended_test$target_rating, ref = "1")
)

#Testing logistic regression
LR_pred_class <- predict(LR, extended_test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(extended_test$target_rating, ref = "1")
)

#Testing Support Vector Machines
SVM_pred_class <- predict(SVM, extended_test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(extended_test$target_rating, ref = "1")
)

#Random Forest
RF_pred_class <- predict(RF, extended_test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(extended_test$target_rating, ref = "1")
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


words_sentiment <- words %>%
  bind_tf_idf(word, sentiment, n) %>% 
  arrange(desc(tf_idf))
words_sentiment


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

### WordCloud Building for positive and negative reviews.
WordCloud <- function( x = buildACorpus() ) {
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

WordCloud()