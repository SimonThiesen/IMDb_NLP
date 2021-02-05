library('dplyr')
packages_needed <- c('SnowballC','slam','tm',
                     'RWeka','Matrix','rvest',
                     'XML','stringr','stringi',
                     'dplyr','tidyverse')

for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}

library(readr)
 all_reviews <- read_csv("all_reviews.csv")

View(all_reviews)

all_reviews <- all_reviews %>% 
  mutate(count_pos = ifelse(ratings > 5, 1, 0))

library(DataExplorer) 
plot_bar(all_reviews$count_pos)
# counting numbers of positive and negatives 

table(all_reviews$count_pos) # 9416 negative og 15084 positive 

#Random sampling from each category to balance the data
new_data_pos <- all_reviews %>% 
  filter(count_pos == '1')

new_data_neg <- all_reviews %>% 
  filter(count_pos == '0')

# Taking random sample (2000) of both negative and positive reviews
new_data_pos_sample <- sample_n(new_data_pos, 2000, replace = FALSE, prob = NULL)
new_data_neg_sample <- sample_n(new_data_neg, 2000, replace = FALSE, prob = NULL)
View(new_data_neg_sample)

# Putting the two datasets together again. 
data <- rbind(new_data_pos_sample, new_data_neg_sample)
View(data)
plot_bar(data$count_pos)


# preprocessing
nlp_reviews <- sapply(data$comments_body,function(x) iconv(x, 'utf8', 'ascii',""))
nlp_reviews <- VCorpus(VectorSource(nlp_reviews))

# Removing punctation, numbers and whitespaces 
nlp_reviews <- tm_map(nlp_reviews, removePunctuation)
nlp_reviews <- tm_map(nlp_reviews, removeNumbers)
nlp_reviews <- tm_map(nlp_reviews, stripWhitespace)
forremoval <- stopwords('english')
nlp_reviews <- tm_map(nlp_reviews, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")])) 
nlp_reviews <- tm_map(nlp_reviews,content_transformer(tolower))

#term matrix

Docmatrix <- DocumentTermMatrix(nlp_reviews)
inspect(Docmatrix)
Docmatrix_Dense <- removeSparseTerms(Docmatrix, 0.99)
inspect(Docmatrix_Dense)
term_list <- Docmatrix_Dense$dimnames$Terms
findFreqTerms(Docmatrix_Dense)
table(term_list)
str(Docmatrix_Dense)

rating_model <- as.factor(data$count_pos)
table()
View(Docmatrix_Dense)

library(tidytext)
df <- as.data.frame(as.matrix(Docmatrix_Dense), stringsAsFactors=False)
df <- cbind(df,rating_model)
table(df$rating_model)

# sample 
samplesize <- floor(0.7*nrow(df))
set.seed(200)
train_x <- sample(seq_len(nrow(df)),size = samplesize)
train <- df[train_x,]
test <- df[-train_x,]


#logistisk regression

set.seed(123)
cv_model3 <- train(
  rating_model ~ ., 
  data = train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5)
)

# trainset
pred_class <- predict(cv_model3, train)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(train$rating_model, ref = "1")
)

# testset 
pred_class <- predict(cv_model3, test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(test$rating_model, ref = "1")
)

save(cv_model3,file = "logistik regression.RData")




# random forrest

RF_model_train <- randomForest(x=train,y=train$rating_model,importance=TRUE,ntree=1001)
RF_model_train
summary(RF_model_train)


pred_class <- predict(RF_model_train, train)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(train$rating_model, ref = "1")
)

pred_class <- predict(RF_model_train, test)
confusionMatrix(
  data = relevel(pred_class, ref = "1"), 
  reference = relevel(test$rating_model, ref = "1")
)

View(df)
?predict
save(RF_model_train,file = "randomforrest.RData")

write.csv(df,"C:\\Users\\Simon\\Documents\\MyData.csv")


