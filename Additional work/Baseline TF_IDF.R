library('dplyr')
packages_needed <- c('SnowballC','slam','tm',
                     'RWeka','Matrix','rvest',
                     'XML','stringr','stringi',
                     'dplyr','tidyverse', "caret")

for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}

library(readr)
all_reviews <- read_csv("all_reviews.csv")

View(all_reviews)
table(all_reviews$ratings)

onlyExtreme <- all_reviews %>%
  filter(!ratings %in% 0)
View(onlyExtreme)

onlyExtreme <- onlyExtreme %>% 
  mutate(count_pos = ifelse(ratings > 5, 1, 0))

library(DataExplorer) 
plot_bar(onlyExtreme$count_pos)

# counting numbers of positive and negatives 

table(onlyExtreme$count_pos) # 8940 negative og 15084 positive 
all_reviews <- onlyExtreme

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

data$comments_body[1039]

nlp_reviews <- sapply(data$comments_body,function(x) iconv(x, 'utf8', 'ascii',""))
nlp_reviews <- VCorpus(VectorSource(nlp_reviews))

# Removing punctation, numbers and whitespaces 
nlp_reviews <- tm_map(nlp_reviews, removePunctuation)
nlp_reviews <- tm_map(nlp_reviews, removeNumbers)
nlp_reviews <- tm_map(nlp_reviews, stripWhitespace)
forremoval <- stopwords('english')
nlp_reviews <- tm_map(nlp_reviews, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")])) 

nlp_reviews <- tm_map(nlp_reviews,content_transformer(tolower))

tdm <- DocumentTermMatrix(nlp_reviews,
                          control = list(weighting = weightTfIdf))
tdm 

tdm <- removeSparseTerms(tdm, 0.99)

inspect(tdm[2005:2015,100:103])

rating_model <- as.factor(data$count_pos)

length(rating_model)


library(tidytext)
df <- as.data.frame(as.matrix(tdm), stringsAsFactors=False)
View(df)
df <- cbind(df,rating_model)
table(df$rating_model)
View(df)
samplesize <- floor(0.7*nrow(df))
set.seed(200)
train_x <- sample(seq_len(nrow(df)),size = samplesize)
train <- df[train_x,]
test <- df[-train_x,]


# model building 
logModelTfidfWithtuning <- train(
  rating_model ~ ., 
  data = train, 
  method = "glm",
  family=binomial(link="logit"),
  trControl = trainControl(method = "cv", number = 10, repeats = 5),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)

save(logModelTfidfWithtuning, file="logModelTfidfWithtuning.rds")

pred_class_TfidfWithtuning <- predict(logModelTfidfWithtuning, test)
confusionMatrix(
  data = relevel(pred_class_TfidfWithtuning, ref = "1"), 
  reference = relevel(test$rating_model, ref = "1")
)



------------------------------- 
freq=rowSums(as.matrix(tdm))
head(freq,10)
tail(freq,10)

plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

tail(sort(freq),n=10)

high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

