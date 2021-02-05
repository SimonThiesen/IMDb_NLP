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
all_reviews <- read_csv("Desktop/all_reviews.csv")

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

# Removing stopwords, but keeping no, not or nor 
stopwordsremove <- stopwords('english')
stopwordsremove <- c(forremoval[!forremoval %in% c("no","not","nor")])

data_words <- tokenize_words(data$comments_body, lowercase = T, stopwords = stopwordsremove)
View(data_words)

# Making corpus 
nlp_reviews <- VCorpus(VectorSource(data_words))

# Removing punctation, numbers and whitespaces 
nlp_reviews <- tm_map(nlp_reviews, removePunctuation)
nlp_reviews <- tm_map(nlp_reviews, removeNumbers)
nlp_reviews <- tm_map(nlp_reviews, stripWhitespace)

View(nlp_reviews)
str(nlp_reviews)

nlp_reviews <- data.frame(text=unlist(sapply(nlp_reviews, `[`, "content")), stringsAsFactors=F)
nlp_reviews[[1]]
View(nlp_reviews)
# 
rating <- data$count_pos
rating
View(rating)
length(rating)
table(rating)

set.seed(500)

y <- as.factor(rating)
p <- 0.7
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)


training_locations <- c(class1_train,class2_train)
length(training_locations)

txt_l <- list()
txt_l[[2]] <- list()

train <- nlp_reviews[sort(training_locations),1]
test<- nlp_reviews[-sort(training_locations),1]

View(txt_l)
?sort

View(nlp_reviews)
nlp_reviews[2,1]
