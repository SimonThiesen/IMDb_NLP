##### Importing necessary packages #####
packages_needed <- c('rvest','XML','stringr',
                     'tidyverse','SnowballC', 'slam', 'tm', 'RWeka', 'Matrix',
                     'stringi', 'dplyr', 'caret', 'readr', 'tidytext', 'randomForest', 
                     'ggplot2', 'h2o', 'ranger', 'e1071')
for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}



#####WEB SCRAPING IMDb #####

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
write.csv(all_reviews,'C:/Users/simt/AppliedDataScience/project')

