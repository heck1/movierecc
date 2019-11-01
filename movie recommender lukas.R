rm(list=ls())
.libPaths( c( "H:/Stat" , .libPaths() ) )

library(tidyverse)
library(dplyr)
library(scales)
library(jsonlite)
library(knitr)
library(kableExtra)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(tidytext)
library(wordcloud)
require(ggthemes)
require(RColorBrewer)
require(viridis)


setwd("H:/LH/test/")

movies <- read_csv(("tmdb_5000_movies.csv"), na="NA")
credits <- read_csv(( "tmdb_5000_credits.csv"),  na="NA")


movies$year <- year(movies$release_date)
C <- mean(movies$vote_average)
m <- quantile(movies$vote_count, 0.75)
movies$weighted_rating <- (movies$vote_average*movies$vote_count + C*m)/(movies$vote_count + m)








#### Genres ########

genres <- movies %>%
  filter(nchar(genres)>2) %>%
  mutate(js = lapply(genres, fromJSON)) %>%
  unnest(js) %>%
  select(id, title, genres=name) 


genres3 <- genres
genres3$order <- 0
genres3$order[1] <- 1

for(i in 1:(nrow(genres3)-1)) {
  if(genres3$id[i+1]!=genres3$id[i]){
    genres3$order[i+1] <- 1
  } else {genres3$order[i+1] <- (genres3$order[i])+1}
}


genres3 <- genres3  %>% filter(order < 5) %>%
  spread(key=order, value=genres) %>% 
  rename(genre_1="1", genre_2="2", genre_3="3", genre_4="4")



###### Cast #####

cast <- credits %>%  
  filter(nchar(cast)>2) %>%
  mutate(                              
    js = lapply(cast, fromJSON)) %>%
  unnest(js) %>%
  mutate_if(is.factor,character) %>%
  rename(actor=name, movie_cast_id=cast_id, actor_id = id, id = movie_id)

cast4 <- cast
cast4$order <- 0
cast4$order[1] <- 1

for(i in 1:(nrow(cast4)-1)) {
  if(cast4$id[i+1]!=cast4$id[i]){
    cast4$order[i+1] <- 1
  } else {cast4$order[i+1] <- (cast4$order[i])+1}
}


cast4 <- cast4  %>% filter(order < 5) %>%
  spread(key=order, value=actor) %>% 
  rename(actor_1="1", actor_2="2", actor_3="3", actor_4="4")


crew <- credits %>%  
  filter(nchar(crew)>2) %>%
  mutate(                              
    js = lapply(crew, fromJSON)) %>%
  unnest(js) %>%
  mutate_if(is.factor,character) %>%
  rename( crew_id = id , id=movie_id)


director <- crew %>% filter(job == "Director" )%>%select(id,title,name) %>% rename(director=name)
camera <- crew %>% filter(job == "Director of Photography" )%>%select(id,title,name)  %>% rename(camera=name)


###### Keyword  ####

keywords <- movies %>%    
  filter(nchar(keywords)>2) %>%       
  mutate(                             
    js = lapply(keywords, fromJSON)    
  ) %>%                               
  unnest(js) %>%                       
  select(id, title, keyword=name)   

keyword <- keywords %>% group_by(id, title) %>% count(keyword)
num_of_keywords <- keyword %>% group_by(id, title) %>% summarize(no_of_keywords_in_film = sum(n))

sum_keywords <- keywords %>% count(keyword) %>% rename(total_occurence = n)

key_words <- left_join(keyword, num_of_keywords,by = c("id", "title"))
key_words <- left_join(key_words, sum_keywords,by = c("keyword"))

#### Join Everything ######


key_words <- left_join(key_words, genres3 %>%
                         select(id,title, genre_1, genre_2, genre_3), by = c("id", "title"))
key_words <- left_join(key_words, cast4 %>%
                         select(id,title, actor_1, actor_2, actor_3,actor_4),by = c("id", "title"))
key_words <- left_join(key_words, movies %>%
                         select(id,title, weighted_rating, year),by = c("id", "title"))
key_words <- left_join(key_words, director %>%
                         select(id,title, director),by = c("id", "title"))
key_words <- left_join(key_words, camera %>%
                         select(id,title, camera),by = c("id", "title"))

key_words$key_rating <- key_words$n / key_words$total_occurence

recommend_movie <- function(movie){
  k1 = key_words[key_words$title == movie,]$keyword[1]
  k2 = key_words[key_words$title == movie,]$keyword[2]
  k3 = key_words[key_words$title == movie,]$keyword[3]
  k4 = key_words[key_words$title == movie,]$keyword[4]
  k5 = key_words[key_words$title == movie,]$keyword[5]
  a1 = key_words[key_words$title == movie,]$actor_1[1]
  a2 = key_words[key_words$title == movie,]$actor_2[1]
  a3 = key_words[key_words$title == movie,]$actor_3[1]
  a4 = key_words[key_words$title == movie,]$actor_4[1]
  g1 = key_words[key_words$title == movie,]$genre_1[1]
  g2 = key_words[key_words$title == movie,]$genre_2[1]
  g3 = key_words[key_words$title == movie,]$genre_3[1]
  c =  key_words[key_words$title == movie,]$camera[1]
  d =  key_words[key_words$title == movie,]$director[1]


  rec_df <- key_words
  
  rec_df$same_k1 <- ifelse(rec_df$keyword==k1, 1, 0)
  rec_df$same_k2 <- ifelse(rec_df$keyword==k2, 1, 0)
  rec_df$same_k3 <- ifelse(rec_df$keyword==k3, 1, 0)
  rec_df$same_k4 <- ifelse(rec_df$keyword==k4, 1, 0)
  rec_df$same_k5 <- ifelse(rec_df$keyword==k5, 1, 0)
  rec_df$same_a1 <- ifelse(rec_df$actor_1==a1 | rec_df$actor_2 == a1 | rec_df$actor_3 == a1 | rec_df$actor_4 == a1 , 1.5, 0)
  rec_df$same_a2 <- ifelse(rec_df$actor_1==a2 | rec_df$actor_2 == a2 | rec_df$actor_3 == a2 | rec_df$actor_4 == a2 , 1.5, 0)
  rec_df$same_a3 <- ifelse(rec_df$actor_1==a3 | rec_df$actor_2 == a3 | rec_df$actor_3 == a3 | rec_df$actor_4 == a3 , 1.5, 0)
  rec_df$same_a4 <- ifelse(rec_df$actor_1==a4 | rec_df$actor_2 == a4 | rec_df$actor_3 == a4 | rec_df$actor_4 == a4 , 1.5, 0)
  rec_df$same_g1 <- ifelse(rec_df$genre_1==g1 | rec_df$genre_2 == g1 | rec_df$genre_3 == g1  , 1.33, 0)
  rec_df$same_g2 <- ifelse(rec_df$genre_1==g2 | rec_df$genre_2 == g2 | rec_df$genre_3 == g2  , 1.33, 0)
  rec_df$same_g3 <- ifelse(rec_df$genre_1==g3 | rec_df$genre_2 == g3 | rec_df$genre_3 == g3  , 1.33, 0)
  rec_df$same_d <- ifelse(rec_df$director == d   , 3, 0)
  rec_df$same_c <- ifelse(rec_df$camera == c   , 2, 0)
  
  rec_df <- rec_df %>% mutate_at(vars("same_k1": "same_c"), list(~replace(., is.na(.), 0)))
  rec_df$sim_count <- rowSums(rec_df[,19:32])
  rec_df <- rec_df %>% group_by(id, title, director, year) %>% 
    summarise(sim = mean(sim_count),
              sim2 = mean(sim_count / weighted_rating),
              avg_rating = mean(weighted_rating),
              total_occurence = mean(total_occurence),
              key_rating = mean(key_rating)
    )
  Top5_rec <- rec_df %>% filter(title != movie & sim > 0) %>% arrange(desc(sim2)) 
  Top5_rec <- data.frame(Top5_rec) %>% slice(1:10)  %>% select(id, title,director,year, avg_rating, sim, sim2 )
  
  kable(Top5_rec) %>%
    kable_styling(full_width=TRUE)
}


title <- data.frame(movies$title)
sample_n(title, 10)


recommend_movie("When Harry Met Sally...")
recommend_movie("Casino")

# jahr, etc...
