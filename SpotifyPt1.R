## Author: Michael Creegan ##
## Date: February 25, 2021 ##

library (readr)

##load data into R with lyrics from Github##
urllyric <- "https://raw.githubusercontent.com/creeganmi/Spotify/851b5a48c307e1a33c25df9f80750eb0192d1aa5/Spotify%20Project%20-%20Lyrics%20-%20Sheet1.csv"
df <-  read_csv(url(urllyric))
head(df)

##check if there are any na's / inf values ##
apply(df, 2, function(x) any(is.na(x)))
indx <- apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
<<<<<<< HEAD
isTRUE(indx)

## Summary Statistics

## Characters
summary(nchar(df$lyrics))

## Words
library(stringr)
summary(str_count(string = df$lyrics,pattern = '\\S+'))

## Song with Shortest Lyrics
shortest_lyrics_index = which.min(str_count(string = df$lyrics,pattern = '\\S+'))
df$title[shortest_lyrics_index]

## Song with Longest Lyrics
longest_lyrics_index = which.max(str_count(string = df$lyrics,pattern = '\\S+'))
df$title[longest_lyrics_index]

=======
colnames[indx]
>>>>>>> 098fce5b05d15ddc778176ccb09b6169a249283f

## potential hypothesis: can you predict song popularity based on sentiment? ##

library(dplyr)
library(tidytext)
library(stringr)

## break text into individual tokens ToKeNiZaTi0N OwO ##
text.df <- tibble(df, text = df$lyrics)


text.df %>%
  unnest_tokens(word, text) 

## create count of most popular words for popular and unpopular (relative) songs ##
mean(df$pop)

pop.text <- text.df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  filter(pop > mean(pop)) %>%
  count(word, sort = TRUE)

<<<<<<< HEAD
unpop.text <- text.df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  filter(pop < mean(pop)) %>%
  count(word, sort = TRUE)

=======
>>>>>>> 098fce5b05d15ddc778176ccb09b6169a249283f
## create afinn sentiment and subset of data for most top n words by popularity ##
pop.sentiment <- pop.text %>%
  inner_join(get_sentiments("afinn"))

<<<<<<< HEAD
unpop.sentiment <- unpop.text %>%
  inner_join(get_sentiments("afinn"))

=======
>>>>>>> 098fce5b05d15ddc778176ccb09b6169a249283f
## create subset of top 20 most popular words ##
top.sentiment <- pop.sentiment %>%
  head(20)

<<<<<<< HEAD
top.sentiment2 <- unpop.sentiment %>%
  head(20)

## compare differences between songs above and below mean popularity for top 20 words ##
top.sentiment %>%
  inner_join(top.sentiment2, by = 'word')

top.sentiment %>%
  anti_join(top.sentiment2, by = 'word')

## create column using red and green to show if sentiment is below or above 0 ##
top.sentiment$color <- ifelse(top.sentiment$value < 0, "red","green")
top.sentiment2$color <- ifelse(top.sentiment2$value < 0, "red","green")

head(top.sentiment)
head(top.sentiment2)
=======
## create column using red and green to show if sentiment is below or above 0 ##
top.sentiment$color <- ifelse(top.sentiment$value < 0, "red","green")

head(top.sentiment)
>>>>>>> 098fce5b05d15ddc778176ccb09b6169a249283f

## visualize each sentiment based off color column! ##
library(ggplot2)

top.sentiment.viz <- ggplot(top.sentiment, aes(reorder(word, -n), value, color=color)) + geom_col(fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
<<<<<<< HEAD
  labs(title="Data: Spotify Songs Above Mean Popularity", x="Top 20 Most Popular Words", y="Sentiment Score") +
=======
  labs(title="Spotify", x="Top 20 Most Popular Words", y="Sentiment Score") +
>>>>>>> 098fce5b05d15ddc778176ccb09b6169a249283f
  theme(plot.title = element_text(size=15,hjust = 0.5)) +
  scale_color_identity()

top.sentiment.viz
<<<<<<< HEAD

top.sentiment.viz2 <- ggplot(top.sentiment2, aes(reorder(word, -n), value, color=color)) + geom_col(fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Data: Spotify Songs Below Mean Popularity", x="Top 20 Most Popular Words", y="Sentiment Score") +
  theme(plot.title = element_text(size=15,hjust = 0.5)) +
  scale_color_identity()
  
top.sentiment.viz2
=======
  
>>>>>>> 098fce5b05d15ddc778176ccb09b6169a249283f
