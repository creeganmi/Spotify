## Author: Michael Creegan ##
## Date: March 29, 2021 ##

library(readr)
library(dplyr)
library(kableExtra)
library(stringr)
library(ggplot2)
library(ggthemes)
library(textdata)
library(tidytext)
library(lexicon)
library(wordcloud)
library(tm)
library(SnowballC) 
library(magrittr)
library(tidyr)
library(topicmodels)
library(rpart)
library(lsa)
library(shiny)
library(shinythemes)
library(plotly)


## Load data into R with lyrics from Github ##
urllyric <- "https://raw.githubusercontent.com/creeganmi/Spotify/851b5a48c307e1a33c25df9f80750eb0192d1aa5/Spotify%20Project%20-%20Lyrics%20-%20Sheet1.csv"
df <-  read_csv(url(urllyric))
df <- rename(df,"id"= "X1")
df <- rename(df, "genre" = "top genre")
head(df)

## Check if there are any na's / inf values ##
apply(df, 2, function(x) any(is.na(x)))
indx <- apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
isTRUE(indx)

## Summary Statistics ##

## Characters ##
summary(nchar(df$lyrics))

## Words ##
library(stringr)
summary(str_count(string = df$lyrics,pattern = '\\S+'))

## Song with Shortest Lyrics ##
shortest_lyrics_index = which.min(str_count(string = df$lyrics,pattern = '\\S+'))
df$title[shortest_lyrics_index]

## Song with Longest Lyrics ##
longest_lyrics_index = which.max(str_count(string = df$lyrics,pattern = '\\S+'))
df$title[longest_lyrics_index]

## Song Popularity Visualization ##

library(ggplot2); library(ggthemes)
ggplot(data=df,aes(x=pop))+
  geom_histogram(fill='chartreuse2')+
  theme_bw()+
  xlab('Song Popularity')+
  labs(title="Histogram of Song Popularity")

## Potential Hypothesis: Can you predict song popularity based on sentiment? ##
library(dplyr)
library(tidytext)

## Break text into individual tokens ##
text.df <- tibble(df, text = df$lyrics)

text.df %>%
  unnest_tokens(word, text) 

## Create count of most popular words for popular and unpopular (relative) songs ##
## Determine mean popularity score ##
mean(df$pop)

## Tokenize, remove stop words, and subset by popularity ##

pop.text <- text.df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  filter(pop > mean(pop)) %>%
  count(word, sort = TRUE)


unpop.text <- text.df %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  filter(pop < mean(pop)) %>%
  count(word, sort = TRUE)


## Create afinn sentiment and subset of data for most top n words by popularity ##
## Add afinn lexicon from tidytext ##
library(textdata)
afinn = get_sentiments('afinn')
pop.sentiment <- pop.text %>%
  inner_join(get_sentiments("afinn"))

unpop.sentiment <- unpop.text %>%
  inner_join(get_sentiments("afinn"))

## Create subset of top 20 most popular words ##
top.sentiment <- pop.sentiment %>%
  head(20)

top.sentiment2 <- unpop.sentiment %>%
  head(20)

## Compare differences between songs above and below mean popularity for top 20 words ##
top.sentiment %>%
  inner_join(top.sentiment2, by = 'word')

top.sentiment %>%
  anti_join(top.sentiment2, by = 'word')

top.sentiment2 %>%
  anti_join(top.sentiment, by = 'word')

## Create column using red and green to show if sentiment is below or above 0 ##
top.sentiment$color <- ifelse(top.sentiment$value < 0, "red","green")
top.sentiment2$color <- ifelse(top.sentiment2$value < 0, "red","green")

head(top.sentiment)
head(top.sentiment2)

## Visualize each sentiment based off color column! ##
library(ggplot2)

top.sentiment.viz <- ggplot(top.sentiment, aes(reorder(word, -n), value, color=color)) + geom_col(fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Spotify Songs Above Mean Popularity", x="Top 20 Most Popular Words", y="Sentiment Score") +
  theme(plot.title = element_text(size=15,hjust = 0.5)) +
  scale_color_identity()

top.sentiment.viz

top.sentiment.viz2 <- ggplot(top.sentiment2, aes(reorder(word, -n), value, color=color)) + geom_col(fill="white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Spotify Songs Below Mean Popularity", x="Top 20 Most Popular Words", y="Sentiment Score") +
  theme(plot.title = element_text(size=15,hjust = 0.5)) +
  scale_color_identity()

top.sentiment.viz2

## Afinn Sentiment Lexicon on All Lyrics ##
df %>%
  select(id,lyrics)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  inner_join(afinn)%>%
  summarize(lyricSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(lyricSentiment),
            max=max(lyricSentiment),
            median=median(lyricSentiment),
            mean=mean(lyricSentiment))

## visualize afinn lexicon on all lyrics ##

df %>%
  select(id,lyrics)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  inner_join(afinn)%>%
  summarize(lyricSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=lyricSentiment,fill=lyricSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

## Jockers Sentiment Lexicon ##
library(lexicon)
head(key_sentiment_jockers)
df %>%
  select(id,lyrics)%>%
  group_by(id)%>%
  unnest_tokens(word, lyrics)%>%
  inner_join(key_sentiment_jockers)%>%
  summarize(lyrics = mean(value))%>%
  ungroup()%>%
  summarize(min=min(lyrics),max=max(lyrics),median=median(lyrics),mean=mean(lyrics))

## Visualize Jockers Sentiment Scores ##
df %>%
  select(id,lyrics)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  inner_join(key_sentiment_jockers)%>%
  summarize(lyrics = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=lyrics,fill=lyrics>0))+
  geom_histogram(binwidth = 0.02)+
  scale_x_continuous(breaks=seq(-1,1,0.2))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

## Senticnet Sentiment Lexicon ##
library(lexicon)
head(hash_sentiment_senticnet)

df %>%
  select(id,lyrics)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  inner_join(hash_sentiment_senticnet, by = c('word'='x'))%>%
  summarize(lyricSentiment = mean(y))%>%
  ungroup()%>%
  summarize(min=min(lyricSentiment),max=max(lyricSentiment),median=median(lyricSentiment),mean=mean(lyricSentiment))

## Visualize Senticnet Sentiment Scores ##
df %>%
  select(id,lyrics)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  inner_join(hash_sentiment_senticnet, by = c('word'='x'))%>%
  summarize(lyricSentiment = mean(y))%>%
  ungroup()%>%
  ggplot(aes(x=lyricSentiment,fill=lyricSentiment>0))+
  geom_histogram(binwidth = 0.01)+
  scale_x_continuous(breaks=seq(-1,1,0.2))+
  scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

## Profanity Aware Lexicon ##
df %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = lyrics)%>%
  ungroup()%>%
  select(id, word)%>%
  anti_join(data.frame(word = c(profanity_alvarez)), 
            by = c('word'='word'))


##sentiment column##

#df2 <- df %>% 
#  mutate( title = gsub("'", "", title), #remove '
#          title = gsub("\\s*\\([^\\)]+\\)\\s*$", "", title), #remove information between parenthesis
#lyricTBL <- df %>%
#  select(title, artist, lyrics)
#topSongTBL <- df


##sentiment function that didn't work correctly##

#Sentiment <- sapply(
#  X = 1:nrow(df)
#  , FUN = function(row_num, topSongTBL){
#    
#    sentiment <- df %>%
#        unnest_tokens(word, lyrics) %>%
#        select(word) %>%
#        inner_join(lexicon) %>%
#        summarise(score = sum(value))
#      
#      sentiment <- sentiment[[1]]
#      
#      error = function(e){
#      print(paste0("Failed for song name: ", topSongTBL[["title"]][row_num]))
#    }
#    return(sentiment)
#  }
#  , topSongTBL = df
#)

#print(head(as.data.frame(Sentiment)))

#Sentiment

##genre analysis##
unique(df$genre)

names(df)

##spread of songs across genre##
df%>%
  group_by(genre = genre) %>%
  summarise(no_of_tracks = n()) %>%
  arrange(desc(no_of_tracks)) %>%
  knitr::kable()

##who are the artists with the most releases##
most_releases <- df %>%
  group_by(artist = artist) %>%
  summarize(no_of_tracks = n()) %>%
  arrange(desc(no_of_tracks)) %>%
  top_n(15, wt = no_of_tracks) %>%
  ggplot(aes(x=artist, y=no_of_tracks)) +
  geom_bar(stat = "identity") +
  coord_flip() + labs(title = "Artists with the Most Releases",x="Artists",y="No of Releases")

most_releases

##popular words in titles##
text <- df$title

docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords,c("feat","edit","remix","remastered",
                                   "remaster","radio","version","original","mix"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#no word has been more associated with music than love in track titles##

wordcloud(words = df$word, freq = df$freq,scale=c(8,0.25), min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

##is there a golden age of music ?##
df <-  read_csv(url(urllyric))
df <- rename(df,"id"= "X1")
df <- rename(df, "genre" = "top genre")

plot_year <- df %>%
  select(year) %>%
  group_by(year) %>%
  summarize(count = n())

year_plot <- ggplot(plot_year,aes(x = year, y = count,group = 1)) + 
  geom_line() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Release of songs across years", x = "Year", 
       y = "No of songs released")

##what characteristics describe a genre?##
names(df)
unique(df$genre)

df <- df %>%
  mutate(grouped_genre = case_when(genre %in% c("canadian pop","australian pop","hip pop"
                                                ,"indie pop","boy band","folk-pop","dance pop"
                                                ,"barbadian pop","art pop","baroque pop"
                                                ,"metropopolis","danish pop","pop","colombian pop"
                                                ,"acoustic pop","candy pop","moroccan pop") ~ "pop",
                                   genre %in% c("big room","electro","downtempo","escape room" 
                                                ,"complextro", "electropop","electronic trap"
                                                ,"brostep","australian dance","tropical house"
                                                ,"edm","house","belgian edm","electrohouse", "electro house") ~ "edm",
                                   genre %in% c("australian hip hop","detroit hip hop","hip hop"
                                                ,"canadian hip hop","atl hip hop") ~ "hiphop",
                                   genre %in% c("canadian latin","latin") ~ "latin",
                                   genre %in% c("permanent wave","alaska indie","french indie pop")  ~ "alternative",
                                   genre %in% c("neo mellow","canadian contemporary r&b"
                                                ,"british soul","irish singer-songwriter"
                                                ,"alternative r&b") ~ "rnb",
                                   genre %in% c("chicago rap","hollywood")  ~ "rap",
                                   genre %in% c("contemporary country","celtic rock") ~ "rock",
                                   TRUE ~ "other"))


pop
View(df)
unique(df$grouped_genre)

df %>%
  count(grouped_genre) %>%
  arrange(desc(n()))


genre_description <- df %>% 
  group_by(grouped_genre = grouped_genre) %>%
  summarise(Danceability = mean(dnce),
            Energy = mean(nrgy),
            Loudness = mean(dB),
            Speechiness = mean(spch),
            Acousticness = mean(acous),
            Liveness = mean(live),
            Valence = mean(val),
            Tempo = mean(bpm),
            Duration = mean(dur),
            Popularity = mean(pop))

kable(genre_description , format = "html") %>%
  kable_styling(bootstrap_options = "striped") %>%
  column_spec(2, width = "12em")

##how closely related are the genres##
names <- names(df)[c(6:15)]

avg_genre_matrix <- df %>%
  group_by(grouped_genre) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup() 

avg_genre_cor <- avg_genre_matrix %>%
  select(names, ) %>% 
  scale() %>%
  t() %>%
  as.matrix() %>%
  cor() 

colnames(avg_genre_cor) <- avg_genre_matrix$grouped_genre
row.names(avg_genre_cor) <- avg_genre_matrix$grouped_genre


##need to get the grouped_genre case statement to work to show this correctly##
avg_genre_cor %>% 
  corrplot::corrplot(method = 'color', 
                     order = 'hclust',
                     type = 'upper',
                     tl.col = 'black',
                     diag = FALSE,
                     addCoef.col = "black",
                     number.cex = 0.75,
                     mar = c(2,2,2,2),
                     main = 'Correlation Between Mean Genre Feature Values',
                     family = 'Avenir')
##sentiment column#

afinn <- get_sentiments("afinn")
glimpse(afinn)

nrc <- get_sentiments("nrc")
glimpse(nrc)

bing <- get_sentiments("bing")
glimpse(bing)

loughran <- get_sentiments("loughran")
glimpse(loughran)

#want to have sentiment on a numerical scale so we use afinn

sentiment <- df %>%
  unnest_tokens(word, lyrics, drop=FALSE) %>%
  inner_join(afinn) %>%
  group_by(id, title, artist, genre, year, bpm, nrgy, dnce, dB, live, val, dur,
           acous, spch, pop, lyrics, grouped_genre) %>% # or add more columns to the group_by to retain all info
  summarise(afinn = sum(value))

dim(sentiment)

names(sentiment)
print(head(as.data.frame(sentiment)))

sentiment

View(sentiment)

genre_description <- sentiment %>% 
  group_by(grouped_genre = grouped_genre) %>%
  summarise(Danceability = mean(dnce),
            Energy = mean(nrgy),
            Loudness = mean(dB),
            Speechiness = mean(spch),
            Acousticness = mean(acous),
            Liveness = mean(live),
            Valence = mean(val),
            Tempo = mean(bpm),
            Duration = mean(dur),
            Popularity = mean(pop),
            Sentiment = mean(afinn))

kable(genre_description , format = "html") %>%
  kable_styling(bootstrap_options = "striped") %>%
  column_spec(2, width = "12em")


names <- names(df)[c(6:15)]

avg_genre_matrix <- sentiment %>%
  group_by(grouped_genre) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup() 

avg_genre_cor <- avg_genre_matrix %>%
  select(names, ) %>% 
  scale() %>%
  t() %>%
  as.matrix() %>%
  cor() 

colnames(avg_genre_cor) <- avg_genre_matrix$grouped_genre
row.names(avg_genre_cor) <- avg_genre_matrix$grouped_genre


avg_genre_cor %>% 
  corrplot::corrplot(method = 'color', 
                     order = 'hclust',
                     type = 'upper',
                     tl.col = 'black',
                     diag = FALSE,
                     addCoef.col = "black",
                     number.cex = 0.75,
                     mar = c(2,2,2,2),
                     main = 'Correlation Between Mean Genre Feature Values',
                     family = 'Avenir')

#recommendation engine UI#
##difference in records between df and sentiment##
library(arsenal)
summary(comparedf(df, sentiment))
summary(comparedf(df, sentiment, by = "id"))

dfSentiment <- left_join(sentiment, df)

##go to shiny.R file to see my code for the ui & server for the recommendation engine :)##
