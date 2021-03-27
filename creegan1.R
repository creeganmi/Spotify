## Authors: Patrick Alvermann, Arielle Brandt, Michael Creegan, Clara Foung, and Nikolai Romanov ##
## Date: March 1, 2021 ##

library (readr)
library(dplyr)

## Load data into R with lyrics from Github ##
urllyric <- "https://raw.githubusercontent.com/creeganmi/Spotify/851b5a48c307e1a33c25df9f80750eb0192d1aa5/Spotify%20Project%20-%20Lyrics%20-%20Sheet1.csv"
df <-  read_csv(url(urllyric))
df <- rename(df,"id"= "X1")
df <- rename(df, "genre" = "top genre")
head(df)

## Check if there are any na's / inf values ##
apply(df, 2, function(x) any(is.na(x)))
indx <- apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames[indx]

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


## Visualizing Text ##

## Word Cloud ##

library(wordcloud)
wordcloudData = 
  df%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  ungroup()%>%
  select(id,word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()


library(wordcloud)
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

## Comparison Cloud ##
library(tidyr)
wordcloudData = 
  df%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=lyrics)%>%
  ungroup()%>%
  select(id,word)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)

## Document Term Matrix ##
library(tm); library(SnowballC); library(magrittr)
corpus = Corpus(VectorSource(df$lyrics))
corpus = 
  corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english')))

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(df$lyrics))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

corpus = 
  corpus %>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)

dtm = DocumentTermMatrix(corpus)
xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),dictionary = dict_corpus,type = 'prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

xdtm <- xdtm %>% select(-X)


## Topic Model ##
which(rowSums(xdtm)==0)
xdtm_topic = xdtm[which(rowSums(xdtm)!=0),]

## Create 5 Topics ##
library(topicmodels)
set.seed(617)
topic5 = LDA(x = xdtm_topic,k = 5)

## Top 10 Terms in Each Topic ##
terms(topic5,10)

length(unique(topic5@terms))
exp(topic5@beta) #term topic probabilities

## List of All Terms by Topic ##
topic5@terms

## Term - Topic Probabilities ##
df_beta = data.frame(t(exp(topic5@beta)),row.names = topic5@terms)

colnames(df_beta) = c('topic1','topic2', 'topic3','topic4','topic5')

df_beta[1:20,]

## Visualize Term Topic Probabilities ##
library(tidytext); library(dplyr); library(ggplot2); library(tidyr)
topic5 %>%
  tidy(matrix='beta')%>%
  group_by(topic)%>%
  top_n(n = 10,wt=beta)%>%
  ungroup()%>%
  ggplot(aes(x=reorder(term,beta),y=beta,fill=factor(topic)))+
  geom_bar(position='dodge', stat='identity')+
  facet_wrap(~topic, scales = 'free')+
  coord_flip()+guides(fill=F)+xlab('')

## Document Topic Probabilities ##
# topic5@gamma # document topic probabilities
df_gamma = cbind(as.integer(topic5@documents), topic5@gamma)
colnames(df_gamma) = c('id','topic1','topic2','topic3','topic4','topic5')
df_gamma[1:10,]  # Document probabilities for first 10 documents

## Visualize Topics first 20 Documents ##
library(tidytext); library(dplyr); library(ggplot2); library(tidyr)
topic5%>%
  tidy('gamma')%>%
  filter(as.integer(document)<=20)%>%
  ggplot(aes(x=reorder(document,as.numeric(document)),y=gamma,fill=factor(topic)))+
  geom_bar(position='fill',stat='identity')+xlab('id')+guides(fill=F)+coord_flip()

## Combine Topics with Original Data ##
text_topics = cbind(as.integer(topic5@documents),topic5@gamma)
colnames(text_topics) = c('id','topic1','topic2','topic3','topic4','topic5')
text_topics = merge(x = text_topics,y = df,by=c('id','id'))
head(text_topics)

## Predict Topic Model ##
set.seed(617)
split = sample(1:nrow(text_topics),size = 0.7*nrow(text_topics))
train = text_topics[split,]
test = text_topics[-split,]

library(rpart)
topicmodel = rpart(pop~topic1+topic2+topic3+topic4+topic5,train)
predtopic = predict(topicmodel,newdata = test)
sqrt(mean((predtopic-test$pop)^2))

## Latent Semantic Analysis ##
library(lsa)
clusters = lsa(xdtm)
# lsa decomposes data into three matrices. The term matrix contains the dimensions from svd
clusters$tk = as.data.frame(clusters$tk)
colnames(clusters$tk) = paste0("dim",1:57)
head(clusters$tk)

clusters_data = cbind(id = df$id, pop = df$pop, clusters$tk)

set.seed(617)
split = sample(1:nrow(clusters_data),size = 0.7*nrow(clusters_data))
train = clusters_data[split,]
test = clusters_data[-split,]

lsamodel = rpart(pop~.-id,train)
predlsa = predict(lsamodel,newdata = test)
sqrt(mean((predlsa-test$pop)^2))

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
  group_by(genre = genre) %>%
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

library(kableExtra)

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

#recommender#
library(shiny)
library(shinythemes)
library(plotly)

names(df)
df$artist

ui <- shinyUI(navbarPage(theme = shinytheme("slate"),"Let's recommend a song for you!",
                         tabPanel("Genre",
                                  sidebarPanel(
                                    # Genre Selection
                                    
                                    selectInput(inputId = "Columns", label = "Select Genre",
                                                unique(df$grouped_genre), multiple = FALSE),
                                    verbatimTextOutput("pop"),
                                    
                                    sliderInput(inputId = "range", label = "Popularity or Mood?",
                                                min = min(df$pop),max = 100,value = c(55,100))
                                  ),
                                  mainPanel(
                                    h2("Top songs of the genre"),
                                    DT::dataTableOutput(outputId = "songsreco")
                                  )
                         ),
                         tabPanel("Artist",
                                  sidebarPanel(selectInput(inputId = "artist", label = "Which singer do you like?",
                                                           unique(df$title), multiple = FALSE),
                                               verbatimTextOutput("Taylor Swift"),
                                               
                                               sliderInput(inputId = "range_2", label = "Popularity or Mood?",
                                                           min = min(df$pop),max = 100,value = c(55,100))),
                                  mainPanel(
                                    h2("Top songs of the artist"),
                                    DT::dataTableOutput(outputId = "songsreco_artist")))))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

  
  
  
  
  
