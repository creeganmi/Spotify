## Spotify Project Code
## Authors: Patrick Alvermann, Arielle Brandt, Michael Creegan, Clara Foung, Nikolai Romanov
## Date: April 5, 2021

#### Initial Data Cleaning and Exploration (Proposal) ####

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

## Initial hypothesis: Can you predict song popularity based on sentiment? ##
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


#### Further Data Cleaning and Exploration ####

## Using linear models to explore correlations 
summary(df)


##Visualizations and correlations between popularity and individual variables using all data 

##pop~year
ggplot(data=df, aes(x=year, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_year = cor(df$pop, df$year); cor_year

#pop~bpm
ggplot(data=df, aes(x=bpm, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_bpm = cor(df$pop, df$bpm); cor_bpm

#pop~nrgy
ggplot(data=df, aes(x=nrgy, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_nrgy = cor(df$pop, df$nrgy); cor_nrgy

#pop~dnce
ggplot(data=df, aes(x=dnce, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_dnce = cor(df$pop, df$dnce); cor_dnce

#pop~dB
ggplot(data=df, aes(x=dB, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_dB = cor(df$pop, df$dB); cor_dB
#NOTE: impact of dB = 0?

#pop~live
ggplot(data=df, aes(x=live, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_live = cor(df$pop, df$live); cor_live

#pop~val
ggplot(data=df, aes(x=val, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_val = cor(df$pop, df$val); cor_val

#pop~dur
ggplot(data=df, aes(x=dur, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_dur = cor(df$pop, df$dur); cor_dur

#pop~acous
ggplot(data=df, aes(x=acous, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_acous = cor(df$pop, df$acous); cor_acous

#pop~spch
ggplot(data=df, aes(x=spch, y=pop))+geom_point()+geom_smooth(method=lm, size=1.3, color="blue")
cor_spch = cor(df$pop, df$spch); cor_spch

##Table of correlations 
df_pop_cor = c(cor_year, cor_bpm, cor_nrgy, cor_dnce, cor_dB, cor_live, cor_val, cor_dur, cor_acous, cor_spch)
variable = c("year", "bpm", "nrgy", "dnce", "dB", "live", "val", "dur", "acous", "spch")
table_df_pop_correlations = data.frame(variable, df_pop_cor)
table_df_pop_correlations


##Linear models on train and test data

#split into train and test
df = as.vector(df)
library(caret)
set.seed(2010)
split = createDataPartition(y = df$pop, p = 0.7, list = F, groups = 50)
train = df[split,]
test = df[-split,]

nrow(train)
nrow(test)
mean(train$pop)
mean(test$pop)

#install.packages("broom")  #(p-value extraction, reference: AntoniosK, https://stackoverflow.com/questions/31570440/extract-regression-p-value-in-r, Aug 6, 2015)
library(broom)

model1 = lm(pop~year, data=train)
pred1 = predict(model1)
rmse1 = sqrt(mean((pred1-train$pop)^2)); rmse1
pred_test1 = predict(model1, newdata=test)
rmse_test1 = sqrt(mean((pred_test1 - test$pop)^2)); rmse_test1
summary(model1)
p_1 = glance(model1)$p.value; p_1
cor1_train = cor(train$pop, train$year); cor1_train


model2 = lm(pop~bpm, data=train)
pred2 = predict(model2)
rmse2 = sqrt(mean((pred2-train$pop)^2)); rmse2
pred_test2 = predict(model2, newdata=test)
rmse_test2 = sqrt(mean((pred_test2 - test$pop)^2)); rmse_test2
summary(model2)
p_2 = glance(model2)$p.value; p_2
cor2_train = cor(train$pop, train$bpm); cor2_train


model3 = lm(pop~nrgy, data=train)
pred3 = predict(model3)
rmse3 = sqrt(mean((pred3-train$pop)^2)); rmse3
pred_test3 = predict(model3, newdata=test)
rmse_test3 = sqrt(mean((pred_test3 - test$pop)^2)); rmse_test3
summary(model3)
p_3 = glance(model3)$p.value; p_3
cor3_train = cor(train$pop, train$nrgy); cor3_train


model4 = lm(pop~dnce, data=train)
pred4 = predict(model4)
rmse4 = sqrt(mean((pred4-train$pop)^2)); rmse4
pred_test4 = predict(model4, newdata=test)
rmse_test4 = sqrt(mean((pred_test4 - test$pop)^2)); rmse_test4
summary(model4)
p_4 = glance(model4)$p.value; p_4
cor4_train = cor(train$pop, train$dnce); cor4_train


model5 = lm(pop~dB, data=train)
pred5 = predict(model5)
rmse5 = sqrt(mean((pred5-train$pop)^2)); rmse5
pred_test5 = predict(model5, newdata=test)
rmse_test5 = sqrt(mean((pred_test5 - test$pop)^2)); rmse_test5
summary(model5)
p_5 = glance(model5)$p.value; p_5
cor5_train = cor(train$pop, train$dB); cor5_train


model6 = lm(pop~live, data=train)
pred6 = predict(model6)
rmse6 = sqrt(mean((pred6-train$pop)^2)); rmse6
pred_test6 = predict(model6, newdata=test)
rmse_test6 = sqrt(mean((pred_test6 - test$pop)^2)); rmse_test6
summary(model6)
p_6 = glance(model6)$p.value; p_6
cor6_train = cor(train$pop, train$live); cor6_train


model7 = lm(pop~val, data=train)
pred7 = predict(model7)
rmse7 = sqrt(mean((pred7-train$pop)^2)); rmse7
pred_test7 = predict(model7, newdata=test)
rmse_test7 = sqrt(mean((pred_test7 - test$pop)^2)); rmse_test7
summary(model7)
p_7 = glance(model7)$p.value; p_7
cor7_train = cor(train$pop, train$val); cor7_train


model8 = lm(pop~dur, data=train)
pred8 = predict(model8)
rmse8 = sqrt(mean((pred8-train$pop)^2)); rmse8
pred_test8 = predict(model8, newdata=test)
rmse_test8 = sqrt(mean((pred_test8 - test$pop)^2)); rmse_test8
summary(model8)
p_8 = glance(model8)$p.value; p_8
cor8_train = cor(train$pop, train$dur); cor8_train


model9 = lm(pop~acous, data=train)
pred9 = predict(model9)
rmse9 = sqrt(mean((pred9-train$pop)^2)); rmse9
pred_test9 = predict(model9, newdata=test)
rmse_test9 = sqrt(mean((pred_test9 - test$pop)^2)); rmse_test9
summary(model9)
p_9 = glance(model9)$p.value; p_9
cor9_train = cor(train$pop, train$acous); cor9_train


model10 = lm(pop~spch, data=train)
pred10 = predict(model10)
rmse10 = sqrt(mean((pred10-train$pop)^2)); rmse10
pred_test10 = predict(model10, newdata=test)
rmse_test10 = sqrt(mean((pred_test10 - test$pop)^2)); rmse_test10
summary(model10)
p_10 = glance(model10)$p.value; p_10
cor10_train = cor(train$pop, train$spch); cor10_train


##Comparing single variable models

RMSE_train_onevar = c(rmse1, rmse2, rmse3, rmse4, rmse5, rmse6, rmse7, rmse8, rmse9, rmse10)
RMSE_test_onevar = c(rmse_test1, rmse_test2, rmse_test3, rmse_test4, rmse_test5, rmse_test6, rmse_test7, rmse_test8, rmse_test9, rmse_test10)
p_train_onevar_full = c(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10)
p_train_onevar = round(p_train_onevar_full, digits = 8)
cor_train = c(cor1_train, cor2_train, cor3_train, cor4_train, cor5_train, cor6_train, cor7_train, cor8_train, cor9_train, cor10_train)
table_onevar_RMSE_p = data.frame(variable, df_pop_cor, cor_train, p_train_onevar, RMSE_train_onevar, RMSE_test_onevar)
table_onevar_RMSE_p = rename(table_onevar_RMSE_p, cor_alldata=df_pop_cor, p_model_train=p_train_onevar, RMSE_train=RMSE_train_onevar, RMSE_test=RMSE_test_onevar)
table_onevar_RMSE_p

## model with all features
variable
model_all_var = lm(pop~year+bpm+nrgy+dnce+dB+live+val+dur+acous+spch, data=train)
pred_all_var = predict(model_all_var)
summary(model_all_var) 
rmse_all_var = sqrt(mean((pred_all_var-train$pop)^2)); rmse_all_var
pred_test_all_var = predict(model_all_var, newdata=test)
rmse_test_all_var = sqrt(mean((pred_test_all_var-test$pop)^2)); rmse_test_all_var
p_all_var = glance(model_all_var)$p.value; p_all_var



## model with all features and interactions
model_all_var_int = lm(pop~year+bpm+nrgy+dnce+dB+live+val+dur+acous+spch+((year+bpm+nrgy+dnce+dB+live+val+dur+acous+spch)^2), data=train)
pred_all_var_int = predict(model_all_var_int)
summary(model_all_var_int) 
rmse_all_var_int = sqrt(mean((pred_all_var_int-train$pop)^2)); rmse_all_var_int
pred_test_all_var_int = predict(model_all_var_int, newdata=test)
rmse_test_all_var_int = sqrt(mean((pred_test_all_var_int-test$pop)^2)); rmse_test_all_var_int
p_all_var_int = glance(model_all_var_int)$p.value; p_all_var_int


## stepwise feature selection with all features and interactions

empty_mod = lm(pop~1,data=train)
full_mod = lm(pop~year+bpm+nrgy+dnce+dB+live+val+dur+acous+spch+((year+bpm+nrgy+dnce+dB+live+val+dur+acous+spch)^2),data=train)
forwardStepwise = step(empty_mod, scope=list(upper=full_mod,lower=empty_mod), direction='forward')
summary(forwardStepwise)

backwardStepwise = step(full_mod, scope=list(upper=full_mod,lower=empty_mod), direction='backward')
summary(backwardStepwise)

hybridStepwise = step(empty_mod, scope=list(upper=full_mod,lower=empty_mod), direction='both')
summary(hybridStepwise)



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
  theme_wsj()+
  labs(title = "Sentiment of all lyrics using afinn lexicon")


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
  theme_wsj()+
  labs(title = "Sentiment of all lyrics using Jockers lexicon")

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
  theme_wsj()+
  labs(title = "Sentiment of all lyrics using Senticnet lexicon")

## Profanity Aware Lexicon ##
df %>%
  group_by(id)%>%
  unnest_tokens(output = word, input = lyrics)%>%
  ungroup()%>%
  select(id, word)%>%
  anti_join(data.frame(word = c(profanity_alvarez)), 
            by = c('word'='word'))


## Visualizing Text and Topics ##

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
train2 = clusters_data[split,]
test2 = clusters_data[-split,]

lsamodel = rpart(pop~.-id,train2)
predlsa = predict(lsamodel,newdata = test2)
sqrt(mean((predlsa-test2$pop)^2))

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
year_plot


#### Final Product ####

##Creating a grouped genre column##
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

##Genre correlation matrix

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

##Creating a song sentiment score column#

afinn <- get_sentiments("afinn")
glimpse(afinn)

nrc <- get_sentiments("nrc") #Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.
glimpse(nrc)

bing <- get_sentiments("bing")
glimpse(bing)

loughran <- get_sentiments("loughran") #Loughran-McDonald Sentiment lexicon. URL: https://sraf.nd.edu/textual-analysis/resources/ 
glimpse(loughran)

#want to have sentiment on a numerical scale so we used afinn

sentiment <- df %>%
  unnest_tokens(word, lyrics, drop=FALSE) %>%
  inner_join(afinn) %>%
  group_by(id, title, artist, genre, year, bpm, nrgy, dnce, dB, live, val, dur,
           acous, spch, pop, lyrics, grouped_genre) %>% # or add more columns to the group_by to retain all info
  summarise(score = sum(value))

dim(sentiment)

print(head(as.data.frame(sentiment)))

sentiment

View(sentiment)

##difference in records between df and sentiment##
library(arsenal)
summary(comparedf(df, sentiment))
summary(comparedf(df, sentiment, by = "id"))

dfSentiment <- left_join(sentiment, df)


## Remove duplicate songs##
dfSentiment <- dfSentiment[!duplicated(dfSentiment[,c('title','artist')]),] 

## UI code ##

ui <- shinyUI(navbarPage(theme = shinytheme("slate"),"Let's recommend a song for you!",
                         tabPanel("Popularity",
                                  sidebarPanel(
                                    # Genre Selection
                                    
                                    selectInput(inputId = "Columns", label = "Select Genre",
                                                unique(dfSentiment$grouped_genre), multiple = FALSE),
                                    verbatimTextOutput("pop"),
                                    
                                    sliderInput(inputId = "range", label = "Popularity",
                                                min = min(dfSentiment$pop),max = 100,value = c(55,100))
                                  ),
                                  mainPanel(
                                    h2("Top songs of the genre"),
                                    DT::dataTableOutput(outputId = "songsreco")
                                  )
                         ),
                         tabPanel("Sentiment",
                                  sidebarPanel(selectInput(inputId = "Columns2", label = "Select Genre",
                                                           unique(dfSentiment$grouped_genre), multiple = FALSE),
                                               verbatimTextOutput("sentiment"),
                                               
                                               sliderInput(inputId = "range_2", label = "Sentiment",
                                                           min = min(dfSentiment$score),max = max(dfSentiment$score),value = c(55,303))),
                                  mainPanel(
                                    h2("Top songs of the genre"),
                                    DT::dataTableOutput(outputId = "songsreco2")))))

#server <- function(input, output) {}

##recommendation engine server logic##

server <- function(input, output) {
  
  datasetInput <- reactive({
    
    # Filtering based on genre and rating
    dfSentiment %>% filter(grouped_genre %in% as.vector(input$Columns)) %>%
      group_by(title) %>% filter(pop >= as.numeric(input$range[1]), pop <= as.numeric(input$range[2])) %>%
      arrange(desc(pop)) %>%
      select(title, artist, pop, grouped_genre) %>%
      rename(`title` = title, `Genre(s)` = grouped_genre)
    
    
  })
  
  datasetInput2 <- reactive({
    
    # Filtering based on genre and sentiment
    dfSentiment %>% filter(grouped_genre %in% as.vector(input$Columns2)) %>%
      group_by(title) %>% filter(score >= as.numeric(input$range_2[1]), score <= as.numeric(input$range_2[2])) %>%
      arrange(desc(score)) %>%
      select(title, artist, score, grouped_genre) %>%
      rename(`title` = title, `Genre(s)` = grouped_genre)
    
    
  })
  
  
  #Rendering the table
  output$songsreco <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput(), n = 50), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  output$songsreco2 <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput2(), n = 50), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  
  output$songsreco_artist <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput(), n = 100), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  output$songsreco_artist2 <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput2(), n = 100), escape = FALSE, options = list(scrollX = '1000px'))
  })
}


shinyApp(ui = ui, server = server)
