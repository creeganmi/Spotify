##Spotify Project - Linear models code
## Authors: Patrick Alvermann, Arielle Brandt, Michael Creegan, Clara Foung, and Nikolai Romanov ##


library (readr)
library(dplyr)

## Load data into R with lyrics from Github ##
urllyric <- "https://raw.githubusercontent.com/creeganmi/Spotify/851b5a48c307e1a33c25df9f80750eb0192d1aa5/Spotify%20Project%20-%20Lyrics%20-%20Sheet1.csv"
df <- read_csv(url(urllyric))
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


#### Linear models/exploration ####
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




