setwd("C:/Users/John/OneDrive/travel list")
#included is analysis on anthems
anthem=read.csv("anthems data.csv",header = T)
library(rwhatsapp)
library(dplyr)

library(ggplot2)
library(lubridate)
library(tidytext)
View(anthem)
str(anthem)

#date of adoption of anthems
date_adopted=anthem%>%
  mutate(year=date.adopted)%>%
  count(year)
#total anthem words

anthem$text2=as.character(anthem$text)
str(anthem$text2)
total_word_anthem=anthem %>%
  select(text2) %>%
  unnest_tokens(word, text2)
dim(total_word_anthem)
View(total_word_anthem)

#anthems by year of adoption
anthem%>%
  mutate(year=date.adopted)%>%
  count(year)%>%
  top_n(20)%>%
  ggplot(aes(x=reorder(year,n),y=n))+
  geom_bar(stat="identity",fill="#4169E1")+
  ylab("")+xlab("")+
  coord_flip()+
  ggtitle("Number of Anthems by Year of Adoption")+
  theme(plot.title = element_text(hjust=0.5))


#unique words of a country say Afghanistan
o_words <- anthem %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(Continent != "Africa") %>% 
  count(word, sort = TRUE) 
#plotting words of Afghanistan
anthem %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(Continent == "Africa") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n =30, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE,fill="#4B0082") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Africa")+
  theme(plot.title = element_text(hjust=0.5))

#most commonly used words after stopwords
library("stopwords")
to_remove <- stopwords(language = "en")
#but exactly how many words does a country use in their anthem
#most words used by a country in their anthem
most_words=anthem %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(Country, word, sort = TRUE) %>%
  group_by(Country) %>%
  top_n(n = 10, n)


#plotting top words used overall
most_words2=anthem %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(word, sort = TRUE) %>%
  top_n(n = 25, n)

most_words2%>%
  ggplot(aes(x=reorder(word,n),y=n))+
  geom_col()+
  coord_flip()

View(most_words2)
#plotting top words in any country
most_words%>%
  filter(Country=="Haiti")%>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE,fill="#4B0082") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Brazil Words")+
  theme(plot.title = element_text(hjust=0.5))


#the top words by continent
most_words_continent=anthem %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(Continent, word, sort = TRUE) %>%
  group_by(Continent) %>%
  arrange(desc(Continent))%>%
  top_n(n = 25, n)

View(most_words_continent)

#now plotting
most_words_continent%>%
  ggplot(aes(x = reorder_within(Continent,n,word), y = n, fill=Continent)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~Continent, ncol = 3, scales = "free_y") +
  ggtitle("Most Words in Each Continet") 


#for fans of highcharter 
library(highcharter)
#let's plot words used in African anthems
most_words_continent%>%
  filter(Continent=="Asia")%>%
  slice(1:15) %>%
  hchart("column", hcaes(x = word, y = n)) %>%
  hc_title(text = "Top Words Used in Asian Anthems")

#plotting highest word overall using highcharter
most_words2%>%
  slice(1:20)%>%
  hchart("column", hcaes(x = word, y = n)) %>%
  hc_title(text = "Top Words Used in Anthems")

#let's plot words used in European anthems
most_words_continent%>%
  filter(Continent=="Europe")%>%
  slice(1:15) %>%
  hchart("bar", hcaes(x = word, y = n)) %>%
  hc_title(text = "Top Words Used in European Anthems")


#countries mentioning God (or any other specific word)
anthem_tidy=anthem%>%
  unnest_tokens(word,text)
god <-anthem_tidy %>% 
  filter(word=="god") 

#now plotting the number of times a country mentions god
god%>%
  mutate(time=word)%>%
  count(Country)%>%
  top_n(20)%>%
  ggplot(aes(x=reorder(Country,n),y=n))+
  geom_bar(stat="identity",fill="#4169E1")+
  ylab("")+xlab("")+
  coord_flip()+
  ggtitle("Number of Times a Nations Anthem Mentions God")+
  theme(plot.title = element_text(hjust=0.5))
