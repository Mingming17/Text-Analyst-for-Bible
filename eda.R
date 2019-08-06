library(tidytext)
library(tidyverse)
library('dplyr')
library(ggplot2)
library(scales)


bible <- read_csv("bible.csv")

bibleTidy <- bible %>%
  select(book,chapter,text) %>%
  unnest_tokens(word,text)

bibleFreqWords <- bibleTidy %>%
  count(word) 

test <- bibleFreqWords %>%
  anti_join(stop_words) %>%
  top_n(25)%>%
  arrange(desc(n))


bibleFreqWords %>%
  anti_join(stop_words) %>%
  top_n(25) %>%
  ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Words',
       subtitle = 'Stop Words Removed',
       x='Count',
       y= 'Word')

list <- c("1","2")

bibleFreqWordsBybook <- bibleTidy %>%
  count(book,word)%>%
  anti_join(stop_words)%>%
  group_by(book)%>%
  arrange(desc(n), .by_group = TRUE)%>%
  top_n(10)%>%
  filter(book %in% list)



bibleFreqWordsBybook %>%
  ggplot(aes(x=fct_reorder(word,n),
             y=n,
             fill=book)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  #scale_x_reordered() +
  facet_wrap(~book,scales = 'free',nrow=1) + 
  theme_bw() + 
  theme(legend.position = "none")+
  labs(title = 'Top Words by books',
       subtitle = 'Stop words removed',
       x = 'Word',
       y = 'Count')



############################################## sentiment #################

bibleTidy <- bible %>%
  select(book,chapter,text) %>%
  unnest_tokens(word,text)%>%
  filter(!word %in% c("god"))


bibleLength <- bibleTidy %>%
  count(book) %>%
  rename(bibleLength = n)


sentRev <- bibleTidy %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(book) %>%
  summarize(sentiment = sum(score)) %>%
  left_join(bibleLength,by='book') %>%
  mutate(aveSentiment = sentiment/bibleLength)




sentRev %>%
  group_by(book) %>%
  summarize(meanSent = mean(aveSentiment)) %>%
  #mutate(reviewRating=factor(reviewRating)) %>%
  ggplot(aes(x=book,y=meanSent,color=book)) + geom_point(size=5,show.legend = F) + 
  geom_hline(aes(yintercept=0)) +
  labs(title='Average Sentiment by Review Rating',
       x = 'book',
       y = 'Average Sentiment')
