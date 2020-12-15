library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library('tidyverse')
library("stringr") 
library('ggplot2')
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library('lattice')

horror <- read.csv("horror_movies_cleaned_with_genre.csv")
str(horror)

horror1 <- df %>% 
  mutate(budget = parse_number(budget))

horror1 %>% filter(release_country == 'USA') %>%
  ggplot(aes(review_rating)) +
  geom_histogram() +
  scale_x_log10(labels = scales::dollar)
#it looks almost normal. 


ggplot(horror1, aes(review_rating))+
  geom_histogram(breaks = seq(1,10, by = 0.5))+
  ggtitle('Distribution of review rating')
# it looks normal, so our DV is normal.


horror1 %>%
  filter(!is.na(movie_rating)) %>%
  filter(!is.na(release_country)) %>%
  mutate(release_country = fct_lump(release_country, 10)) %>%
  mutate(movie_rating = fct_lump(movie_rating, 5)) %>%
  ggplot()+
  geom_bar(mapping = aes(x = release_country, fill = movie_rating))+
  ggtitle('Countries with Movie Rating')+
  xlab('Country')+
  ylab('Movie Rating')
# unsurprisingly USA is the most productive country. UK is 2, India .
# Movie rating and countries have a relation. USA produced most R and Not Rated movies


horror %>%
  filter(!is.na(review_rating)) %>%
  filter(!is.na(multi_language)) %>%
  ggplot(aes(multi_language, review_rating))+
  geom_boxplot() + xlab("Multi Language Movies") +
  ylab('Review Rating')
  
# Multi Language movies have higher review rating.


horror %>%
  filter(!is.na(review_rating)) %>%
  filter(!is.na(multi_genre)) %>%
  ggplot(aes(multi_genre, review_rating))+
  geom_boxplot() + xlab("Multi Genre Movies") +
  ylab('Review Rating')

# Multi Genre movies have higher review rating.

ht <- horror %>%
  group_by(main_genre) %>%
           summarise(mean_rating = mean(review_rating, na.rm = TRUE))

ggplot(ht, aes(x = main_genre, y = mean_rating))+
  geom_bar(stat = 'identity') +
  coord_flip() +
  ggtitle("Mean Rating by Genre")
  
ggplot(horror, aes(x = main_genre, y= review_rating)) + geom_boxplot()

# Animated horror movies have the highest mean review rating 


horror1 %>%
  filter(!is.na(movie_rating)) %>%
  filter(!is.na(review_rating)) %>%
  mutate(movie_rating = fct_lump(movie_rating, 5)) %>%
  ggplot(aes(movie_rating, review_rating))+
  geom_boxplot() + xlab("Movie Rating") +
  ylab('Review Rating')
  
# TV Rated movies have the lowest review rating, R rated ones looks like have the highest rating.
summary(horror1$review_rating)

horror2 <- read.csv('halo_reg.csv')

horror2 %>%
  ggplot(aes(review_rating,runtime_in_min )) +
  geom_point() +
  geom_smooth(method = "lm")
lm(review_rating ~ runtime_in_min, data = horror2) %>%
  anova()
#there is linear relation between runtime and review rating.
#anova test proves the relations as p value is significant 


horror1 %>%
  filter(!is.na(movie_rating)) %>%
  mutate(movie_rating = fct_lump(movie_rating, 5)) %>%
  lm(review_rating ~ movie_rating, data = .) %>%
  anova()
# there is significant relations with movie rating and review rating.

library('tidytext')
install.packages('tm')
library('tm')

h

horror_text <- horror %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!is.na(word)) %>%
  count(word) %>%
  arrange(desc(n))

head (horror_text, 20)
# house, blood, night, dark, evil, zombie, massacre, death, hell are the most common words in movei titles