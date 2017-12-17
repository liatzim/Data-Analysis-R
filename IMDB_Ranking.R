# IMDB movies 

knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(ggplot2) # visualisation 
library(dplyr) # data manipulation for data frames
library(Hmisc)
library(psych)

# Open the data set and describe it 
movie <- read.csv('movie.csv', stringsAsFactors = F)
str(movie)
dim(movie)
summary(movie)

# distribution of the amount of reviews
  # Rights skewed graph that displays a large amount of reviews in the range 
  # of 50 - 200 reviews. The maximum reviews per moview is 813. 

ggplot(aes(x = num_critic_for_reviews), data = movie) + 
  geom_histogram(bins = 30, color = 'red') + 
  labs(title='Distribution of Reviews')+
  labs(subtitle='IMDB')+
  theme(axis.text.x=element_text(angle=-45, hjust=0, vjust=1))+
xlab('Number of Reviews')

# Distribution of movie rankings. 
  # Most reviews ranked between 6-7, left skewed.
  # Large majority receives above 5. 

ggplot(aes(x = imdb_score), data = movie) + 
  geom_histogram(bins = 20, color = 'maroon') + 
  ggtitle('Distribution of rankings')+
 theme(plot.title=element_text(size=25, hjust=0.5, face="bold", colour="maroon", vjust=-1))+
  xlab('Ranking')


# Distribution of years. Most moviews were produced after 2000
  # In 2009 260 new movies came out to theaters, that is the highest number
  # of movies per year. 
  # Last year only 106 new movies came out.

ggplot(aes(x = title_year), data = movie) + 
  geom_histogram(color='white') +
  ggtitle('Distribution of Years')


# In this graph we can see that from 2000 onwords, 
# larger amount of movies received lower ranking, with many outliers around
# the low rankings. Taking this into account, we must remember that from 1916-1960 
# there haven't been too many moviews out there (in our data set), thus, 
# it is very likely to see such a distribution. 

boxplot(imdb_score ~ title_year, data=movie, col='lightgreen')
title("Distribution of Mean Ranking 1916-2015")

# Bar plot of movies produces by countries. 
  # The US is far ahead of every other country

countries <- group_by(movie, country)
movies <- summarise(countries,
                              mean_ranking = mean(imdb_score),
                              n = n()) 

ggplot(aes(x = country, y = n, fill = country), data = movies) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none", axis.text=element_text(size=6)) +
  coord_flip() + 
  labs(title='Distribution of Production by Countries')+
  labs(x='Number of movies produces')+
  labs(y='Countries')


# Bar plot of rankings. Although the US produces the most movies, 
  # when looking at the average ranking per country, one could see that other countries 
  # are pretty close to the US. 
  # Especially, Libia, UA, Kyrgystan. 

ggplot(aes(x = country, y = mean_ranking, fill = country), 
       data = movies) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none", axis.text=element_text(size=7)) +
  coord_flip() + 
  labs(title='Mean Ranking per Country ')+
  labs(x='Average Ranking')+
  labs(y='Countries')

# Directors quality
  # John Blanchard receives on average the highest rankings for his moviews, 
  # with an average of 9.6. The runner up is Frank Darabont, with 9.3 average.

directors <- group_by(movie, director_name, genres)
directors_summary <- summarise(directors,
                               mean_ranking = mean(imdb_score))

directors_summary <- directors_summary[with(directors_summary, order(-mean_ranking)), ]
directors_summary <- head(directors_summary, 20)

ggplot(aes(x = mean_ranking, y = director_name), data = directors_summary) +
  geom_point(aes(color = genres), size = 2) + xlab("Mean Ranking") + 
  ylab("Directors")+ theme_dark() + ggtitle('Directors by Ranking')


# Predicting Rankings

# Dealing with missing values

movie$imdb_score <- as.numeric(impute(movie$imdb_score, mean))
movie$num_critic_for_reviews <- as.numeric(impute(movie$num_critic_for_reviews, mean))
movie$duration <- as.numeric(impute(movie$duration, mean))
movie$director_facebook_likes <- as.numeric(impute(movie$director_facebook_likes, mean))
movie$actor_3_facebook_likes <- as.numeric(impute(movie$actor_3_facebook_likes, mean))
movie$actor_1_facebook_likes <- as.numeric(impute(movie$actor_1_facebook_likes, mean))
movie$gross <- as.numeric(impute(movie$gross, mean))
movie$cast_total_facebook_likes <- as.numeric(impute(movie$cast_total_facebook_likes, mean))
movie$facenumber_in_poster <- as.numeric(impute(movie$facenumber_in_poster, mean))
movie$budget <- as.numeric(impute(movie$budget, mean))
movie$title_year <- as.numeric(impute(movie$title_year, median))
movie$actor_2_facebook_likes <- as.numeric(impute(movie$actor_2_facebook_likes, mean))
movie$aspect_ratio <- as.numeric(impute(movie$aspect_ratio, mean))


# Split the data

library(caTools)
set.seed(2017)
split=sample.split(movie$imdb_score,SplitRatio = 0.8)
train_sample=subset(movie, split==T)
test_sample=subset(movie, split==F)

# MLR first fit

movie_sub <- subset(movie, select = c(num_critic_for_reviews, duration, director_facebook_likes, actor_1_facebook_likes, gross, cast_total_facebook_likes, facenumber_in_poster, budget, movie_facebook_likes, imdb_score))

fit <- lm(imdb_score ~ num_critic_for_reviews + duration +    director_facebook_likes + actor_1_facebook_likes + gross + cast_total_facebook_likes + facenumber_in_poster + budget + movie_facebook_likes, data=train_sample)
summary(fit) 

# Second fit - backword elimination model 

fit1 <- lm(imdb_score ~ num_critic_for_reviews + duration +    director_facebook_likes + actor_1_facebook_likes + cast_total_facebook_likes + facenumber_in_poster + movie_facebook_likes, data=train_sample)
summary(fit1) 

# After the second fit, we could conclude that the model explains 14% of the variation in the dependent variable.
# There are 6 significant independent variables, where the duration of the movie has the strongest affect on the predicted score. 





