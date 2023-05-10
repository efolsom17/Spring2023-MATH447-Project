rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("tidyverse","neuralnet","conflicted", "GGally","randomForest"))
library(tidyverse)
library(neuralnet)
library(conflicted)
library(GGally)
library(randomForest)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
#loading data
beer<-as_tibble(read.csv("https://www.dropbox.com/s/nc3dygybis4wkbk/beer_reviews.csv?dl=1", header=TRUE))
head(beer)
View(head(beer))
View(beer)
beer_map<-select(beer, brewery_id,brewery_name, beer_name, beer_beerid, beer_style, beer_abv)
beer2<-select(beer,-review_profilename, -brewery_name, -brewery_id,-beer_beerid, -review_time)
sum(is.na(beer2$beer_abv))
beer2 <- beer2 %>%
  group_by(beer_name) %>%
  fill(beer_abv, .direction = "downup")
sum(is.na(beer2$beer_abv))
map2<-select(beer_map, beer_name, beer_style)
map2<-map2%>%distinct(beer_name,.keep_all = TRUE)

#cleaning data
beer_by_name<-beer2%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))%>%
  filter(!is.na(beer_abv))

beer_by_name<-left_join(beer_by_name, map2, by="beer_name")
beer_by_name<-column_to_rownames(beer_by_name,var='beer_name')

View(beer_by_name)
length(beer_by_name$beer_abv)


#beer_by_style<-beer2%>%
#  group_by(beer_style)%>%
#  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))%>%
#  filter(!is.na(beer_abv))
#beer_by_style<-column_to_rownames(beer_by_style,var='beer_style')
#View(beer_by_style)
#length(beer_by_style$beer_abv)

#Exploratory Data Analysis

ggplot(beer2, aes(beer_abv, review_overall))+geom_point()
abv.dot<-ggplot(beer2, aes(beer_abv))
abv.scat<-ggplot(beer2, aes(beer_beerid, beer_abv))
abv.scat+geom_point()
abv.dot+geom_dotplot()
pc.beer_by_name<-prcomp(beer_by_name,scale=TRUE)


#model creation



