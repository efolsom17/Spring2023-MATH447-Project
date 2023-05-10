rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("tidyverse","neuralnet","conflicted", "GGally"))
library(tidyverse)
library(neuralnet)
library(conflicted)
library(GGally)
conflicts_prefer(dplyr::select)
#loading data
beer<-as_tibble(read.csv("https://www.dropbox.com/s/nc3dygybis4wkbk/beer_reviews.csv?dl=1", header=TRUE))
head(beer)
View(head(beer))
View(beer)
beer_map<-select(beer, brewery_id,brewery_name, beer_name, beer_beerid, beer_style)
beer2<-select(beer,-review_profilename, -brewery_name, -brewery_id,-beer_beerid, -review_time)

#cleaning data
beer_by_name<-beer2%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))
beer_by_name<-column_to_rownames(beer_by_name,var='beer_name')
beer_by_style<-beer2%>%
  group_by(beer_style)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))
beer_by_style<-column_to_rownames(beer_by_style,var='beer_style')
View(beer_by_name)
View(beer_by_style)

#Exploratory Data Analysis

ggplot(beer2, aes(beer_abv, review_overall))+geom_point()
abv.dot<-ggplot(beer2, aes(beer_abv))
abv.scat<-ggplot(beer2, aes(beer_beerid, beer_abv))
abv.scat+geom_point()
abv.dot+geom_dotplot()
pc.beer_by_name<-prcomp(beer_by_name,scale=TRUE)


#model creation