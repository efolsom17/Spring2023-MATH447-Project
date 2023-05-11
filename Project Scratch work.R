####Setup####

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

####Loading Data####
beer<-as_tibble(read.csv("https://www.dropbox.com/s/nc3dygybis4wkbk/beer_reviews.csv?dl=1", header=TRUE))
head(beer)
View(head(beer))
View(beer)
beer_map<-select(beer, brewery_id,brewery_name, beer_name, beer_beerid, beer_style, beer_abv)
beer2<-select(beer,-review_profilename, -brewery_name, -brewery_id,-beer_beerid, -review_time)
beer2 <- beer2 %>%
  group_by(beer_name) %>%
  fill(beer_abv, .direction = "downup")
beer2<-beer2%>%
  filter(!is.na(beer_abv))
map2<-select(beer_map, beer_name, beer_style)
map2<-map2%>%distinct(beer_name,.keep_all = TRUE)

####################
#cleaning data####

beer_avg<-beer2%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))
beer_avg<-left_join(beer_avg, map2, by="beer_name")
beer_avg<-as_tibble(beer_avg)

beer_var<-beer2%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste), var))
beer_var<-beer_var%>%replace_na(list(review_overall=0,review_aroma=0,review_appearance=0,review_palate=0,review_taste=0))

colnames.new.var<-c( review_overall_Var="review_overall",review_aroma_Var="review_aroma",review_appearance_Var= "review_appearance",
                     review_palate_Var= "review_palate",review_taste_Var="review_taste")
beer_var<-rename(beer_var, all_of(colnames.new.var))

beer_n<-beer2%>%
  group_by(beer_name)%>%
  summarise(n=n())

beer_avg<-beer_avg%>%
  add_column(n_reviews=beer_n$n,review_overall_Var=beer_var$review_overall_Var,
               review_aroma_Var=beer_var$review_aroma_Var,review_appearance_Var=beer_var$review_appearance_Var,
               review_palate_Var= beer_var$review_palate_Var,review_taste_Var=beer_var$review_taste_Var)

beer_avg<-beer_avg%>%
  select(beer_name, beer_style, beer_abv, 
    n_reviews, review_overall, review_overall_Var, review_aroma, 
    review_aroma_Var, review_appearance, review_appearance_Var, review_palate, 
    review_palate_Var,  review_taste, review_taste_Var)

head(beer_avg)
View(beer_avg)

beer_avg.df<-column_to_rownames(beer_avg,var="beer_name")
head(beer_avg.df)
beer_avg.fac_style<-beer_avg%>%mutate(beer_style=as.factor(beer_style))
head(beer_avg.fac_style)

rm(beer_n,beer_var, colnames.new.var)
#View(beer_var)
#beer_by_style<-beer2%>%
#  group_by(beer_style)%>%
#  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))%>%
#  filter(!is.na(beer_abv))
#beer_by_style<-column_to_rownames(beer_by_style,var='beer_style')
#View(beer_by_style)
#length(beer_by_style$beer_abv)



####Exploratory Data Analysis####

ggplot(beer2, aes(beer_abv, review_overall))+geom_point()
abv.dot<-ggplot(beer2, aes(beer_abv))
abv.scat<-ggplot(beer2, aes(beer_beerid, beer_abv))
abv.scat+geom_point()
abv.dot+geom_dotplot()
pc.beer_by_name<-prcomp(beer_by_name,scale=TRUE)


####model creation####



