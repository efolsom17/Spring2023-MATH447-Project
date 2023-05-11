#Setup####

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("tidyverse","neuralnet","conflicted", "GGally","randomForest"))
library(tidyverse)
library(neuralnet)
library(conflicted)
library(GGally)
library(randomForest)
library(reshape)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::rename)

#so cam can use his python
#install.packages("reticulate")
#library(reticulate)
#click on terminal and type in the following:
#conda create --prefix ./envs python=3.10

#then follow onscreen instructions

###################
#Loading Data####
#downloading and reading the beer reviews dataset as a tibble
beer<-as_tibble(read.csv("https://www.dropbox.com/s/nc3dygybis4wkbk/beer_reviews.csv?dl=1", header=TRUE))
head(beer)
###################
#cleaning data####

#We will remove some of the non-important variables, but we may need them for later
beer_map<-select(beer, brewery_id,brewery_name, beer_name, beer_beerid, beer_style, beer_abv)
#removing some of the unnecessary columns in the dataset
beer.clean<-select(beer,-review_profilename, -brewery_name, -brewery_id,-beer_beerid, -review_time)
#a lot of abv values were missing so we need to take care of those
#filling in the missing values that we can from other reviews of the same beer
beer.clean <- beer.clean %>%
  group_by(beer_name) %>%
  fill(beer_abv, .direction = "downup")
#removing the rows with missing values which we couldn't fill in
beer.clean<-beer.clean%>%
  filter(!is.na(beer_abv))
#now beer.clean is clean raw data if we need to use it later


#this will be useful later
map2<-select(beer_map, beer_name, beer_style)
map2<-map2%>%distinct(beer_name,.keep_all = TRUE)

##condensing the data down to what we want

#taking the average review of each beer for each category
beer_avg<-beer.clean%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))
#adding the style of each beer
beer_avg<-left_join(beer_avg, map2, by="beer_name")
beer_avg<-as_tibble(beer_avg)

#getting the variance of each review
beer_var<-beer.clean%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste), var))
#for the beers where there were only one review, there is 0 variance,
#but because of the sample variance formula, we would be dividing by 0, which induced NAs
beer_var<-beer_var%>%replace_na(list(review_overall=0,review_aroma=0,review_appearance=0,review_palate=0,review_taste=0))

#renaming some columns for later use
colnames.new.var<-c( review_overall_Var="review_overall",review_aroma_Var="review_aroma",review_appearance_Var= "review_appearance",
                     review_palate_Var= "review_palate",review_taste_Var="review_taste")
beer_var<-rename(beer_var, all_of(colnames.new.var))

#calcualting the number of reviews for each beer
beer_n<-beer.clean%>%
  group_by(beer_name)%>%
  summarise(n=n())

#putting everything from above together
beer_avg<-beer_avg%>%
  add_column(n_reviews=beer_n$n,review_overall_Var=beer_var$review_overall_Var,
               review_aroma_Var=beer_var$review_aroma_Var,review_appearance_Var=beer_var$review_appearance_Var,
               review_palate_Var= beer_var$review_palate_Var,review_taste_Var=beer_var$review_taste_Var)
#ordering the columns in a way that it is easy to view raw
beer_avg<-beer_avg%>%
  select(beer_name, beer_style, beer_abv, 
    n_reviews, review_overall, review_overall_Var, review_aroma, 
    review_aroma_Var, review_appearance, review_appearance_Var, review_palate, 
    review_palate_Var,  review_taste, review_taste_Var)


head(beer_avg)
View(beer_avg)

#need this for PCA
beer_avg.df<-column_to_rownames(beer_avg,var="beer_name")
head(beer_avg.df)

#need this for something else I think. If we want to use some form of logistic regression
beer_avg.fac_style<-beer_avg.df%>%mutate(beer_style=as.factor(beer_style))
head(beer_avg.fac_style)

#List object, each item in the list corresponds to the style of beer
beer.avg_list<-split(beer_avg, beer_avg$beer_style)
View(beer.avg_list)

#raw list of the data, broken down by beer style
beer.raw_list<-split(beer.clean, beer.clean$beer_style)
View(beer.raw_list)
#cleaning up the R environment
rm(beer_n,beer_var, colnames.new.var)

#un-comment if you want to save a .csv of the data
#write_csv(beer_avg, "beer_reviews_average.csv")
#write_csv(beer.clean, "beer_reviews_clean.csv")

#stuff we probably won't use, but if we need it just uncomment the code
#View(beer_var)
#beer_by_style<-beer.clean%>%
#  group_by(beer_style)%>%
#  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))%>%
#  filter(!is.na(beer_abv))
#beer_by_style<-column_to_rownames(beer_by_style,var='beer_style')
#View(beer_by_style)
#length(beer_by_style$beer_abv)


###################
#Exploratory Data Analysis####

#making life a little easier for now
attach(beer_avg)

#not gonna comment this till we have something concrete

styles<-distinct(beer_avg, beer_style)

#pairs plot, kinda cool looking, WARNING: will turn your macbook into a fighter-jet
ggpairs(beer_avg, columns=c(3,4,5,7,9,11,13),aes(color = beer_style, alpha = 0.5))

#I think we need to remove some outliers from the abv

#get ready for a lot of scatter plots, mostly comparing with overall review

abv.plot<-ggplot(beer_avg, aes(beer_abv,review_overall, color=beer_style))
aroma.plot<-ggplot(beer_avg, aes(review_aroma,review_overall, color=beer_style))
appearance.plot<-ggplot(beer_avg, aes(review_appearance,review_overall, color=beer_style))
palate.plot<-ggplot(beer_avg, aes(review_palate,review_overall, color=beer_style))
taste.plot<-ggplot(beer_avg, aes(review_taste,review_overall, color=beer_style))

#abv vs overall review

abv.plot+geom_point()+geom_smooth(method = lm, se = FALSE)+
  theme(legend.position = "none")
abv.plot.ungrouped<-ggplot(beer_avg, aes(beer_abv,review_overall))
abv.plot.ungrouped+geom_point()+geom_smooth()+
  theme(legend.position = "none")

#R really doesn't want to make a heatmap, this will be figured out later
heatmap_data <- beer_avg[c('review_overall', 'review_aroma', 'review_appearance', 'review_palate', 'review_taste')]

heatmap_data<-as.matrix(heatmap_data)
heatmap(heatmap_data)

# Create the plot object using ggplot2
ggplot(data = heatmap_data_melt, aes(x = names, y = name, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation", guide = guide_colourbar(ticks=T, 
                                                                            barwidth = 10, barheight = 0.5, 
                                                                            title.position = 'top', 
                                                                            title.hjust = 0.5, title.vjust=1.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  labs(title="Heatmap of Correlation Matrix", 
       subtitle="",
       x="",
       y="")


#testing some scatter plot stuff
ggplot(beer.clean, aes(beer_abv, review_overall))+geom_point()
abv.dot<-ggplot(beer.clean, aes(beer_abv))
abv.scat<-ggplot(beer.clean, aes(beer_beerid, beer_abv))
abv.scat+geom_point()
abv.dot+geom_dotplot()

pc.beer<-prcomp(beer_avg.fac_style[,-1], scale=TRUE)
pc.beer$rotation
biplot(pc.beer, scale=0)
pc.beer.var<-pc.beer$sdev^2
pve.beer<-pc.beer.var/sum(pc.beer.var)
temp.x<-c(1:length(pve.beer))
pve.df<-data.frame(pve.beer,temp.x)
scree.beer<-ggplot(pve.df, aes(x=temp.x, y=pve.beer))
scree.beer+geom_point()+geom_line()
temp.x
detach(beer_avg)

####model creation####

#Choose either 80/20 or 50/50 split for training and testing
#and comment out the other so you don't make mistakes

#80-20 split for training and testing data
set.seed(42069)
train<-sample(c(1:nrow(beer_avg)),floor(0.80 * nrow(beer_avg)))
test<- -train
beer.train<-beer_avg[train,]
beer.test<-beer_avg[test,]

#50/50 splot for training and testing data
set.seed(42069)
train<-sample(c(1:nrow(beer_avg)),floor(nrow(beer_avg)/2))
test<- -train
beer.train<-beer_avg[train,]
beer.test<-beer_avg[test,]