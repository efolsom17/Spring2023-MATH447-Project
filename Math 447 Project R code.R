#### 447 Project R Code ####
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#### Loading Required Libraries ####
library(tidyverse)
library(conflicted)
library(GGally)
library(reshape)
library(patchwork)
library(glmnet)

conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::rename)

#### Loading Data ####
#downloading and reading the beer reviews dataset as a tibble
beer<-as_tibble(read.csv("https://www.dropbox.com/s/nc3dygybis4wkbk/beer_reviews.csv?dl=1", header=TRUE))

#### Cleaning Data ####
## We are cleaning the data so that we have a dataset with the average review of each beer ###
#We will remove some of the non-important variables, but we may need them for later
beer_map<-select(beer, brewery_id,brewery_name, beer_name, beer_beerid, beer_style, beer_abv)
#removing some of the unnecessary columns in the dataset
beer.clean<-select(beer,-review_profilename, -brewery_id,-beer_beerid, -review_time)
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
map2<-select(beer_map, beer_name, beer_style, brewery_name)
map2<-map2%>%distinct(beer_name,.keep_all = TRUE)

##condensing the data down to what we want

#taking the average review of each beer for each category
beer_avg<-beer.clean%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste, beer_abv), mean))
#adding the style of each beer
beer_avg<-left_join(beer_avg, map2, by="beer_name")
beer_avg<-as_tibble(beer_avg)
#getting the standard deviation of each review
beer_sd<-beer.clean%>%
  group_by(beer_name)%>%
  summarise(across(c(review_overall, review_aroma, review_appearance, review_palate, review_taste), sd))
#for the beers where there were only one review, there is 0 variance,
#but because of the sample variance formula, we would be dividing by 0, which induced NAs
beer_sd<-beer_sd%>%replace_na(list(review_overall=0,review_aroma=0,review_appearance=0,review_palate=0,review_taste=0))

#renaming some columns for later use
colnames.new.sd<-c( review_overall_sd="review_overall",review_aroma_sd="review_aroma",review_appearance_sd= "review_appearance",
                    review_palate_sd= "review_palate",review_taste_sd="review_taste")
beer_sd<-rename(beer_sd, all_of(colnames.new.sd))

#calcualting the number of reviews for each beer
beer_n<-beer.clean%>%
  group_by(beer_name)%>%
  summarise(n=n())

#putting everything from above together
beer_avg<-beer_avg%>%
  add_column(n_reviews=beer_n$n,review_overall_sd=beer_sd$review_overall_sd,
             review_aroma_sd=beer_sd$review_aroma_sd,review_appearance_sd=beer_sd$review_appearance_sd,
             review_palate_sd= beer_sd$review_palate_sd,review_taste_sd=beer_sd$review_taste_sd)
#ordering the columns in a way that it is easy to view raw
beer_avg<-beer_avg%>%
  select(beer_name,brewery_name, beer_style, beer_abv, 
         n_reviews, review_overall, review_overall_sd, review_aroma, 
         review_aroma_sd, review_appearance, review_appearance_sd, review_palate, 
         review_palate_sd,  review_taste, review_taste_sd)
## Creating a new variable which is the avrage of all the review scores
beer_avg<-beer_avg%>%mutate(beer_avg, total_average_score= (review_overall+review_aroma+review_appearance+review_palate+review_taste)/5)
head(beer_avg)

#### Some summary statistics for each category
summary(beer_avg[,c(4,6,8,10,12,14,16)])

##Data visiaulization
ggpairs(beer_avg, columns=c(4,6,8,10,12,14,16))
p1<-ggplot(beer_avg, aes(x=beer_abv, y=review_overall))+
  geom_point(color="#2E8B57")+
  labs(x = "ABV", y = "Overall Review") 
p2<-ggplot(beer_avg, aes(x=review_aroma, y=review_overall))+
  geom_point(color="#FF1493")+
  labs(x = "Aroma", y = "Overall Review") 
p3<-ggplot(beer_avg, aes(x=review_appearance, y=review_overall))+
  geom_point(color="#00FFFF")+
  labs(x = "Appearance", y = "Overall Review") 
p4<-ggplot(beer_avg, aes(x=review_palate, y=review_overall))+
  geom_point(color="#63B8FF")+
  labs(x = "Palate", y = "Overall Review") 
p5<-ggplot(beer_avg, aes(x=review_taste, y=review_overall))+
  geom_point(color="#FA8072")+
  labs(x = "Taste", y = "Overall Review") 
plot.lout<-"
11##22
##33##
44##55
"
p1+p2+p3+p4+p5+plot_layout(design = plot.lout)

#### Modeling ####

#selecting only the numeric variables in our dataset
numeric_df <- beer_avg %>%
  select(beer_abv, n_reviews, review_overall, review_aroma, review_appearance,
         review_palate, review_taste, total_average_score)
numeric_df<-data.frame(numeric_df)

## Fitting a Lasso regression model to see which variables affect the overall review
set.seed(11)
#Number of times we regenerate training sets
B<-1000
#initializing empty vectors for storing results
lasso01mat<-double()
lassobetamat<-double()
mse.lasso<-double()

for (i in 1:B){
  #Setting a random seed for each iteration
  set.seed(i+sample(1:B,1))
  
  #splitting the data into training and testing sets
  train<-sample(1:dim(numeric_df)[1], dim(numeric_df)[1]*.8)
  test<- -train
  beer.train<-numeric_df[train,]
  beer.test<-numeric_df[test,]
  
  train.mat<-model.matrix(review_overall ~. -total_average_score, data=beer.train)[,-1]
  test.mat<-model.matrix(review_overall~. -total_average_score, data=beer.test)[,-1]
  #Fitting lasso regression on the training set
  #note alpha = 1 is default
  fit.lasso<-glmnet(train.mat,beer.train$review_overall, alpha=1)
  #performing cross-fold validation, default is 10 fold
  cv.lasso<-cv.glmnet(train.mat,beer.train$review_overall, alpha=1)
  #choosing the minimum lambda value
  bestlam.lasso<-cv.lasso$lambda.min
  bestlam.lasso
  #predicted values and MSE calculations
  pred.lasso<-predict(fit.lasso, s=bestlam.lasso, newx=test.mat)
  mse.lasso[i]<-mean((pred.lasso-beer.test$review_overall)^2)
  #storing the beta coefficients
  beta.lasso<-predict(fit.lasso, s=bestlam.lasso, type="coefficients")
  lassobetamat<-rbind(lassobetamat, beta.lasso[-1,1])
  #storing whether or not the explanatory variable was included in the model
  #stored as a 1 if it is included and a 0 if it is not included
  lasso01<-as.integer(abs(beta.lasso[-1,1])>0)
  lasso01mat<-rbind(lasso01mat,lasso01)
}
#naming the columns of the matricies of the magnitude of coefficients or if 
#predictor variables were chosen to match the original data
colnames(lasso01mat)<-colnames(numeric_df)[c(-8,-3)]
colnames(lassobetamat)<-colnames(numeric_df)[c(-3,-8)]
## Proportion of time that the predictor variables were chosen
colMeans(lasso01mat)
##  Mean value for each coefficient
colMeans(lassobetamat)
## Average MSE
mean(mse.lasso)
## Ranking our explanatory variables
colMeans(lasso01mat)*colMeans(lassobetamat)

## performing k-means clustering on the dataset to cluster similar beers
# Normalize the data
numeric_df <- scale(numeric_df)
## Choosing K by looking at a graph
set.seed(1234) 
wss2 <- (nrow(numeric_df)-1)*sum(apply(numeric_df,2,var))
for (i in 2:100) {
  wss2[i] <- sum(kmeans(numeric_df, centers=i)$withinss)
}
plot(1:400, wss2, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

## Finding the "optimal" K by seeing which value of K decreases the WSS by less than 5%
set.seed(1234) 
wss <- (nrow(numeric_df)-1)*sum(apply(numeric_df,2,var))
old_wss <- Inf
for (i in 2:100) {
  wss[i] <- sum(kmeans(numeric_df, centers=i)$withinss)
  percentage_decrease <- ((old_wss - wss[i]) / old_wss) * 100
  if (i > 1 && !is.nan(percentage_decrease) && percentage_decrease < 5) {
    break
  }
  old_wss <- wss[i]
}
optimal_k <- which.min(wss)

#Performing K means clustering using our "optimal" K
set.seed(80085) 
kmeans_result <- kmeans(numeric_df, centers=optimal_k, iter.max = 100)

# Add the cluster assignments back to the original data
beer_avg$cluster <- kmeans_result$cluster

#function to recommend beers based on how we clustered them
# We are choosing the 5 most similar beers by default but that can be changed
beer_recommend <- function(beer_name, N = 5) {
  beer_cluster <- beer_avg$cluster[which(beer_avg$beer_name == beer_name)]
  print(paste("Cluster for", beer_name, "(Brewery:",beer_avg$brewery_name[which(beer_avg$beer_name == beer_name)], "):", beer_cluster))
  
  # Filter beers in the same cluster without excluding the input beer
  same_cluster_beers <- beer_avg %>%
    filter(cluster == beer_cluster)
  
  cluster_beers <- same_cluster_beers[same_cluster_beers$beer_name != beer_name, ]
  
  print(paste("Other beers in the same cluster:", nrow(cluster_beers)))
  
  
  if (nrow(cluster_beers) > 0) {
    # Extract numeric features for the given beer and the other beers in the cluster
    beer_features <- matrix(as.numeric(beer_avg[beer_avg$beer_name == beer_name, c(4,5,6,8,10,12,14,16)]), nrow = 1)
    # Compute distances from the given beer to all other beers in the cluster
    distances <- sapply(1:nrow(cluster_beers), function(i) {
      cluster_features <- as.matrix(cluster_beers[i, c(4,5,6,8,10,12,14,16)])
      dist(rbind(beer_features, cluster_features))
    })
    
    
    # Select the N beers with the smallest distances
    closest_beers <- cluster_beers$beer_name[order(distances)[1:N]]
    
    print(paste("Number of recommendations:", length(closest_beers)))
    
    return(closest_beers)
  } else {
    return(character(0))
  }
}

#Seeing which beers our model recommends based on which beers we like

beer_recommend("Coors")
beer_recommend("Modelo Especial")
beer_recommend("Pacífico")
beer_recommend("Pacifico Light")
beer_recommend("Corona Extra")
beer_recommend("Corona Light")
beer_recommend("Rainier Lager")
beer_recommend("Bud Light")
beer_recommend("Bud Light Lime")
beer_recommend("Blue Moon Belgian White")

View(beer_avg)