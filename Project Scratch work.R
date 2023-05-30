#Setup####

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
install.packages(c("tidyverse","neuralnet","conflicted", "GGally","randomForest","patchwork"))
library(tidyverse)
library(neuralnet)
library(conflicted)
library(GGally)
library(randomForest)
library(reshape)
library(patchwork)

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

beer_avg.nosd<-beer_avg
#getting the variance of each review
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
beer_avg.nosd<-beer_avg.nosd%>%
  add_column(n_reviews=beer_n$n)
#ordering the columns in a way that it is easy to view raw
beer_avg<-beer_avg%>%
  select(beer_name,brewery_name, beer_style, beer_abv, 
    n_reviews, review_overall, review_overall_sd, review_aroma, 
    review_aroma_sd, review_appearance, review_appearance_sd, review_palate, 
    review_palate_sd,  review_taste, review_taste_sd)
beer_avg<-beer_avg%>%mutate(beer_avg, total_average_score= (review_overall+review_aroma+review_appearance+review_palate+review_taste)/5)
head(beer_avg)
View(beer_avg)



#### Extra Data manipulation ####

#need this for PCA
beer_avg.df<-column_to_rownames(beer_avg,var="beer_name")
head(beer_avg.df)

beer_avg.nosd.df<-column_to_rownames(beer_avg.nosd, var="beer_name")

#need this for something else I think. If we want to use some form of logistic regression
beer_avg.fac_style<-beer_avg.df%>%mutate(beer_style=as.factor(beer_style))
head(beer_avg.fac_style)

beer_novsd.fac<-beer_avg.nosd.df%>%mutate(beer_style=as.factor(beer_style))
#List object, each item in the list corresponds to the style of beer
beer.avg_list<-split(beer_avg, beer_avg$beer_style)
View(beer.avg_list)
beer_avg.nosd.list<-split(beer_avg.nosd, beer_avg.nosd$beer_style)
#raw list of the data, broken down by beer style
beer.raw_list<-split(beer.clean, beer.clean$beer_style)
View(beer.raw_list)

#list by beer name (raw data)

beer_name.list<-split(beer.clean, beer.clean$beer_name)


#cleaning up the R environment
#rm(beer_n,beer_var, colnames.new.var)

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
abv.plot.ungrouped+geom_point()+geom_smooth(method = lm, se = FALSE)+
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
ggplot(beer.clean, aes(beer_abv, review_overall, color=beer_style))+geom_point()
abv.dot<-ggplot(beer.clean, aes(beer_abv))
abv.scat<-ggplot(beer.clean, aes(beer_beerid, beer_abv, color=beer_style))
abv.scat+geom_point()
abv.dot+geom_dotplot()

#PCA
pc.beer.avg<-prcomp(beer_avg.fac_style[,-1], scale=TRUE)
pc.beer.avg$rotation
#biplot(pc.beer.avg, scale=0)
pc.beer.avg.var<-pc.beer.avg$sdev^2
pve.beer.avg<-pc.beer.avg.var/sum(pc.beer.avg.var)
temp.x<-c(1:length(pve.beer.avg))
pve.avg.df<-data.frame(pve.beer.avg,temp.x)
scree.beer.avg<-ggplot(pve.avg.df, aes(x=temp.x, y=pve.beer.avg))
avg.scree<-scree.beer.avg+geom_point()+geom_line()
avg.scree
pve.cum.avg<-data.frame(cumsum(pve.beer.avg),temp.x)
pve.cum.avg
cum.scree.beer<-ggplot(pve.cum.avg, aes(x=temp.x,y=cumsum.pve.beer.avg.))
scree.cum<-cum.scree.beer+geom_point()+geom_line()
scree.cum
plot.layout<-"
11#
#22
"
avg.scree+scree.cum+plot_layout(design=plot.layout)

pc.novar<-prcomp(beer_novar.fac[,-7],scale=TRUE)
pc.novar$rotation<- -1*pc.novar$rotation
pc.novar$rotation
biplot(pc.novar, scale=0)
pc.novar_var<-pc.novar$sdev^2
pve.novar<-pc.novar_var/sum(pc.novar_var)
x.novar<-c(1:length(pve.novar))
pve.novar.df<-data.frame(pve.novar,x.novar)
scree.novar<-ggplot(pve.novar.df, aes(x=x.novar, pve.novar))+geom_line()+geom_point()
cumsum(pve.novar)
cum.novar.df<-data.frame(cumsum(pve.novar), x.novar)
cum.scree.novar<-ggplot(cum.novar.df, aes(x=x.novar, y=cumsum.pve.novar.))+geom_line()+geom_point()

scree.novar+cum.scree.novar+plot_layout(design=plot.layout)
pc.novar$rotation
######This doesn't really work :(
beer.clean.factor<-beer.clean%>%mutate(beer_style=as.factor(beer_style),beer_name=as.factor(beer_name))

pc.beer.clean<-prcomp(beer.clean.factor, scale=TRUE)

detach(beer_avg)

####model creation####

#Choose either 80/20 or 50/50 split for training and testing
#and comment out the other so you don't make mistakes

#80-20 split for training and testing data
set.seed(42069)
train<-sample(c(1:nrow(beer_avg.fac_style)),floor(0.80 * nrow(beer_avg.fac_style)))
test<- -train
beer.train<-beer_avg.fac_style[train,]
beer.test<-beer_avg.fac_style[test,]

#50/50 splot for training and testing data
set.seed(42069)
train<-sample(c(1:nrow(beer_avg)),floor(nrow(beer_avg)/2))
test<- -train
beer.train<-beer_avg[train,]
beer.test<-beer_avg[test,]


#trying to do hierarchical clustering:

hc.complete <- hclust(dist(beer.train), method = "complete")
hc.average <- hclust(dist(beer.train), method = "average")
hc.single <- hclust(dist(beer.train), method = "single")

#classification tree
attach(beer.train)
library ( tree )
beer.tree<-tree(beer_name~., beer.train)
summary(beer.tree)

kmeans.beer<-kmeans(beer.train, 100)
plot(beer.train)


#### This works ####

#performing K means clustering on all the numeric data, then finding the most similar beers within the cluster to make our reccomendations

numeric_df <- beer_avg %>%
  select(beer_abv, n_reviews, review_overall, review_aroma, review_appearance, review_palate, review_taste, total_average_score)

# Normalize the data
numeric_df <- scale(numeric_df)

set.seed(123) # Set seed for reproducibility
wss <- (nrow(numeric_df)-1)*sum(apply(numeric_df,2,var))
for (i in 2:400) wss[i] <- sum(kmeans(numeric_df, centers=i)$withinss)
plot(1:400, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


set.seed(42069) # Set seed for reproducibility
kmeans_result <- kmeans(numeric_df, centers=40, iter.max = 100)

# Add the cluster assignments back to the original data
beer_avg$cluster <- kmeans_result$cluster



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

