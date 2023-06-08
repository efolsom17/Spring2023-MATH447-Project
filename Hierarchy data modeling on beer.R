# Step 1: Data Preprocessing
beer_data <- read.csv("beer_dataset.csv")  # Replace "beer_dataset.csv" with the actual file path
# Preprocess the data as necessary

# Step 2: Similarity Calculation
# Select the relevant columns for similarity calculation
selected_columns <- c("ABV", "aroma", "palate", "taste", "appearance")
selected_data <- beer_data[, selected_columns]

# Calculate similarity between beers using cosine similarity
similarity_matrix <- cosine_similarity(selected_data)

# Step 3: Hierarchy Construction
# Perform hierarchical clustering
hclust_result <- hclust(dist(similarity_matrix), method = "complete")

# Determine the number of clusters or cut-off point
num_clusters <- 5  # Adjust this based on your preference or using an appropriate method

# Cut the dendrogram at the desired number of clusters
clusters <- cutree(hclust_result, k = num_clusters)

# Step 4: Simplification by Beer Style
beer_styles <- beer_data$beer_style
lookup_table <- data.frame(cluster = clusters, beer_style = beer_styles)

# Step 5: Recommendation Generation
generate_recommendations <- function(input_beer, lookup_table, beer_data, num_recommendations) {
  input_cluster <- lookup_table[lookup_table$beer_style == input_beer, "cluster"]
  
  # Filter beers from different clusters (excluding the input cluster)
  recommended_beers <- subset(lookup_table, cluster != input_cluster)
  
  # Randomly select beers from each cluster
  recommended_beers <- aggregate(beer_style ~ cluster, recommended_beers, FUN = function(x) sample(x, num_recommendations, replace = FALSE))
  
  # Extract the recommended beer styles
  recommended_beer_styles <- recommended_beers$beer_style
  
  # Retrieve the full details of the recommended beers
  recommended_beers_data <- subset(beer_data, beer_style %in% recommended_beer_styles)
  
  return(recommended_beers_data)
}

# Step 6: Output Recommendations
input_beer <- "Input Beer Style"  # Replace with the name of the input beer
num_recommendations <- 5  # Adjust the number of recommendations as desired

recommended_beers <- generate_recommendations(input_beer, lookup_table, beer_data, num_recommendations)

# Display the recommendations
print(recommended_beers)
