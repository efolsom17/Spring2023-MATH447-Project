library(class)

# Step 1: Data Preprocessing
beer_data <- read.csv("beer_dataset.csv")  # Replace "beer_dataset.csv" with the actual file path
# Preprocess the data as necessary

# Step 2: Feature Selection and Scaling
selected_columns <- c("ABV", "aroma", "palate", "taste", "appearance")
selected_data <- beer_data[, selected_columns]

# Perform feature scaling if required
scaled_data <- scale(selected_data)

# Step 3: Simplification by Beer Style
beer_styles <- beer_data$beer_style
lookup_table <- data.frame(beer_style = beer_styles)

# Step 4: Recommendation Generation
generate_recommendations <- function(input_beer, scaled_data, lookup_table, num_recommendations) {
  # Find the index of the input beer
  input_index <- which(lookup_table$beer_style == input_beer)
  
  # Find the K nearest neighbors
  k <- num_recommendations
  knn_results <- knn(scaled_data, scaled_data[input_index, ], lookup_table, k)
  
  # Extract the recommended beer styles
  recommended_beer_styles <- unique(knn_results)
  
  # Exclude the input beer style if present
  recommended_beer_styles <- recommended_beer_styles[recommended_beer_styles != input_beer]
  
  # Retrieve the full details of the recommended beers
  recommended_beers_data <- subset(beer_data, beer_style %in% recommended_beer_styles)
  
  return(recommended_beers_data)
}

# Step 5: Output Recommendations
input_beer <- "Input Beer Style"  # Replace with the name of the input beer
num_recommendations <- 5  # Adjust the number of recommendations as desired

recommended_beers <- generate_recommendations(input_beer, scaled_data, lookup_table, num_recommendations)

# Display the recommendations
print(recommended_beers)
