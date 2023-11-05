install.packages("sf")      # For working with shapefiles
install.packages("leaflet") # For geospatial mapping
install.packages("caret")



install.packages("rmarkdown")

setwd("C:/Users/USER/Desktop/Final Project")



# Step 1: Load the required R packages
library(sf)      # For working with shapefiles
library(leaflet) # For geospatial mapping
library(rgdal)
library(raster)
library(terra)
library(dplyr)
library(ggplot2)
library(rgeos)
library(viridis)
library(rasterVis)
library(randomForest)

# Step 2: Load the in-situ data (shapefile)
in_situ_data <- st_read("LULC_Crop-Types_In-SituData2021_USP/LULC_Crop-Types_In-SituData2021_USP.shp")

head(in_situ_data)


# Remove the specified columns from your data frame
in_situ_data <- in_situ_data %>%
  select(-data_Other, -data_Photo, -data_UserN, -data_Phone, -data_Subsc, -data_Email, -data_EndTi, -data_meta_, -F17, -F18)

head(in_situ_data)

# Filter rows where data_LULC is 'agriculture'
insitu_filtered <- in_situ_data %>% filter(data_LULC == 'Agriculture')


head(insitu_filtered)

# Define a custom color palette for the specific class types
color_palette <- colorFactor(
  palette = c("lightgreen", "yellow", "darkgreen", "cyan", "orange"),  # Define colors for each class type
  domain = c("Sugarcane", "PaddyRice", "Orchid","Bamboo", "OtherCrop")  # Define class types
)

# Create a leaflet map
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = insitu_filtered, 
                   lng = ~st_coordinates(geometry)[, 1], 
                   lat = ~st_coordinates(geometry)[, 2], 
                   popup = ~data_CropT,
                   color = ~color_palette(data_CropT)) # Apply the color palette to class types




#importing sentinel image data
blue_band <- raster("T45RVK_20221218T045211_B02_10m.jp2")
green_band <- raster("T45RVK_20221218T045211_B03_10m.jp2")
red_band <- raster("T45RVK_20221218T045211_B04_10m.jp2")
nir_band <- raster("T45RVK_20221218T045211_B08_10m.jp2")


# Calculation NDVI
ndvi_funtion <- function(red_band, nir_band) {
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  return(ndvi)
}

ndvi <- ndvi_funtion(red_band , nir_band)


#plotting NDVI
gplot(ndvi) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  coord_quickmap() +
  ggtitle("NDVI") +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +   					   
  theme(plot.title = element_text(hjust = 0.5),             
        text = element_text(size=15),		       	    
        axis.text.x = element_text(angle = 90, hjust = 1)) 


#Projecting data from latlong to easting northing
crs_target <- st_crs("+init=EPSG:32645")

insitu_filtered_projected <- st_transform(insitu_filtered, crs_target)

# Extract NDVI values for insitu data locations
ndvi_values <- raster::extract(ndvi, insitu_filtered_projected)

# Add the extracted NDVI values to the insitu_filtered_projected data
insitu_merged <- cbind(insitu_filtered_projected, NDVI = ndvi_values)
head(insitu_merged)


# Remove rows with NA values in the NDVI column
insitu_merged <- insitu_merged[!is.na(insitu_merged$NDVI), ]


# Define the proportion for the training data (e.g., 70% for training, 30% for testing)
train_prop <- 0.7

# Set a seed for reproducibility
set.seed(123)

# Create an index for splitting the data
train_index <- sample(1:nrow(insitu_merged), size = round(train_prop * nrow(insitu_merged)))

# Split the data into training and testing sets
train_data <- insitu_merged[train_index, ]
test_data <- insitu_merged[-train_index, ]

train_data$data_CropT <- as.factor(train_data$data_CropT)

rf_model <- randomForest(data_CropT ~ NDVI, data = train_data, ntree = 100)


# Make predictions on the test data
rf_predictions <- predict(rf_model, test_data)

confusion_matrix <- table(Actual = test_data$data_CropT, Predicted = rf_predictions)
print(confusion_matrix)

#Calculating Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))

# Define your class names
class_names <- c("Bamboo", "Orchid", "PaddyRice", "Sugarcane", "othercrop")

# Create empty matrices to store precision, recall, and F1 scores
precision_scores <- matrix(0, nrow = length(class_names), ncol = 1)
recall_scores <- matrix(0, nrow = length(class_names), ncol = 1)
f1_scores <- matrix(0, nrow = length(class_names), ncol = 1)


# Calculate precision, recall, and F1 scores for each class
for (i in 1:length(class_names)) {
  class_name <- class_names[i]
  true_positive <- confusion_matrix[class_name, class_name]
  false_positive <- sum(confusion_matrix[, class_name]) - true_positive
  false_negative <- sum(confusion_matrix[class_name, ]) - true_positive
  precision <- true_positive / (true_positive + false_positive)
  recall <- true_positive / (true_positive + false_negative)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Store scores in respective matrices
  precision_scores[i] <- precision
  recall_scores[i] <- recall
  f1_scores[i] <- f1_score
}

# Create a data frame to display the scores
score_df <- data.frame(Class = class_names, Precision = precision_scores, Recall = recall_scores, F1_Score = f1_scores)

# Display the scores
score_df

# Display the F1 scores
f1_scores

# Initialize matrices to store producer accuracy (PA) and user accuracy (UA)
PA_scores <- matrix(0, nrow = length(class_names), ncol = 1)
UA_scores <- matrix(0, nrow = length(class_names), ncol = 1)

# Calculate producer accuracy and user accuracy for each class
for (i in 1:length(class_names)) {
  class_name <- class_names[i]
  
  # Calculate Producer Accuracy (PA)
  PA <- confusion_matrix[class_name, class_name] / sum(confusion_matrix[class_name, ])
  PA_scores[i] <- PA

  # Calculate User Accuracy (UA)
  UA <- confusion_matrix[class_name, class_name] / sum(confusion_matrix[, class_name])
  UA_scores[i] <- UA
}
# Create a data frame to display the scores
accuracy_df <- data.frame(Class = class_names, Producer_Accuracy = PA_scores, User_Accuracy = UA_scores)

# Display the producer and user accuracy scores
accuracy_df


