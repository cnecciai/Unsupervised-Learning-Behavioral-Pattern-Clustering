#-----------------------------------
#title: "Unsupervised Learning Final Project"
#subtitle: 'PCA and Clustering'
#authors: "Saxa Group #9"
    #Kassandra Sellers - Preprocessing/Docx Formatting
    #Mabel Lorena Barba - Preprocessing
    #Benny Huang - Clustering(K-means)
    #Tony Campoverde - Preprocessing/PCA
    #Clark Necciai

#Outline: 
    #Data Pre-processing
        #Handling Missing Values
    #Exploratory Data Analysis
        #Principal Components Analysis
        #K-Means Clustering
#-----------------------------------
#NOTE:

#It may be the case that the full dataset might be performing so well would be because these reviews given shouldn't be looked
#at as mainly outliers to simply be rid of. Maybe if there were minimal amounts, but these reviews are still valid. They aren't
#invalid and so we shouldn't just arbitrarily throw them away. 

#---Data Pre-Processing---

#Load Libraries
library(tidyverse)
library(readxl)
library(visdat)
library(psych)
library(factoextra)
library(PerformanceAnalytics)
library(plotly)
library(corrplot)

#Read in Travel Data - CHANGE TO CSV
#Travel_Data <- read_xlsx("Travel_Review.xlsx")
Travel_Data <- read_csv("Travel_Review.csv")
#Initial  Inspection
Travel_Data %>% glimpse()

#Dimensionality
print(paste("Number of observations/rows: ", dim(Travel_Data)[1]))
print(paste("Number of columns/variables: ", dim(Travel_Data)[2]))

#Check for Duplicate Observations
duplicates <- Travel_Data[which(duplicated(Travel_Data)), ]
if ((nrow(duplicates)) == 0) {
    print("No duplicates detected")}


#Show Variables w/ Number of Missing Values (Train Set)
print(paste("Percentage Missing Values for Dataset:"))
Travel_miss <- sapply(Travel_Data, function(x){sum(is.na(x))})
Travel_miss[Travel_miss != 0]

#Visualize Missing Training Data
vis_miss(Travel_Data) +
    coord_flip()

#---Missing Value Imputation---

# Calculate median of "Gardens" column - Skewed Distribution
median_gardens <- median(Travel_Data$Gardens, na.rm = TRUE)

# Replace missing value with median
Travel_Data$Gardens[is.na(Travel_Data$Gardens)] <- median_gardens

# Verify the replacement
missing_in_each_column <- colSums(is.na(Travel_Data))
print(missing_in_each_column)

hist(Travel_Data$Monuments)
#------------------------------------end replacing w median-----

#---Exploratory Data Analysis---

# Taking Zero to mean no input/not interested, therefore skews data
# towards a non-meaningful distribution... We're going to remove them 

#---Function to Remove all Zero Values---
remove_zeros <- function(x) {
    x <- ifelse(x == 0, NA, x)
    return(x)
}

#Replace with NA Values
Travel_Data_Non_Zero <- Travel_Data %>%
    mutate(across(where(is.numeric), remove_zeros)) 

#Visualize Missing Values
vis_miss(Travel_Data_Non_Zero, cluster = T) + coord_flip()

replace_zeros <- function(var) {
    median_value = median(var, na.rm = T)
    newvar = replace_na(var, median_value)
    return(newvar)
}

Travel_Data_Zero_Impute <- Travel_Data_Non_Zero %>%
    mutate_all(funs(ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Note: Knowing that K-Means & PCA can be sensitive to outliers, we're going 
#to remove all outlier values from our full dataset and work with 2 datasets
#Full Dataset and Another with All Outliers Removed to see the possible benefits

#---Function to Remove all Outliers---
remove_outliers <- function(x) {
    qnt <- quantile(x, probs=c(0.25, 0.75))
    iqr <- IQR(x)
    lower <- qnt[1] - (1.5 * iqr)
    upper <- qnt[2] + (1.5 * iqr)
    x <- ifelse(x < lower | x > upper, NA, x)
    return(x)
}

Travel_Data_No_Outliers <- Travel_Data_Zero_Impute %>%
    mutate(across(where(is.numeric), remove_outliers)) %>%
    drop_na()

#Describe with Summary Statistics
#UserID obviously useless here but useful for later

Travel_Data_Full <- Travel_Data_Zero_Impute %>% select(-UserID) %>% data.frame()
Travel_Data_Reduced <- Travel_Data_No_Outliers %>% select(-UserID) %>% data.frame()

Travel_Data_Full %>% describe()
Travel_Data_Reduced %>% describe()

#----
#Currently we've imputed all zeros
#Have two datasets
#1. --- Travel_Data_Full --- Full Dataset with outliers + zero answers imputed
#2. --- Travel_Data_Reduced ---  Outliers and zero answers imputed
#----

#Correlation Matrix
corrplot::corrplot(cor(Travel_Data_Full),
                   type = "lower", diag = F, 
                   tl.col = "black", tl.cex = .7,
                   tl.srt = 60)

#Correlation Matrix - Stronger Correlation (PCA may Perform Better)
corrplot::corrplot(cor(Travel_Data_Reduced),
                   type = "lower", diag = F, 
                   tl.col = "black", tl.cex = .7,
                   tl.srt = 60)

#Visualize Histograms of Variables
names <- colnames(Travel_Data_Full)
for (i in 1:ncol(Travel_Data_Full)) {
    chart.Histogram(Travel_Data_Full[,names[i]],
                    xlab = names[i],
                    main = paste("Full Dataset - Distribution of", names[i]),
                    border.col = "black"
    )
    
    chart.Histogram(Travel_Data_Reduced[,names[i]],
                    xlab = names[i],
                    main = paste("Reduced Dataset - Distribution of", names[i]),
                    border.col = "black"
    )
}

#---Principal Components Analysis---

#Extract Principal Components Through SVD
Travel_PCA = prcomp(Travel_Data_Full, scale. = TRUE)

#Extract Principal Components Through SVD
Travel_PCA_Outlierless = prcomp(Travel_Data_Reduced, scale. = TRUE)

#Show how the Variables are related to the Principal Components
pca_var <- get_pca_var(Travel_PCA)
pca_outlierless_var <- get_pca_var(Travel_PCA_Outlierless)

corrplot(pca_var$contrib, is.corr = F, tl.col = "black",)
corrplot(pca_outlierless_var$contrib, is.corr = F, tl.col = "black",)

#Variable Contributions to Principal Components
fviz_contrib(Travel_PCA, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(Travel_PCA_Outlierless, choice = "var", axes = 1) # Contributions of variables to PC1

fviz_contrib(Travel_PCA, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(Travel_PCA_Outlierless, choice = "var", axes = 2) # Contributions of variables to PC2


#Visualize explained variances per component
fviz_eig(Travel_PCA, addlabels=TRUE)+
    labs(x = "Dimensions (Pricipal Components)",
         title = "Scree-Plot for Full Dataset")

#Actually Achieve better Variance for first few Principal Components
fviz_eig(Travel_PCA_Outlierless, addlabels=TRUE)+
    labs(x = "Dimensions (Pricipal Components)",
         title = "Scree-Plot for Dataset without Outliers")
    
#MABEL-INSERT REASONING FOR REDUCTION HERE - Retain 80% of the Variance
get_eig(Travel_PCA)
ReducedPCA <- Travel_PCA$x[,1:13]

get_eig(Travel_PCA_Outlierless)
ReducedPCA_Outlierless <- Travel_PCA_Outlierless$x[,1:11]

fviz_pca_var(Travel_PCA,  col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA Full Dataset") 


fviz_pca_var(Travel_PCA_Outlierless,  col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA Outliers Removed")


#Try two-dimensional plots of principal components
Travel_PCA$x %>% data.frame() %>% ggplot(aes(PC1, PC2)) +
    geom_point()


Travel_PCA_Outlierless$x %>% data.frame() %>% ggplot(aes(PC1, PC2)) +
    geom_point()


#Show 3d dimensional plot of the full Travel Data
#Hard to make out any spheres but there are some apparent
#clusters, but mostly it appears as one big glob
plot_ly(as.data.frame(Travel_PCA$x),
        x=~PC1,y=~PC2,z=~PC3, size = 1.3) %>%
    add_markers()

#Removing the outliers from the full dataset actually 
#seems to slim down the shape from the previous 3d plot
plot_ly(as.data.frame(Travel_PCA_Outlierless$x),
        x=~PC1,y=~PC2,z=~PC3, size = 1.3) %>%
    add_markers()


#---K-Means Clustering---

#Three methods: Elbow, Silhouette, and Gap statistics

#Elbow method - with maximum 5 clusters
fviz_nbclust(ReducedPCA, kmeans, k.max=6, nstart=30, method="wss")
fviz_nbclust(ReducedPCA_Outlierless, kmeans, k.max=6, nstart=30, method="wss")

#Silhouette method - maximum 5 clusters
fviz_nbclust(ReducedPCA, kmeans, k.max=6, nstart=30, method="silhouette")
fviz_nbclust(ReducedPCA_Outlierless, kmeans, k.max=6, nstart=30, method="silhouette")

#Compute gap statistics - maximum 5 clusters and 30 bootstrap samples
set.seed(2023)
fviz_nbclust(ReducedPCA, kmeans, k.max=6, nstart=30, method="gap_stat", nboot=30)
fviz_nbclust(ReducedPCA_Outlierless, kmeans, k.max=6, nstart=30, method="gap_stat", nboot=30)

#Verify Using NbClust for K-means --> Showing 3 as the best number of clusters
nb_clust_full <- NbClust(ReducedPCA, distance = "euclidean", min.nc = 2, 
                         max.nc = 6, method = "kmeans")

#Overwhelmingly showing 17 supporting 3 as the optimal number of clusters
nb_clust_reduced <- NbClust(ReducedPCA_Outlierless, distance = "euclidean", min.nc = 2,
                            max.nc = 6, method = "kmeans")


#Rerun K-means with Optimal K = 3 on Full Dataset K = 5 on Reduced

set.seed(2024)
k3_full = kmeans(ReducedPCA, centers=3, nstart=30)

#--- Full Dataset Plots ---

#2d Plot of Clusters
ReducedPCA %>% data.frame() %>%
    ggplot(aes(PC1, PC2, color = factor(k3_full$cluster))) +
    geom_point() +
    labs(title = "Full Dataset K-Means Optimal Cluster Size: 3") +
    scale_color_manual("Cluster", values = c("red", "orange", "grey")) +
    theme_bw()

#3d Plot of cluster
plot_ly(as.data.frame(ReducedPCA),
        x=~PC1,y=~PC2,z=~PC3, color = k3_full$cluster,
        colors = "Set1",
        size = 4) %>%
    add_markers()

#--- Reduced Dataset Plots ---

set.seed(2024)
k3_full_outlierless = kmeans(ReducedPCA_Outlierless, centers=5, nstart=30)

#2d Plot of Clusters
ReducedPCA_Outlierless %>% data.frame() %>%
    ggplot(aes(PC1, PC2, color = factor(k3_full_outlierless$cluster))) +
    geom_point() +
    labs(title = "Reduced Dataset K-Means Optimal Cluster Size: 5") +
    scale_color_manual("Cluster", values = c("red", "green", "orange", "brown", "grey")) +
    theme_bw()

#3d Plot of cluster
plot_ly(as.data.frame(ReducedPCA_Outlierless),
        x=~PC1,y=~PC2,z=~PC3, color = k3_full_outlierless$cluster,
        colors = "Set1",
        size = 6) %>%
    add_markers()


#Visualize the silhouettes for full and reduced dataset
sile_full = silhouette(k3_full$cluster, dist(ReducedPCA))
fviz_silhouette(sile_full)

#Visualize the silhouettes based on 5 kmeans clusters
sile_full_outlierless = silhouette(k3_full_outlierless$cluster, dist(ReducedPCA_Outlierless))
fviz_silhouette(sile_full_outlierless)


#Based on everything I've seen so far, I'm inclined to believe that the full dataset with the "supposed"
#outliers might actually be the preferred approach. There is an overwhelming consensus that the optimal
#number of clusters is three. 

#--Hierarchical---


