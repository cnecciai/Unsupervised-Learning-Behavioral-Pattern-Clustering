#-----------------------------------
#title: "Unsupervised Learning Final Project"
#subtitle: 'PCA and Clustering'
#authors: "Saxa Group #9"
    #Kassandra Sellers 
    #Mabel Lorena Barba 
    #Benny Huang 
    #Tony Campoverde 
    #Clark Necciai
#-----------------------------------

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
library(NbClust) #NbClust Method
library(clValid) #Silhouette

#Read in Travel Data
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

#---Function to Remove all Zero Values---
zero_to_na <- function(x) {
    x <- ifelse(x == 0, NA, x)
    return(x)
}

#Replace with NA Values
Travel_Data_Non_Zero <- Travel_Data %>%
    mutate(across(where(is.numeric), zero_to_na)) 

#Visualize Missing Values
vis_miss(Travel_Data_Non_Zero) + coord_flip()

#Show Variables w/ Number of Missing Values (Train Set)
print(paste("Percentage Missing Values for Dataset:"))
Travel_miss <- sapply(Travel_Data_Non_Zero, function(x){sum(is.na(x))})
Travel_miss[Travel_miss != 0]


#Replace Missing Values (per variable) with median
Travel_Data_Zero_Impute <- Travel_Data_Non_Zero %>%
    mutate_all(funs(ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Creating a Second, Reduced Dataset for the Purposes of evaluating PCA and K-means Performance

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


Travel_Data_Full <- Travel_Data_Zero_Impute %>% select(-UserID) %>% data.frame()
Travel_Data_Reduced <- Travel_Data_No_Outliers %>% select(-UserID) %>% data.frame()

#Summary Statistics of Dataset with All Observations
Travel_Data_Full %>% describe()

#Summary Statistics of Dataset with Outlier Values Removed
Travel_Data_Reduced %>% describe()


#Correlation Matrix for Full Dataset
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
Travel_PCA_Full = prcomp(Travel_Data_Full, scale. = TRUE)
Travel_PCA_Reduced = prcomp(Travel_Data_Reduced, scale. = TRUE)

#Show how the Variables are related to the Principal Components
pca_full_var <- get_pca_var(Travel_PCA_Full)
pca_reduced_var <- get_pca_var(Travel_PCA_Reduced)

corrplot(pca_full_var$contrib, is.corr = F, tl.col = "black",)
corrplot(pca_reduced_var$contrib, is.corr = F, tl.col = "black",)

#Variable Contributions to Principal Components
fviz_contrib(Travel_PCA_Full, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(Travel_PCA_Reduced, choice = "var", axes = 1) # Contributions of variables to PC1

fviz_contrib(Travel_PCA_Full, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(Travel_PCA_Reduced, choice = "var", axes = 2) # Contributions of variables to PC2

#Visualize explained variances per component
fviz_eig(Travel_PCA_Full, addlabels=TRUE)+
    labs(x = "Dimensions (Pricipal Components)",
         title = "Scree-Plot for Full Dataset")

#Actually Achieve better Variance for first few Principal Components
fviz_eig(Travel_PCA_Reduced, addlabels=TRUE)+
    labs(x = "Dimensions (Pricipal Components)",
         title = "Scree-Plot for Dataset without Outliers")
    
#Retain Determined Percentage of Data (80%)
get_eig(Travel_PCA_Full)
ReducedPCA <- Travel_PCA_Full$x[,1:13]

get_eig(Travel_PCA_Reduced)
ReducedPCA_Outlierless <- Travel_PCA_Reduced$x[,1:11]

fviz_pca_var(Travel_PCA_Full,  col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA Full Dataset") 


fviz_pca_var(Travel_PCA_Reduced,  col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA Outliers Removed")


#---K-Means Clustering---

#Three methods: Elbow, Silhouette, and Gap statistics

#Elbow method - with maximum 5 clusters
fviz_nbclust(ReducedPCA, kmeans, k.max=5, nstart=30, method="wss")
fviz_nbclust(ReducedPCA_Outlierless, kmeans, k.max=5, nstart=30, method="wss")

#Silhouette method - maximum 5 clusters
fviz_nbclust(ReducedPCA, kmeans, k.max=5, nstart=30, method="silhouette")
fviz_nbclust(ReducedPCA_Outlierless, kmeans, k.max=5, nstart=30, method="silhouette")

#Compute gap statistics - maximum 5 clusters and 30 bootstrap samples
set.seed(2023)
fviz_nbclust(ReducedPCA, kmeans, k.max=5, nstart=30, method="gap_stat", nboot=30)
fviz_nbclust(ReducedPCA_Outlierless, kmeans, k.max=5, nstart=30, method="gap_stat", nboot=30)

#Verify Using NbClust for K-means --> Showing 3 as the best number of clusters
nb_clust_full <- NbClust(ReducedPCA, distance = "euclidean", min.nc = 2, 
                         max.nc = 5, method = "kmeans")

#Overwhelmingly showing 17 supporting 3 as the optimal number of clusters
nb_clust_reduced <- NbClust(ReducedPCA_Outlierless, distance = "euclidean", min.nc = 2,
                            max.nc = 5, method = "kmeans")


#Knowing that K-means is susceptible to outliers and that we've chosen 
#to include the supposed outliers for our full dataset, we're going to run through
#k-medoids with that dataset to see how the groupings may change (If at all)

#Our full dataset actually appears to have very little change in groups.
#This to us, suggests that our groupings are tight enough to warrant 
#trusting in the original k-means cluster decision of having 3 groups

#PAM Clustering
pamclus = pam(ReducedPCA, 3)

#3d Plot of cluster
plot_ly(as.data.frame(ReducedPCA),
        x=~PC1,y=~PC2,z=~PC3, color = pamclus$clustering,
        colors = "Set1",
        size = 4) %>%
    add_markers()


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



#NOW THAT WE HAVE MADE A DETERMINATION AS TO THE APPROPRIATE NUMBER OF CLUSTERS, 
#WE SHOULD ATTACH THOSE LABELS TO THE ORIGINAL VARIABLES AND TRY AND SEE IF THERE
#ARE ANY PATTERNS WE SHOULD CAPITALIZE ON TO GUIDE OUR MARKETING CAMPAIGN

Cluster_And_Travel_Data <- cbind(Group = k3_full$cluster, Travel_Data_Zero_Impute)

summary_df <- Cluster_And_Travel_Data %>%
    group_by(Group) %>%
    summarise(across(where(is.numeric), 
                     list( median = median)))

#Each Group Had a Median Rating... Let's Target Our Campaign
highest_medians <- c()
target_group <- c()
consider_trip <- c()
for (i in 2:ncol(summary_df)) {
    var_name = summary_df[,i] %>% names()
    max_for_var = (max(summary_df[,i]))
    for (j in 1:3) {
        if (summary_df[j,i] == max_for_var) {
            target_group = c(target_group, j)
            consider_trip = c(consider_trip, var_name)
            highest_medians = c(highest_medians, max_for_var)
        }
    }
}

travel_guide <- data.frame(
    "TargetGroup" = target_group,
    "HighestMedianRating" = as.numeric(highest_medians),
    "Corresponding Location" = str_remove(consider_trip, "_median")
)


#Use Sorted Travel Guide Information To target specific groups
#for certain locations of interest
travel_guide %>%
arrange(TargetGroup, desc(HighestMedianRating))



