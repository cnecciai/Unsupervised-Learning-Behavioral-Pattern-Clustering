#------------------------------------------------
#| title: "Unsupervised Learning Final Project" |
#| subtitle: 'PCA and Clustering'               | 
#|  authors: "Saxa Group #9"                    | 
#|    Kassandra Sellers                         | 
#|    Mabel Lorena Barba                        | 
#|    Benny Huang                               | 
#|    Tony Campoverde                           | 
#|    Clark Necciai                             | 
#------------------------------------------------

#-------------------------
#---Data Pre-processing---
#-------------------------

#Load Libraries
library(tidyverse)
library(readxl)
library(visdat)
library(psych)
library(factoextra)
library(plotly)
library(corrplot)
library(PerformanceAnalytics)
library(NbClust) 
library(clValid) 

#Read in Travel Data
travel_df <- read_csv("Travel_Review.csv")

#Initial Inspection
travel_df %>% glimpse()

#Dimensionality
print(paste("Number of observations/rows: ", dim(travel_df)[1]))
print(paste("Number of columns/variables: ", dim(travel_df)[2]))

#Check for Duplicate Observations
duplicates <- travel_df[which(duplicated(travel_df)), ]
if ((nrow(duplicates)) == 0) {
    print("No duplicates detected")}

#Show Variables w/ Number of Missing Values
print(paste("Total Missing Values for Dataset:"))
miss <- sapply(travel_df, function(x){sum(is.na(x))})
miss[miss != 0]

#Impute Median for Missing Value of Gardens
travel_df[(is.na(travel_df$Gardens)), ]$Gardens = median(travel_df$Gardens, na.rm = T)

#Verify no Missing Values
miss <- sapply(travel_df, function(x){sum(is.na(x))})
miss[miss != 0]

#---Replace Zero Values With NA---
zero_to_na <- function(x) {
    x <- ifelse(x == 0, NA, x)
    return(x)
}

Travel_Data_Non_Zero <- travel_df %>%
    mutate(across(where(is.numeric), zero_to_na)) 

#Visualize Missing Values & Show Variables w/ Number of Missing Value
vis_miss(Travel_Data_Non_Zero) + coord_flip()
print(paste("Percentage Missing Values for Dataset:"))
Travel_miss <- sapply(Travel_Data_Non_Zero, function(x){sum(is.na(x))})
Travel_miss[Travel_miss != 0]

#Replace Missing Values (per variable) with median
Travel_Data_Zero_Impute <- Travel_Data_Non_Zero %>%
    mutate_all(funs(ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Remove UserID in order to easily work with continuous variables
Travel_Data <- Travel_Data_Zero_Impute %>% select(-UserID) %>% data.frame()

#----------------------------------
#---Preliminary Data Exploration---
#----------------------------------

#Summary Statistics of Dataset with All Observations
Travel_Data %>% describe()

#Correlation Matrix for Full Dataset
corrplot::corrplot(cor(Travel_Data),
                   type = "lower", diag = F, 
                   tl.col = "black", tl.cex = .7,
                   tl.srt = 60)

#Visualize Histograms of Variables
names <- colnames(Travel_Data)
for (i in 1:ncol(Travel_Data)) {
    chart.Histogram(Travel_Data[,names[i]],
                    xlab = names[i],
                    main = paste("Full Dataset - Distribution of", names[i]),
                    border.col = "black"
    )
}

#----------------------------------------
#---Principal Components Analysis(PCA)---
#----------------------------------------

#Extract Principal Components Through SVD
Travel_PCA = prcomp(Travel_Data, scale. = TRUE)

#Show how the Variables are related to the Principal Components
pca_full_var <- get_pca_var(Travel_PCA)

#Variable Correlations 
pca_full_var$coord

#Variable Contributions
corrplot(pca_full_var$contrib, is.corr = F, tl.col = "black")


#Weak Variable Contributions to Principal Components

# Contributions of variables to PC1
fviz_contrib(Travel_PCA, choice = "var", axes = 1) 

# Contributions of variables to PC2
fviz_contrib(Travel_PCA, choice = "var", axes = 2) 

# Contributions of variables to PC3
fviz_contrib(Travel_PCA, choice = "var", axes = 3) 


#Visualize explained variances per component
fviz_eig(Travel_PCA, addlabels=TRUE, ncp = 24)+
    labs(x = "Dimensions (Principal Components)",
         y = "Percentage of Explained Variances",
         title = "Scree-Plot",
         subtitle = "All Contributions from Principal Components") +
    geom_text(label = "Retain 75% Cumulative Variance", 
              x = 12, y = 10, 
              color = "steelblue") +
    geom_curve(xend = 11, x = 12, y = 9.5, yend = 3.7,
               arrow = arrow(length = unit(0.2, "cm")), curvature = .2)
    

#Retain Determined Percentage of Data (75%)
get_eig(Travel_PCA)
Reduced_Dimensional_Data <- Travel_PCA$x[,1:11]

fviz_pca_var(Travel_PCA,  col.var = "cos2",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA") 


#------------------------
#---K-Means Clustering---
#------------------------

#Elbow method
fviz_nbclust(Reduced_Dimensional_Data, kmeans, k.max=5, nstart=30, method="wss")

#Silhouette
fviz_nbclust(Reduced_Dimensional_Data, kmeans, k.max=5, nstart=30, method="silhouette")

#Compute GAP
set.seed(2023)
fviz_nbclust(Reduced_Dimensional_Data, kmeans, k.max=5, nstart=30, method="gap_stat", nboot=20)

#Verify Using NbClust for K-means --> Showing 3 as the best number of clusters
set.seed(123)
NbClust(Reduced_Dimensional_Data, distance = "euclidean", min.nc = 2, 
                         max.nc = 5, method = "kmeans")


#Rerun K-means with Optimal K = 3
set.seed(2024)
K_Means_Model = kmeans(Reduced_Dimensional_Data, centers=3, nstart=30)

#--- Full Dataset Plots ---

#2d Plot of Clusters
Reduced_Dimensional_Data %>% data.frame() %>%
    ggplot(aes(PC1, PC2, color = factor(K_Means_Model$cluster))) +
    geom_point() +
    labs(title = "K-Means Optimal Cluster Size: 3") +
    scale_color_manual("Cluster", values = c("darkred", "orange", "grey")) +
    theme_bw()

#3d Plot of cluster
plot_ly(as.data.frame(Reduced_Dimensional_Data),
        x=~PC1,y=~PC2,z=~PC3, color = K_Means_Model$cluster,
        name = "Hello",
        colors = c("darkred", "orange", "grey"),
        size = 4) %>%
    add_markers()

#Visualize the silhouettes for full and reduced dataset
sile_full = silhouette(K_Means_Model$cluster, dist(Reduced_Dimensional_Data))
fviz_silhouette(sile_full) +
    scale_fill_manual("Cluster", values = c("darkred", "orange", "grey")) +
    scale_color_manual("Cluster", values = c("darkred", "orange", "grey"))


#------------------------------------------------------------------
#---Exploratory Data Analysis/Pattern Discussion/Recommendations---
#------------------------------------------------------------------

#-------------------------------
#Map our cluster labels back to the original dataset to understand the
#difference in variable distributions. This will guide us in building our
#recommendations by considering which Target_Group tends to rate particular
#locations/attractions as being the highest. A Target_Group that have has a
#particularly high rating for any given variable might be a target for a
#marketing campaign.
#-------------------------------

Travel_with_Cluster_Labels <- cbind(Target_Group = factor(K_Means_Model$cluster), Travel_Data_Zero_Impute)

#Original Dataset with Cluster Labels
Travel_with_Cluster_Labels

summary_stats <- Travel_with_Cluster_Labels %>%
    group_by(Target_Group) %>%
    summarise(across(where(is.numeric), 
                     list( median = median)))

#Summary Statistics - Each Group and Corresponding Median Rating Per Variable 
summary_stats

#Retrieve Statistics Relating to How Groups Rated Certain Attractions/Locations
Highest_Median_Rating <- c()
Target_Group <- c()
Location_Attraction <- c()
for (i in 2:ncol(summary_stats)) {
    var_name = summary_stats[,i] %>% names()
    max_median = (max(summary_stats[,i]))
    for (j in 1:3) {
        if (summary_stats[j,i] == max_median) {
            Target_Group = c(Target_Group, j)
            Location_Attraction = c(Location_Attraction, var_name)
            Highest_Median_Rating = c(Highest_Median_Rating, max_median)
        }
    }
}

#-------------------------------
#Using our `Target_Campaign_Guide` sorted by our Target Groups and Their High
#Median Ratings of Particular Locations/Attractions, we Can utilize the Results
#of our PCA and K-Means Clustering to target Certain Groups of People who might
#find interest in other similar. locations/attractions rated highly by other
#tourists in their Target Group Might want to look at the top 3/4 for each of
#the groups since the locations/attractions may be related.
#-------------------------------

Target_Campaign_Guide <- data.frame(
    "Target_Group" = factor(Target_Group),
    "High_Median_Rating" = as.numeric(Highest_Median_Rating),
    "Location_or_Attraction" = str_remove(Location_Attraction, "_median")
)

#Show Statistics of Group Preferences of Destinations/Attractions
Target_Campaign_Guide %>%
    arrange(Target_Group,
            desc(High_Median_Rating))


#Visualize
Target_Campaign_Guide %>% 
    ggplot(aes(x = fct_reorder(Location_or_Attraction, High_Median_Rating),
               High_Median_Rating, fill = Target_Group)) +
    geom_col() + 
    facet_wrap(~Target_Group, nrow = 3, ncol = 1, scales = "free_y") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Group Rating of Preferred Destinations/Attractions", 
         x = "Tourist Destination",
         y = "Rating of Destination/Attraction") +
    theme(legend.position = "bottom") +
    scale_fill_manual("Cluster", values = c("darkred", "orange", "grey")) +
    theme(strip.text = element_blank()) 
