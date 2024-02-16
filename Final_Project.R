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

#---Data Pre-Processing---

#Load Libraries
library(tidyverse)
library(readxl)
library(visdat)
library(psych)
library(factoextra) #For PCA
library(PerformanceAnalytics)
library(plotly)

#Read in Travel Data - Change to CSV
Travel_Data <- read_xlsx("Travel_Review.xlsx")

#Initial  Inspection
Travel_Data %>% glimpse()

#Dimensionality
print(paste("Number of observations/rows: ", dim(Travel_Data)[1]))
print(paste("Number of columns/variables: ", dim(Travel_Data)[2]))

#Check for Duplicate Observations
duplicates <- Travel_Data[which(duplicated(Travel_Data)), ]
if ((nrow(duplicates)) == 0) {
    print("No duplicates detected")}


#Show Variables w/ Percentage of Missing Values (Train Set)
print(paste("Percentage Missing Values for Dataset:"))
Travel_miss <- sapply(Travel_Data, function(x){sum(is.na(x))/nrow(Travel_Data) * 100})
Travel_miss[Travel_miss != 0]

#Visualize Missing Training Data
vis_miss(Travel_Data) +
    coord_flip()

#Drop For Now - Single Datapoint don't bother
Travel_Data <- drop_na(Travel_Data)

#---Exploratory Data Analysis---

#Remove ID for now during Analysis
Travel_Data_Quant <- Travel_Data %>% select(-UserID) %>% as.data.frame() 

#Note: Knowing that K-Means & PCA can be sensitive to outliers, we're going 
#to remove all outlier values from our full dataset and work with 2 datasets
#Full Dataset and Another with All Outliers Removed to see the possible benefits

#---Function to Remove all Outliers---
remove_outliers <- function(x) {
    qnt <- quantile(x, probs=c(0.25, 0.75))
    iqr <- IQR(x)
    lower <- qnt[1] - 1.5 * iqr
    upper <- qnt[2] + 1.5 * iqr
    x <- ifelse(x < lower | x > upper, NA, x)
    return(x)
}

Travel_Data_No_Outliers <- Travel_Data %>%
    mutate(across(where(is.numeric), remove_outliers)) %>%
    drop_na()


Travel_Data_No_Outliers_Quant <- Travel_Data_No_Outliers %>% select(-UserID)
#Describe with Summary Statistics
#UserID obviously useless here but useful for later

Travel_Data_Quant %>% describe()

Travel_Data_No_Outliers %>% describe()


#INCREDIBLY WEAK CORRELATIONS ACROSS FULL DATASET, WE MIGHT ACTUALLY
#EXPECT PCA TO PERFORM POORLY ON FULL DATASET

#Correlation Matrix
corrplot::corrplot(cor(Travel_Data_Quant),
                   type = "lower", diag = F, 
                   tl.col = "black", tl.cex = .7,
                   tl.srt = 60)

#Correlation Matrix - Stronger Correlation (PCA may Peform Better)
corrplot::corrplot(cor(Travel_Data_No_Outliers_Quant),
                   type = "lower", diag = F, 
                   tl.col = "black", tl.cex = .7,
                   tl.srt = 60)

#Visualize Histograms of Variables
names <- colnames(Travel_Data_Quant)
for (i in 1:ncol(Travel_Data_Quant)) {
    chart.Histogram(Travel_Data_Quant[,names[i]],
                    xlab = names[i],
                    main = paste("Distribution of", names[i]),
                    border.col = "black",
                    show.outliers = F
    )
}

#Visualize Histograms of Variables with Outliers Removed
names <- colnames(Travel_Data_Quant)
for (i in 1:ncol(Travel_Data_Quant)) {
    chart.Histogram(Travel_Data_Quant[,names[i]],
                    xlab = names[i],
                    main = paste("Distribution of", names[i]),
                    border.col = "black",
                    show.outliers = F
    )
}

#---Principal Components Analysis---

#Extract Principal Components Through SVD
Travel_PCA = prcomp(Travel_Data_Quant, scale. = TRUE)

#Extract Principal Components Through SVD
Travel_PCA_Outlierless = prcomp(Travel_Data_No_Outliers_Quant, scale. = TRUE)


#Variable Contributions to Principal Components
fviz_contrib(Travel_PCA, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(Travel_PCA_Outlierless, choice = "var", axes = 1) # Contributions of variables to PC1

fviz_contrib(Travel_PCA, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(Travel_PCA_Outlierless, choice = "var", axes = 2) # Contributions of variables to PC2

fviz_contrib(Travel_PCA, choice = "var", axes = 3) # Contributions of variables to PC3
fviz_contrib(Travel_PCA_Outlierless, choice = "var", axes = 3) # Contributions of variables to PC3


#Visualize explained variances per component
fviz_eig(Travel_PCA, addlabels=TRUE)+
    labs(x = "Dimensions (Pricipal Components)",
         title = "Scree-Plot for Full Dataset")

#Actually Achieve better Variance for first few Principal Components
fviz_eig(Travel_PCA_Outlierless, addlabels=TRUE)+
    labs(x = "Dimensions (Pricipal Components)",
         title = "Scree-Plot for Dataset without Outliers")
    

fviz_pca_var(Travel_PCA,  col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA Full Dataset") 


fviz_pca_var(Travel_PCA_Outlierless,  col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE,
             title = "Variables - PCA Outliers Removed")


#Show 3d dimensional plot of the full Travel Data
#Hard to make out any spheres but there are some apparent
#clusters, but mostly it appears as one big glob
fig = plot_ly(as.data.frame(Travel_PCA$x),
              x=~PC1,y=~PC2,z=~PC3, size = 1)
fig = fig %>% add_markers()
fig

#Removing the outliers from the full dataset actually 
#seems to slim down the shape from the previous 3d plot
fig = plot_ly(as.data.frame(Travel_PCA_Outlierless$x),
              x=~PC1,y=~PC2,z=~PC3, size = 1)
fig = fig %>% add_markers()
fig

#---K-Means Clustering---

#--Hierarchical---

#---Extra Visualizations---

