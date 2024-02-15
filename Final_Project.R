#-----------------------------------
#title: "Unsupervised Learning Final Project"
#subtitle: 'PCA and Clustering'
#authors: "Saxa Group #9"
    #Kassandra Sellers
    #Mabel Lorena Barba
    #Benny Huang
    #Tony Campoverde
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
library(PerformanceAnalytics)

#Read in Travel Data
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

Travel_Data <- drop_na(Travel_Data)
vis_miss(Travel_Data) +
    coord_flip()

#---Exploratory Data Analysis---

#For Working with Quantitative Data, we'll remove 
#the UserID for now and reattach/utilize it later...

Travel_Data_Quant <- Travel_Data %>% select(-UserID) %>% as.data.frame() 

#Describe with Summary Statistics
#UserID obviously useless here but useful for later
Travel_Data_Quant %>% describe()

#Correlation Matrix
corrplot::corrplot(cor(Travel_Data_Quant),
                   type = "lower", diag = F, 
                   tl.col = "black", tl.cex = .7,
                   tl.srt = 60)

#Visualize Histograms of Variables
names <- colnames(Travel_Data_Quant)
for (i in 1:ncol(Travel_Data_Quant)) {
    chart.Histogram(Travel_Data_Quant[,names[i]],
                    xlab = names[i],
                    main = paste("Distribution of", names[i]),
                    border.col = "black"
                    )
}

#---Principal Components Analysis---

#Extract Principal Components Through SVD
Travel_PCA = prcomp(Travel_Data_Quant, scale. = TRUE)


    #Visualize explained variances per component
fviz_eig(Travel_PCA, addlabels=TRUE)+
    labs(x = "Dimensions(Pricipal Components")
    



fviz_pca_var(Travel_PCA,  col.var = "cos2",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE) #User repel to avoid text overlapping (slow if many points)

fviz_pca_var(Travel_PCA, col.var = "contrib",
             gradient.cols = c("red", "darkgreen"),
             repel=TRUE) #User repel to avoid text overlapping (slow if many points)

Travel_PCA_and_Users = bind_cols(UserID = Travel_Data$UserID, Travel_PCA$x)


#---K-Means Clustering---

#---Extra Visualizations---