# Behavioral Patterns of Travelers  
**Determining Significant Factors Driving Tourism**  

---

## 📖 Table of Contents  
1. [Executive Summary and Problem Statement](#executive-summary-and-problem-statement)  
2. [Methodology](#methodology)  
   - [Data Pre-Processing](#data-pre-processing)  
     - [Variable Inspection](#variable-inspection)  
     - [Outlier Evaluation](#outlier-evaluation)  
     - [Correlation](#correlation)  
   - [Distance Measure/Clustering Procedure Selection](#distance-measureclustering-procedure-selection)  
3. [Unsupervised Modeling Approach](#unsupervised-modeling-approach)  
   - [Principal Component Analysis (PCA)](#principal-component-analysis-pca)  
   - [K-Means Clustering](#k-means-clustering)  
4. [Recommendations](#recommendations)  
5. [Conclusion](#conclusion)  
6. [Appendix](#appendix)  

---

## 1️⃣ Executive Summary and Problem Statement  

Our analytics team was approached by **Travelbiz** to analyze data from Google reviews (2018) on travel and tourism. The goal is to develop **targeted travel packages** for European destinations by identifying **distinct traveler groups** through data-driven insights.  

We leveraged **Principal Component Analysis (PCA)** and **K-Means Clustering**, leading to the identification of three traveler segments. These findings enable targeted marketing strategies tailored to each group's **travel preferences** and **ratings of locations and activities**.  

### Key Objectives:  
✔ **Identify and describe distinct travel groups** using PCA and clustering techniques.  
✔ **Develop targeted travel packages** based on unique preferences identified in each group.  

---

## 2️⃣ Methodology  

### 2.1 Data Pre-Processing  

#### 📌 Variable Inspection  
- Dataset consists of **25 continuous variables** across **5,455 user ratings**.  
- Ratings of **zero (0)** indicated that a location was either not visited or not reviewed.  
- To **preserve data integrity**, median imputation was used for missing values.  

#### 📌 Outlier Evaluation  
- Conducted **parallel analyses** with and without outliers.  
- The **full dataset** yielded more **robust and reliable clusters**.  

#### 📌 Correlation Analysis  
- Weak correlations in the full dataset suggested potential challenges in PCA effectiveness.  
- Stronger correlations in the reduced dataset initially suggested better PCA performance.  

### 2.2 Distance Measure / Clustering Procedure Selection  

- Used **Euclidean Distance** for similarity assessment due to **continuous** and **scaled** ratings.  
- **K-Means** was selected over **Hierarchical Clustering** due to **scalability** for large datasets.  

📌 **Key Decision:** Optimal **K (number of clusters) = 3**, ensuring **clear segmentation** of traveler types.  

---

## 3️⃣ Unsupervised Modeling Approach  

### 3.1 Principal Component Analysis (PCA)  

- **PCA helped reduce dimensionality**, addressing the **curse of dimensionality**.  
- Retained **11 principal components**, capturing **75% of the variance**.  
- **Interpretation of Components:**  
  - **PC1**: Related to **restaurants, malls, zoos, bars, gardens, churches, cafes**.  
  - **PC2**: Captured additional relevant travel preferences.  
- **Key Challenge:** Weak variable correlations limited PCA effectiveness.  

📊 **Scree Plot Analysis:**  
- To retain **75% variance**, **11 components** were necessary.  
- Lower than desired variance contribution per component but **still beneficial** for clustering.  

### 3.2 K-Means Clustering  

- **K-Means Clustering** used to **segment travelers** based on their ratings.  
- **Evaluated K = 2 to 5 clusters** using 24 different performance metrics.  
- **Optimal K = 3**, as identified by majority voting and Silhouette analysis.  
- **Silhouette Metric:**  
  - Strong cluster separation, indicating a reliable grouping of travelers.  
- **Validation Check:**  
  - **K-Medoids** clustering produced **minimal label shifts**, confirming K-Means robustness.  

📌 **Final Decision:** **Three distinct traveler groups identified.**  

---

## 4️⃣ Recommendations  

Based on cluster insights, we developed three **targeted travel packages**:  

### ✈ **Travel Package 1 (Cluster 1): Family-Oriented Mid-Size Cities**  
- Ideal for **moderate travelers with children**.  
- Features **restaurants, malls, zoos, and art galleries**.  
- **Marketing Strategy:**  
  - Ads in **daycares, zoos, malls, and family-focused locations**.  

### 🌿 **Travel Package 2 (Cluster 2): Nature & Small-Town Exploration**  
- Ideal for **outdoor enthusiasts** who prefer a **peaceful experience**.  
- Includes **national parks, castles, hikes, small-town cafes**.  
- **Marketing Strategy:**  
  - Ads on **national park websites and outdoor gear catalogs**.  

### 🌆 **Travel Package 3 (Cluster 3): Big City & Luxury Getaways**  
- Ideal for **urban travelers** who enjoy a mix of **entertainment and relaxation**.  
- Features **resorts, beaches, museums, fine dining, and theaters**.  
- **Marketing Strategy:**  
  - Ads in **ticketing platforms, travel directories, and resort listings**.  

---

## 5️⃣ Conclusion  

Using **PCA and K-Means Clustering**, we successfully:  
✅ Identified **three distinct traveler groups**.  
✅ Mapped clusters to **clear travel preferences**.  
✅ Developed **targeted travel packages** to optimize marketing and **enhance traveler satisfaction**.  

📌 **Final Recommendation:**  
- Travelbiz should adopt these **data-driven travel packages** to maximize engagement and revenue.  

---

## 6️⃣ Appendix  

📊 **Visualizations & Supporting Figures**  

- **Figure 4:** Scree Plot - **Variance contributed by each Principal Component**.  
- **Figure 6:** Silhouette Metric - **Shows strong cluster separation**.  
- **Figure 7:** Bar Plot - **Displays highest median ratings per target group**.  

---

## 📌 How to Use This Repository  

### 🔹 Data & Code  
This repository contains:  
- **Processed Data Files** (CSV format).  
- **Python Notebooks** with **PCA & K-Means clustering implementation**.  
- **Visualizations & Reports** summarizing findings.  

### 🔹 Installation  
Ensure you have Python installed with the required libraries:  
```bash
pip install pandas numpy scikit-learn matplotlib seaborn
```
