                                                  #######                #######
                                                  ####### 2nd Assignment #######
                                                  #######                #######

                      
# Installing packages

#install.packages("cluster")
#install.packages("fpc")
#install.packages("clusterSim")
#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("GPArotation")
#install.packages("psych")
#install.packages("nFactors")
#install.packages("scatterplot3d")
#install.packages("mvoutlier")
                                                  
                      
# Call libraries

library(nFactors)
library(psych)
library(GPArotation)
library(factoextra)
library(cluster)
library(ggplot2)
library(clusterSim)
library(fpc)
library(dplyr)
library(scatterplot3d)                                                  
library(mvoutlier)
                                                  

                      
# Read original dataset (csv)

df = read.csv('Co2_europe_V2.0.csv',sep=',', header= TRUE)
df <- df %>% select(-pop_cluster) #remove a column that does not interest us for now


# Read the data and consider only numerical variables and complete observations

df[is.na(df)] = 0       #replace NA with 0
datax<-df[,-1]          #sub-setting df removing first column
rownames(datax)<-df[,1] #change the row names of datax to the values in the first column of df
#View(datax)

#-------------------------------------------------------------------------------
                                        ########                      ######### 
                                        ######## Principal Component  #########
                                        ########      Analysis        #########
                                        #########                     #########


#1) Normalize the data

# PCA on standardized data

datax_normalized <- scale(datax) #normalized dataset

pca = prcomp(datax_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1
pca   
(sum.pca=summary(pca)) #summarize the results



# We can use various plotting functions, such as Scatterplot , biplot, and screeplot, to visualize the results of the PCA


#2) Scatterplot

plot(pca$x[,1], pca$x[,2], type="n", xlab=paste(" PC1  (", (round(100*sum.pca$importance[2,1],digits=1)), " % )"),
     ylab=paste(" PC2  (", (round(100*sum.pca$importance[2,2],digits=1)), " % )"), main="PCA (normalized data)")  #first two PCs
text(pca$x[,1:2], lab=rownames(datax))  #add labels to the points using the row names of data


#3) Biplot

# Biplot preserving column metrics (classical biplot, according to Gabriel)
biplot(pca,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs


#4) Scree Plot

# In PCA, the eigenvalues represent the importance of each PC in explaining the variance in the
# data, and the eigenvectors represent the direction of the PCs.

eigen(cor(datax_normalized)) #returns a list eigenvalues and eigenvectors of a matrix


# Calculate the eigenvalues of the correlation matrix

eigen_vals <- eigen(cor(datax_normalized))$values


# Calculate the explained variance of each PC

expl_var <- eigen_vals / sum(eigen_vals) * 100


qplot(c(1:12), expl_var) +        #PCs as x-axis (1 to 12) and explained variance as y-axis
  geom_col()+                     #add bars to the plot
  geom_line()+                    #add a line to the plot
  xlab("Principal Components") +  #add labels to the x-axis
  ylab("Explained Variance (%)") +    #add labels to the y-axis
  ggtitle("Classical Scree Plot") +         #plot title
  ylim(0,100)                     #set the limits of the y-axis


#5) Cumulative Scree Plot

# We will now apply a more comprehensive view of the explained variance of the PCs. While a classical scree plot
# plots the explained variance of each PC individually, a cumulative scree plot plots the cumulative explained 
# variance, which is the sum of the explained variances of all the PCs up to a certain point. This helps getting
# a better understanding of how much of the total variance in the data is explained by a given number of PCs.


# Calculate the cumulative explained variance

cumvar <- cumsum(eigen_vals) #calculated as the sum of the explained variances of all the PCs up to a certain point


# Calculate the percentage of explained variance
cumvar_perc <- cumvar / sum(eigen_vals) * 100


qplot(c(1:12),  cumvar_perc) +
  geom_col()+
  geom_line()+
  xlab("Principal Components") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)


#6) Correlation between the variables and the PCs


# Extract the loadings and the PCs

loadings <- pca$rotation
pcs <- pca$x

# Compute the correlation between the variables and the PCs
correlation <- cor(datax_normalized, pcs); correlation


# Immediately by the scatter plot we can see that there is an observation (Russia) that has high scores in both
# principal components and is isolated, camouflaging other patterns/ relationships that may be happening.

# If an observation has big scores on both PCs, it means that it has a significant contribution to the variance
# in both of the first two principal components. This may indicate that the variables that contributed most to
# the observed scores are correlated or have a strong relationship with each other. 

# This fact is confirmed in all the observations that follow. Therefore, we consider this point an outlier and
# will repeat the visualizations without it to see what happens.


                                        ####### Analysis without Russia #######

# Read dataset (csv)

df1 = read.csv('Co2_europe_V2.0_noRus.csv',sep=',', header= TRUE)
df1 <- df1 %>% select(-pop_cluster) #remove a column that does not interest us for now


# Read the data and consider only numerical variables and complete observations

df1[is.na(df1)] = 0        #replace NA with 0
datax1<-df1[,-1]           #sub-setting df1 removing first column
rownames(datax1)<-df1[,1]  #change the row names of datax1 to the values in the first column of df1
#View(datax1)


#1) Normalize our data

# PCA on standardized data

datax1_normalized <- scale(datax1) #normalized dataset

pca1 = prcomp(datax1_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1
pca1   
(sum.pca1=summary(pca1)) #summarize the results



# We can use various plotting functions, such as Scatterplot , biplot, and screeplot, to visualize the results of the PCA


#2) Scatterplot

plot(pca1$x[,1], pca1$x[,2], type="n", xlab=paste(" PC1  (", (round(100*sum.pca1$importance[2,1],digits=1)), " % )"),
     ylab=paste(" PC2  (", (round(100*sum.pca1$importance[2,2],digits=1)), " % )"), main="PCA (normalized data without RUS)")  #first two PCs
text(pca1$x[,1:2], lab=rownames(datax1))  #add labels to the points using the row names of data


#3) Biplot

# Biplot preserving column metrics (classical biplot, according to Gabriel)

biplot(pca1,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca1$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca1$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs


#4) Cumulative Scree Plot

# Calculate the eigenvalues of the correlation matrix

eigen_vals <- eigen(cor(datax1_normalized))$values


# Calculate the cumulative explained variance

cumvar <- cumsum(eigen_vals) #calculated as the sum of the explained variances of all the PCs up to a certain point


# Calculate the percentage of explained variance
cumvar_perc <- cumvar / sum(eigen_vals) * 100


qplot(c(1:12),  cumvar_perc) +
  geom_col()+
  geom_line()+
  xlab("Principal Components") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)


#5) Correlation between the variables and the PCs

# Extract the loadings and the PCs

loadings <- pca1$rotation
pcs <- pca1$x

# Compute the correlation between the variables and the PCs
correlation <- cor(datax1_normalized, pcs); correlation


# For the same reasons we will remove, in addition to Russia,
# Germany (DEU) and see what the effects are on our visualizations


                            ####### Analysis without Russia and Germany #######


# Read data set (csv)

df2 = read.csv('Co2_europe_V2.0_noGer.csv',sep=',', header= TRUE)

# Read the data and consider only numerical variables and complete observations

df2[is.na(df2)] = 0        #replace NA with 0
datax2<-df2[,-1]           #sub-setting df2 removing first column
rownames(datax2)<-df2[,1]  #change the row names of datax2 to the values in the first column of df2
#View(datax2)


#1) Normalize our data

# PCA on standardized data

datax2_normalized <- scale(datax2) #normalized dataset

pca2 = prcomp(datax2_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1
pca2   
(sum.pca2=summary(pca2)) #summarize the results




# We can use various plotting functions, such as Scatterplot , biplot, and screeplot, to visualize the results of the PCA


#2) Scatterplot

plot(pca2$x[,1], pca2$x[,2], type="n", xlab=paste(" PC1  (", (round(100*sum.pca2$importance[2,1],digits=1)), " % )"),
     ylab=paste(" PC2  (", (round(100*sum.pca2$importance[2,2],digits=1)), " % )"), main="PCA (normalized data without RUS, DEU)")  #first two PCs
text(pca2$x[,1:2], lab=rownames(datax2))  #add labels to the points using the row names of data


#3) Biplot

# Biplot preserving column metrics (classical biplot, according to Gabriel)
biplot(pca2,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca2$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca2$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs


#4) Cumulative Scree Plot

# Calculate the eigenvalues of the correlation matrix

eigen_vals <- eigen(cor(datax2_normalized))$values

# Calculate the cumulative explained variance

cumvar <- cumsum(eigen_vals) #calculated as the sum of the explained variances of all the PCs up to a certain point


# Calculate the percentage of explained variance
cumvar_perc <- cumvar / sum(eigen_vals) * 100


qplot(c(1:12),  cumvar_perc) +
  geom_col()+
  geom_line()+
  xlab("Principal Components") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)


#5) Correlation between the variables and the PCs


# Extract the loadings and the PCs

loadings <- pca2$rotation
pcs <- pca2$x

# Compute the correlation between the variables and the PCs
correlation <- cor(datax2_normalized, pcs); correlation

# From the scatter plot we can see that we still have a group with a strong relationship
# and cohesion, the group formed by France, Italy and the UK (FRA, ITA and GBR).
# Again, this is also confirmed by the other visualizations. Let's try to remove this
# group and see what happens.


                    ####### Analysis without RUS, DEU, FRA, ITA and GBR #######

# Read dataset (csv)

df3 = read.csv('Co2_europe_V2.0_noUK.csv',sep=',', header= TRUE)


# Read the data and consider only numerical variables and complete observations

df3[is.na(df3)] = 0        #replace NA with 0
datax3<-df3[,-1]           #sub-setting df3 removing first column
rownames(datax3)<-df3[,1]  #change the row names of datax3 to the values in the first column of df3
#View(datax3)


#1) Normalize our data

# PCA on standardized data

datax3_normalized <- scale(datax3) #normalized dataset


pca3 = prcomp(datax3_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1
pca3   
(sum.pca3=summary(pca3)) #summarize the results



# We can use various plotting functions, such as Scatterplot , biplot, and screeplot, to visualize the results of the PCA


#2) Scatterplot

plot(pca3$x[,1], pca3$x[,2], type="n", xlab=paste(" PC1  (", (round(100*sum.pca3$importance[2,1],digits=1)), " % )"),
     ylab=paste(" PC2  (", (round(100*sum.pca3$importance[2,2],digits=1)), " % )"), main="PCA (normalized data without RUS, DEU, FRA, ITA, GBR)")  #first two PCs
text(pca3$x[,1:2], lab=rownames(datax3))  #add labels to the points using the row names of data


#3) Biplot

# Biplot preserving column metrics (classical biplot, according to Gabriel)
biplot(pca3,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca3$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca3$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs


#4) Cumulative Scree Plot

# Calculate the eigenvalues of the correlation matrix

eigen_vals <- eigen(cor(datax3_normalized))$values


# Calculate the cumulative explained variance

cumvar <- cumsum(eigen_vals) #calculated as the sum of the explained variances of all the PCs up to a certain point


# Calculate the percentage of explained variance
cumvar_perc <- cumvar / sum(eigen_vals) * 100


qplot(c(1:12),  cumvar_perc) +
  geom_col()+
  geom_line()+
  xlab("Principal Components") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)


#5) Correlation between the variables and the PCs

# Extract the loadings and the PCs

loadings <- pca3$rotation
pcs <- pca3$x

# Compute the correlation between the variables and the PCs
correlation <- cor(datax3_normalized, pcs); correlation

# There still seems to us to be a small group of countries with a strong
# cohesion that may not be allowing us to do the analysis of the remaining points.
# This group is made up of Australia and Ukraine (AUS and UKR). 


              ####### Analysis without RUS, DEU, FRA, ITA, GBR, AUS, UKR #######


# Read dataset (csv)
df4 = read.csv('Co2_europe_V2.0_noUKR.csv',sep=',', header= TRUE)


# Read the data and consider only numerical variables and complete observations

df4[is.na(df4)] = 0        #replace NA with 0
datax4<-df4[,-1]           #sub-setting df4 removing first column
rownames(datax4)<-df4[,1]  #change the row names of datax4 to the values in the first column of df4
#View(datax4)


#1) Normalize our data

# PCA on standardized data

datax4_normalized <- scale(datax4) #normalized dataset


pca4 = prcomp(datax4_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1
pca4   
(sum.pca4=summary(pca4)) #summarize the results




# We can use various plotting functions, such as Scatterplot , biplot, and screeplot, to visualize the results of the PCA


#2) Scatterplot


plot(pca4$x[,1], pca4$x[,2], type="n", xlab=paste(" PC1  (", (round(100*sum.pca4$importance[2,1],digits=1)), " % )"),
     ylab=paste(" PC2  (", (round(100*sum.pca4$importance[2,2],digits=1)), " % )"), main="PCA (normalized data without RUS, DEU, FRA, ITA, GBR, UKR, AUS)")  #first two PCs
text(pca4$x[,1:2], lab=rownames(datax4))  #add labels to the points using the row names of data


#3) Biplot

# Biplot preserving column metrics (classical biplot, according to Gabriel)
biplot(pca4,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca4$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca4$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs


#4) Cumulative Scree Plot

# Calculate the eigenvalues of the correlation matrix

eigen_vals <- eigen(cor(datax4_normalized))$values


# Calculate the cumulative explained variance

cumvar <- cumsum(eigen_vals) #calculated as the sum of the explained variances of all the PCs up to a certain point


# Calculate the percentage of explained variance

cumvar_perc <- cumvar / sum(eigen_vals) * 100


qplot(c(1:12),  cumvar_perc) +
  geom_col()+
  geom_line()+
  xlab("Principal Components") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)


#5) Correlation between the variables and the PCs

# Extract the loadings and the PCs

loadings <- pca4$rotation
pcs <- pca4$x

# Compute the correlation between the variables and the PCs
correlation <- cor(datax4_normalized, pcs); correlation

# Finally, for the same reasons as in the previous cases, let's take TUR, POL
# and ESP and see what we get.


####### Analysis without RUS, DEU, FRA, ITA, GBR, AUS, UKR, ESP, TUR, POL #######
######  Final PCA ###### 

# Read dataset (csv)
df5 = read.csv('Co2_europe_V2.0_noTUR.csv',sep=',', header= TRUE)


# Read the data and consider only numerical variables and complete observations

df5[is.na(df5)] = 0        #replace NA with 0
datax5<-df5[,-1]           #sub-setting df5 removing first column
rownames(datax5)<-df5[,1]  #change the row names of datax5 to the values in the first column of df5
View(datax5)


#1) Normalize our data

# PCA on standardized data

datax5_normalized <- scale(datax5) #normalized dataset

pca5 = prcomp(datax5_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1
pca5   
(sum.pca5=summary(pca5)) #summarize the results




# We can use various plotting functions, such as Scatterplot , biplot, and screeplot, to visualize the results of the PCA


#2) Scatterplot

plot(pca5$x[,1], pca5$x[,2], type="n", xlab=paste(" PC1  (", (round(100*sum.pca5$importance[2,1],digits=1)), " % )"),
     ylab=paste(" PC2  (", (round(100*sum.pca5$importance[2,2],digits=1)), " % )"), main="PCA (normalized data without RUS, DEU, FRA, ITA, GBR, UKR, AUS, TUR, ESP, POL)")  #first two PCs
text(pca5$x[,1:2], lab=rownames(datax5))  #add labels to the points using the row names of data


#3) Biplot

# Biplot preserving column metrics (classical biplot, according to Gabriel)
biplot(pca5,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca5$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca5$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs


#4) Cumulative Scree Plot

# Calculate the eigenvalues of the correlation matrix

eigen_vals <- eigen(cor(datax5_normalized))$values


# Calculate the cumulative explained variance

cumvar <- cumsum(eigen_vals) #calculated as the sum of the explained variances of all the PCs up to a certain point


# Calculate the percentage of explained variance
cumvar_perc <- cumvar / sum(eigen_vals) * 100


qplot(c(1:12),  cumvar_perc) +
  geom_col()+
  geom_line()+
  xlab("Principal Components") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)


#5) Correlation between the variables and the PCs


# Extract the loadings and the PCs

loadings <- pca5$rotation
pcs <- pca5$x

# Compute the correlation between the variables and the PCs
correlation <- cor(datax5_normalized, pcs)


# It is generally recommended to retain the number of PCs that account for at
# least 70-80% of the variance in the data. In this case, the first 2 PCs 
# explain about 71.9% of the variance, and the first 3 PCs explain about 80.5%
# of the variance. We can verify this on the cumulative scree plot.
# It is also important to consider the interpretability of the PCs and how well
# they capture the underlying structure of the data. Retaining 2 PCs results in 
# a more satisfactory level of interpretability and captures the relevant 
# structure of the data.
#-------------------------------------------------------------------------------
                                          ########                    ######### 
                                          ######## Factor Analysis    #########
                                          ########                    #########

# Evaluate if the data is adequate to be described by some factorial model
# For this, we compute the Kaiser-Meyer-Olkin (KMO) measure (global and individual)

# It is generally recommended to conduct the KMO analysis on raw data, 
# rather than on normalized data. This is because the KMO statistic is based on 
# the correlations between variables, and normalization can alter the relationships
# between variables.


# Calculate KMO (global and individual)

global_kmo <- KMO(datax5)
print(global_kmo)


# Select variables comparing with reference table provided by Kaiser (1970)

# We can see that the third feature has the lowest KMO measure, less than 0.5.
# Therefore, we will remove it so that our data is indeed suitable for
# the application of factor analysis  

global_kmo <- KMO(datax5[,-c(3)])
print(global_kmo)

# So later when we apply the factor analysis model we should drop the third 
#feature/ column


# Normalize the data

# We will already drop the columns we don't want to apply in this model when 
# normalizing the data. In addition to column 3, we will also drop the 4th 
# column. We drop it because, when we applied the AF model we saw that the
# variable number 4, flaring_co2, was the worst one in the model.
# Since the 3rd column is the worst one in the KMO test, and the 4th is the
# worst in the factorial analysis, we decided to drop both of them to get better
# results in both models

datax5_normalized <- scale(datax5)[,-c(3,4)] 
datax5_normalized


# Bartlett's test

cortest.bartlett(cor(datax5_normalized),n=nrow(datax5_normalized))

# The output of the Bartlett test suggests that the correlation matrix of the data 
# is not an identity matrix, since the p-value is very small (less than the 
# significance level of 0.05). This means that the variables in the data are 
# correlated and factor analysis may be a suitable method for exploring these 
# correlations.



# Now, we will select the number of factors 


# Run parallel analysis to determine the number of factors

# By doing the nScree function, we get an analysis of the number of component or
# factors to retain in an exploratory factor analysis. The
# function also returns information about the number of components/factors to retain
# with the Kaiser rule and the parallel analysis

rMatrix<-cov(datax5_normalized)


parallelAnalysis <- nScree(rMatrix)
parallelAnalysis

# Different solutions are given. The classical ones are the Kaiser rule, the parallel
# analysis, and the usual scree test. Non graphical solutions to the Cattell
# subjective scree test are also proposed: an acceleration factor (af) and the
# optimal coordinates index oc. The acceleration factor indicates where the elbow
# of the scree plot appears. It corresponds to the acceleration of the curve, i.e.
# the second derivative. The optimal coordinates are the extrapolated coordinates of
# the previous eigenvalue that allow the observed eigenvalue to go beyond this
# extrapolation. 


plotnScree(parallelAnalysis) # We should choose 2 factors

# This plot shows the eigenvalues of the observed variables on the y-axis and the
# number of factors on the x-axis. 

# Therefore, we can also confirm from the graph that we should retain 2 factors
# for our factor analysis 


# Now, we will construct several factorial models and select one (with more 
# interpretable factors). We will leave out of commentary only the ones we 
# actually used


## Via PCA, no rotation ##

#AF<-principal(rMatrix, nfactors = 2,residuals = TRUE, rotate = "none", covar=TRUE)
#AF
#print(AF,cut=0.5,digits=3)


## Via PCA, orthogonal rotation: varimax ##

#AF<-principal(rMatrix, nfactors = 2,residuals = TRUE, rotate = "varimax", covar=TRUE)
#AF
#print(AF,cut=0.5,digits=3)


## Via PCA, non-orthogonal rotation: oblimin ##
#AF<-principal(rMatrix, nfactors = 2,residuals = TRUE, rotate = "oblimin", covar=TRUE)
#AF
#print(AF,cut=0.3,digits=3)


## Via PCA, orthogonal rotation: quartimax ##
AF<-principal(rMatrix, nfactors = 2,residuals = TRUE, rotate = "quartimax", covar=TRUE)
AF
print(AF,cut=0.3,digits=3)

# We get a warning running the first line, but we weren't able to find what is 
# happening. However the warning says that R had done smoothing of the data
# and an ouput is given

# Quartimax rotation was the best rotation for our data.


# Percentage of explained variance

# In general, the explained variance is a measures of how well a statistical
# model or set of factors can describe or explain the variance in a dataset.
# The higher the explained variance, the better the model or factors are at
# explaining the variance in the data.


# Calculate the eigenvalues of the PCs

# Extract eigenvalues from AF object
eigenvalues <- AF$values

# Calculate the total variance explained by all the PCs
total_variance_explained <- sum(eigenvalues)

# Calculate the percentage of explained variability for each PC
percentage_explained_variability <- eigenvalues / total_variance_explained * 100

# Calculate the cumulative percentage of explained variability for all the PCs
cumulative_percentage_explained_variability <- cumsum(percentage_explained_variability)

# Print the results
print(percentage_explained_variability)
print(cumulative_percentage_explained_variability)

# We can see that the first factor explains 64.47% of the variance in the data, the 
# second factor explains an additional 13.59% of the variance, and so on. The 
# cumulative percentage of explained variance tells us the total percentage of 
# variance explained by all of the factors up to a certain point. For example, the
# first and second factors combined explain 78.06% of the variance in the data.


# Visualizing the percentage of cumulative explained variance using a bar plot


# Plot percentage of explained variance

qplot(c(1:10),  cumulative_percentage_explained_variability) + # from 1:10 because we dropped 2 features
  geom_col()+
  geom_line()+
  xlab("Factors") +
  ylab("Explained Variance (%)") +
  ggtitle("Cumulative Explained Variance") +
  ylim(0, 100)

#This will create a bar plot with the factors on the x-axis and the percentage of 
# cumulative explained variance on the y-axis.


# Via Maximum Likelihood (using the same values for rotation as via PCA)


# To be able to apply the maximum likelihood method, we have to verify that our 
# data follows a multivariate normal distribution. To do so, we can make a qqplot (in an exploratory
# way, to check if each variable has a normal distribution). Make chi-plot and
# Mahalanobis distances is also a good idea. We need to check if the points are
# close to a straight line. If they are far apart they do not have a normal
# distribution.


# Prove Normal Distribution using qq-plot
for (i in 1:10){
  qqnorm(datax5_normalized[1:33, i], pch = 1, frame = FALSE)
  qqline(datax5_normalized[1:33, i], col = "steelblue", lwd = 2)
}

# It is suspected that the normal distribution does not fit the data, since there
# are points that are relatively far from the straight line.


# Mahalanobis Distance
mahalanobis(datax5_normalized[1:33, 1:10], colMeans(datax5_normalized[1:33, 1:10]), cov(datax5_normalized[1:33, 1:10]))

#Chi-Square 
for (i in 1:10){
  chisq.plot(x = matrix(c(datax5_normalized[1:33, i])))
}

# The chi-squared plot is similar to the qq-plot of the normal distribution,
# but now thought of as chi squared. The points should again fit on a straight 
# line. This is not the case. Therefore, Mahalanobis distances do not fit a
# chi-squared distribution, which is what we would expect if the data had a 
# multivariate normal distribution.

# Conclusion: the data do not come from a multivariate normal distribution.
# Therefore, it is not advisable to use factor analysis via maximum likelihood.


# Via  Maximum Likelihood, orthogonal rotation: quartimax
AFmv=factanal(datax5_normalized, factors=2, scores = c("Bartlett"), rotation = "quartimax"); AFmv
print(AFmv$loadings,cut=0.35,digits=3)

# Even suspecting that the multivariate normality assumption is not valid, we
# performed and found that the resulting factorial structure, via maximum 
# likelihood, is equal to the one obtained when the factors were extracted using
# PCA.

# This consistency of results suggests that maximum likelihood may be robust to the
# normality assumption, and on the other hand, it demonstrates that the factorial
# structure found is strong enough, since it does not change with other procedures 
# (even if these fail in some assumptions).
 

# Now, we calculate the proportion of residuals that are small in absolute value,
# which may be an indication of the amount if our model fits our data well or not.


# via PCA model
mat=AF$residual
mat
diag(mat)= rep(1000,dim(AF$residual)[1])   # Substitute the diagonal of mat by a high value (1000).
abs(mat)
a=length(which( abs(mat) <0.05)) # number of correlation values (in absolute value), outside the diagonal, lower than 0.05
a=a/2  
b=(dim(mat)[1])*(dim(mat)[1]-1)/2
a/b  # Proportion of the values in the residual matrix lower than 0.05

# We extracted the residuals from the results of the PCA. The residuals are the
# differences between the observed values and the fitted values of the data.

# Next, the diagonal elements of the mat object are replaced with the value 1000.
# This effectively sets the residuals for each variable to a high value, since the
# residuals for a single variable are always on the diagonal of the matrix.

# The absolute values of the residuals are then calculated using the abs function
# This is done because the residuals are typically expressed as correlations, which
# can be either positive or negative.

# The number of absolute residuals that are less than 0.05 is then calculated
# using the which function and the length function. The result is divided by 2 to
# account for the fact that the residual matrix is symmetric and contains redundant
# information.

# Finally, the proportion of absolute residuals that are less than 0.05 is
# calculated by dividing the number of such residuals by the total number of
# possible residuals in the matrix.


# This suggests that about 60% of the values in the residual matrix are lower
# than 0.05, which may indicate that the model is fitting the data well.


# Estimate the scores of each factor for each subject in the data

scores <- factor.scores(datax5_normalized, f=AF, method = c("Bartlett")) #factor scores and variable weights
scores <- scores$scores; scores

# The matrix with the factor scores can be used in subsequent analyses (Cluster
# analysis, scatterplot, etc,...)
# The main idea is extract information from multivariate data.


# PCA without the columns we dropped in FA (Biplot)

pca5 = prcomp(datax5_normalized, scale=TRUE) #the data will be standardized by centering and scaling each variable to have mean 0 and standard deviation 1

biplot(pca5,choices=1:2, pch=15, cex=0.8,cex.axis=0.7, xlab=paste(" PC1  (", (round(100*sum.pca5$importance[2,1],digits=1)), " % )"),
       ylab=paste(" PC2  (", (round(100*sum.pca5$importance[2,2],digits=1)), " % )"),var.axes=TRUE, scale=1,  main="Classic Biplot") #first two PCs

# ------------------------------------------------------------------------------


                                                  ########            ######### 
                                                  ######## Clustering #########
                                                  ########            #########

# We will start by clustering all our data 

# Loading data
clusteringdf = read.csv('clustering.csv',sep=',', header= TRUE)


# Read the data and consider only numerical variables and complete observations

clusteringdf[is.na(clusteringdf)] = 0       #replace NA with 0
datax6<-clusteringdf[,-1]                   #sub-setting df removing first column
rownames(datax6)<-clusteringdf[,1]          #change the row names of datax to the values in the first column of df
View(datax6)


# Normalize the data and remove and remove last column because it is one of the 
# clusters we will use to compare and compute the ARI

datax6_normalized <- scale(datax6[,-c(13)]) #normalized dataset


# We will apply hierarchical clustering and non-hierarquical clustering and compare
# the results to see which one works better for this particular case


                                          ######################################
                                          # Non-Hierarchical Clustering
                                          ######################################

# We will use K-means clustering

set.seed(1)  # to make sure we always get the same result

# Optimal cluster number

# For this, we will check for the average silhouette width. We should look for the
# number of clusters that results in the highest silhouette coefficients. The 
# x-axis of the plot indicates the number of clusters, and the y-axis indicates
# the silhouette coefficient.

# A high silhouette coefficient indicates that the data point is well-assigned to
# its own cluster, while a low coefficient indicates that the data point is 
# poorly-assigned or overlapping with other clusters.

fviz_nbclust(datax6_normalized[1:33,1:12], kmeans, method = "silhouette")

# Here, visualize the silhouette coefficient for different numbers of clusters, using K-means
# clustering as the clustering method.
# It is recommended to use 2 clusters.

# Perform k-means
data2G<-kmeans(datax6_normalized[1:33,1:12], 2, iter.max = 10)
data2G
vcol <- c("#FF6B7B","#2CC2FF")


# Visualize the k-means cluster

a=prcomp(datax6_normalized[1:33,1:12])

plot(a$x[,1:2], col = vcol[data2G$cluster], pch=19, main="kmeans, 2 groups")

#### Cluster ####

fviz_cluster(data2G, data = datax6_normalized[1:33,1:12])



                                             ###################################
                                             #  Hierarchical Clustering:
                                             ###################################


Mdistance<-dist(datax6_normalized, method="euclidian")

hc = hclust(Mdistance, method="ward.D") # perform hierarchical clustering on the given dataset
# the "method" argument specifies the distance measure to use


#Optimal number of cluster 

fviz_nbclust(datax6_normalized, hcut, method = "silhouette") # we should choose k=2


# Plot dendrogram

plot(hc, hang=-1, main="Euclidean distance, and nearest neighbor") #plot the resulting hierarchical cluster tree

rect.hclust(hc,k=2)  # draw rectangles around the clusters in the tree plot, and the k argument specifies the number of clusters to create

cutree(hc, 2) # cut the tree into a specified number of clusters. In this case, the tree is being cut into 2 clusters

# The y-axis represents the distance between the different clusters, and the x-axis
# represents the different observations or data points being clustered. The 
# observations are typically represented by leaves on the dendrogram, and the
# clusters are represented by branches. The height of the branches on the y-axis
# represents the distance between the clusters, and the length of the branches
# on the x-axis represents the number of observations in each cluster.


#--------------
# Lets perform a cluster analysis of the factor scores

                                            ######################################
                                            # Non-Hierarchical Clustering
                                            ######################################

# We start by performing cluster analysis to the factor scores we got from 
# factor analysis 

# We will use K-means clustering

set.seed(1)  # to make sure we always get the same result

# Optimal cluster number

# For this, we will check for the average silhouette width. We should look for the
# number of clusters that results in the highest silhouette coefficients. The 
# x-axis of the plot indicates the number of clusters, and the y-axis indicates
# the silhouette coefficient.

# A high silhouette coefficient indicates that the data point is well-assigned to
# its own cluster, while a low coefficient indicates that the data point is 
# poorly-assigned or overlapping with other clusters.

fviz_nbclust(datax5_normalized[1:33,1:10], kmeans, method = "silhouette")

# Here, visualize the silhouette coefficient for different numbers of clusters, using K-means
# clustering as the clustering method. It is recommended to use 2 clusters/centers.

# Performing k-means
scoresk <- as.data.frame(scores)
kmeans_results <- kmeans(scoresk, centers=2)


# Add cluster assignments to the scores data frame
scoresk$cluster <- kmeans_results$cluster


# View the resulting clusters
scoresk


# Create the scatterplots

# With the first two factors
ggplot(scoresk, aes(x=RC1, y=RC2, color=factor(cluster))) +
  geom_point() +
  ggtitle("Factor Scores and Cluster Assignments") 


                                            ######################################
                                            # Hierarchical Clustering
                                            ######################################


# Extract factor scores, again
scores <- factor.scores(datax5_normalized, f=AF, method = c("Bartlett")) #factor scores and variable weights
scores <- scores$scores

# Calculate distance matrix
distances <- dist(scores, method="euclidean")

# The "method" argument specifies the distance measure to use. In this case, 
# the Euclidean distance measure is being used.

# Perform hierarchical clustering
hc <- hclust(distances, method="ward.D") # perform hierarchical clustering on the given dataset

# Best number of clusters

fviz_nbclust(datax5_normalized, hcut, method = "silhouette") #we should choose k=2


# Plot dendrogram

plot(hc, hang=-1, main="Euclidean distance, and nearest neighbor")

rect.hclust(hc,k=2)  # draw rectangles around the clusters in the tree plot, and the k argument specifies the number of clusters to create

cutree(hc, k=2) # cut the tree into a specified number of clusters. In this case, the tree is being cut into 3 clusters

# The y-axis represents the distance between the different clusters, and the x-axis
# represents the different observations or data points being clustered. The 
# observations are typically represented by leaves on the dendrogram, and the
# clusters are represented by branches. The height of the branches on the y-axis
# represents the distance between the clusters, and the length of the branches
# on the x-axis represents the number of observations in each cluster.



# Now that we did the cluster analysis of the factors' scores we will evaluate
# the clustering we did before. We will do 2 different comparisons:

## 1- With the cluster analysis of the factor scores from factor analysis
## 2- With the cluster analysis of population


####################################################################
## Evaluation 1.1 - Non-hierarchical Clustering Vs. Factors' scores ##
####################################################################

# Calculates various statistics that can be used to evaluate the quality of a clustering solution
res=cluster.stats(dist(datax6_normalized[1:33,1:10]),clustering = data2G$cluster, alt.clustering = scoresk$cluster)
res

# Computing ARI and silhouette width
resultadoIndices=matrix(c(res$corrected.rand,res$avg.silwidth),byrow=TRUE,1,2)
colnames(resultadoIndices)=c("ARI","avg.Silhw")
round(resultadoIndices, 3)

# Visualize the silhouette (from cluster obtained by k-means)
D <- dist(datax6_normalized[1:33,1:10])
plot(silhouette(data2G$cluster, D),col= c("blue", "purple"))#, "yellow", "orange"

# The ARI (Adjusted Rand Index) is a measure of the agreement between two 
# clusterings. It ranges from 0 to 1, with higher values indicating greater 
# agreement. The average silhouette width is a measure of how well-defined the 
# clusters are. It ranges from -1 to 1, with higher values indicating more well
# -defined clusters.
# In this case, the ARI value of 0.312. We got a value of 0.471 for the Silhouette,
# which indicates that the observations within the clusters are somewhat similar
# to each other and somewhat different from the observations in other clusters,
# but it's is far from good. We can also see some negative values in the silhouette
#plot, which means that these values probably don't belong to this cluster

####################################################################
## Evaluation 1.2 - Hierarchical Clustering Vs. Factors' scores ##
####################################################################


data2G2<-hcut(datax6_normalized[1:33,1:12], 2, iter.max = 10)
data2G2

# Calculates various statistics that can be used to evaluate the quality of a clustering solution
res=cluster.stats(dist(datax6_normalized[1:33,1:12]),clustering = data2G2$cluster, alt.clustering = scoresk$cluster)
res

# Computing ARI and silhouette width
resultadoIndices=matrix(c(res$corrected.rand,res$avg.silwidth),byrow=TRUE,1,2)
colnames(resultadoIndices)=c("ARI","avg.Silhw")
round(resultadoIndices, 3)

# Visualize the silhouette (from cluster obtained by hierarchical clustering)
D <- dist(datax6_normalized[1:33,1:12])
plot(silhouette(data2G$cluster, D),col= c("blue", "purple"))#, "yellow", "orange"


# We got a worse ARI that when using the k-means (0.185) and the average silhouette 
# width is 0.505, which is better. 


##########################################################################
## Evaluation 2.1 - Non-Hierarchical Clustering Vs. Population Clusters ##
##########################################################################

# Calculates various statistics that can be used to evaluate the quality of a clustering solution
res=cluster.stats(dist(datax6_normalized[1:33,1:12]),clustering = data2G$cluster, alt.clustering = datax6$pop_cluster + 1)
res

# Computing ARI and silhouette widt
resultadoIndices=matrix(c(res$corrected.rand,res$avg.silwidth),byrow=TRUE,1,2)
colnames(resultadoIndices)=c("ARI","avg.Silhw")
round(resultadoIndices, 3)

# Visualize the silhouette (from cluster obtained by k-means)
D <- dist(datax6_normalized[1:33,1:12])
plot(silhouette(data2G$cluster, D),col= c("blue", "purple"))#, "yellow", "orange"

# In this case, we got an ARI value of 0.383 and an avg.Silhw value of 0.459.

##########################################################################
## Evaluation 2.2 - Hierarchical Clustering Vs. Population Clusters ##
##########################################################################

res=cluster.stats(dist(datax6_normalized[1:33,1:12]),clustering = data2G2$cluster, alt.clustering = datax6$pop_cluster + 1)
res

# Computing ARI and silhouette width
resultadoIndices=matrix(c(res$corrected.rand,res$avg.silwidth),byrow=TRUE,1,2)
colnames(resultadoIndices)=c("ARI","avg.Silhw")
round(resultadoIndices, 3)

# Visualize the silhouette (from cluster obtained by hierarchical clustering)
D <- dist(datax6_normalized[1:33,1:12])
plot(silhouette(data2G$cluster, D),col= c("blue", "purple"))#, "yellow", "orange"


# We got an ARI of 0.237 and an average silhouette width of 0.505. 
