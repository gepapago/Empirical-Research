
######################################################################################
#####################################Exercise 09######################################
###################################Cluster analysis###################################
######################################################################################



########################################Recap#########################################

### 1. What is cluster analysis?

#Grouping similar objects/individuals
#e.g. tv broadcaster, brands (high/low price)

#These groups are meant to be internally homogeneous and externally heterogeneous 

#while aiming at a reduction in the number of objects to analyze.

### 2. Data requirements

#Low collinearity between the variables used since they should be real classification 
#dimensions.
#Check for outliers since cluster algorithms are sensitive to extreme values.
#Standardize the data to achieve homogeneous units of measurement (avoid comparing
#apples and pears).
#Data do not have to be metric/non-metric.
#Data do not have to be normally distributed/linearly related.

### 3. Main steps of cluster analyses

#1. Select a proximity measure (distance/similarity measure) for individual observations
#2. Choose a clustering algorithm 
#3. Define the new distance between two clusters
#4. Determine the optimal number of clusters

#3.1 Proximity measures

#They describe the relationship between objects.
#On the basis of these relationships, the individual objects are summarized into groups.
#We have i) similarity measures, ii) distance measures
#Most important are: i) Pearson correlation, ii) City block distance, (squared) Euclidean
#distance.

#3.2/3.3 Choose a cluster algorithm

#Single linkage (nearest neighbor): new distance is smallest individual distance
#Complete linkage (furthest neighbor): new distance is largest individual distance
#Ward: Calculation of new distance is based on formula (see lecture slides)

#3.4 Determine the optimal number of clusters

#Final solution must be interpretable.
#Final solution must be the best one w.r.t. to initial research problem.
#=> Evaluate several solutions and choose the most suitable.
#Elbow criterion for agglomeration coefficients (AC) 



#######################################Exercise########################################


#Import the cars data  
library(haven)
cars <- read_sav("C:/Users/AEW/Downloads/Cars.sav")
View(cars)



# 1. Data requirements => data must be standardized
cardat = scale(cars[,2:10])
row.names(cardat) <- cars$Name



# 2. Calculate distances
#Manual calculation
cars[,2:10] = scale(cars[,2:10])
edman = ((cars[1,2]-cars[2,2])^2+(cars[1,3]-cars[2,3])^2+(cars[1,4]-cars[2,4])^2+
        (cars[1,5]-cars[2,5])^2+(cars[1,6]-cars[2,6])^2+(cars[1,7]-cars[2,7])^2+
        (cars[1,8]-cars[2,8])^2+(cars[1,9]-cars[2,9])^2+(cars[1,10]-cars[2,10])^2)^0.5
edman

#Distance function from R
eucdist = dist(cardat, method = "euclidean") #city block = manhattan
eucdist

#For squared euclidean and other distance measures: use distance() from philentropy
install.packages("philentropy")
library(philentropy)
sqe = distance(cardat, method="squared_euclidean", use.row.names = TRUE)
sqe



# 3. Generate clusters
clus = hclust(eucdist, method="ward.D") #single linkage=single, complete linkage=complete 
clus
plot(clus)

data.frame(clus[2:1])


f <- function(hc){
  data.frame(row.names=paste0("Stage",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE)
}
f(clus)


# 4. Choose optimal number of clusters
NoC = 14:1
plot(NoC,clus$height, type="l")


# 5. Graphical representation of cluster
plot(clus)
rect.hclust(clus,k=2,border="red")


fit = kmeans(cardat, 5,  nstart=20, algorithm = c("Hartigan-Wong"))

library(cluster)
clusplot(cardat, fit$cluster, color = TRUE, shade = TRUE, labels=4, lines=1)


########################################Tasks##########################################

### Now it is your turn. 

##Do the following tasks:

#1. Import the US crime data. You can do so by typing: 
dat = USArrests
#2. Z-standardize the data.
#3. Calculate the city-block distance matrix for the variables murder, assault and rape 
#   (m, a and r).
#4. Perform a cluster analysis using the single linkage method using m,a, and r.
#5. Generate the agglomeration schedule for the cluster analysis. 
#6. Create a graph plotting the agglomeration coefficients sorted from low to high 
#   against the number of clusters.


#The variables you will need are:

#Murder = Number of people arrested for murder per 100,000 inhabitants
#Assault = Number of people arrested for assault per 100,000 inhabitants
#Rape = Number of people arrested for rape per 100,000 inhabitants


#Solutions
#2. 
dat = scale(dat)

#3.
cbd = dist(dat[,c(1,2,4)], method = "manhattan")
cbd

#4.
slc = hclust(cbd, method="single")


#5. 
data.frame(slc[2:1])


f <- function(hc){
  data.frame(row.names=paste0("Stage",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE)
}
f(slc)

#6.
NoC = 49:1
plot(NoC,slc$height, type="l")



#EX09Q1
#What are the z-standardized values in Maine for murder, assault and rape, respectively?

#a 1.24, 0.78, -0.003
#b 1.24, 0.51, 0.07
#c -1.31, -1.05, -1.00
#d -1.31, -1.05, -1.43 (correct)


#EX09Q2
#What is the city block distance between California and Arizona?

#a 0 
#b 1.14
#c 1.45 (correct)
#d 1.07

#EX09Q3
#What of the following statements is true?

#a At stage 10, Illinois and New York are combined to one cluster.
#b At stage 1, Iowa and Vermont are merged to one cluster and appear the next time at 
#  stage 2.
#c Cluster 4 contains Iowa, New Hampshire, Vermont and Wisconsin. (correct)
#d Alaska and Nevada are the most similar states in terms of their city block distance.

#EX09Q4
#What statement about the agglomeration coefficient (AC) curve is true?

#a We observe the highest AC for 49 clusters.
#b The optimal number of clusters is between 20 and 30.
#c The AC for 10 clusters is below 1.0.
#d The decrease in the AC from 1 to 25 clusters is larger than the decrease in the AC from 25 to 50 clusters.(correct)