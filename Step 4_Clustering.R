library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(caret)
library(forecast)
library(corrplot)
library(visualize)
library(FNN)
library(e1071)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(sf)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(writexl)
library(mice)
library(Hmisc)

# Step 4: Clustering

alpha <- read.csv("alpha.csv") #Load clean Copacabana rentals dataset
alpha$id <- 1:nrow(alpha)

range(alpha$accommodates)

CopaCluster <- subset(alpha, select = c(id, 
                                        accommodates, 
                                        guests_included, 
                                        bathrooms, 
                                        bedrooms, 
                                        beds, 
                                        price, 
                                        minimum_nights, 
                                        security_deposit, 
                                        cleaning_fee, 
                                        extra_people))

CopaCluster$Max.Cost <- (CopaCluster$price*CopaCluster$minimum_nights)+ 
  ((CopaCluster$accommodates-CopaCluster$guests_included)*CopaCluster$extra_people)+
  CopaCluster$security_deposit+
  CopaCluster$cleaning_fee #Cost if maxing out guests accommodated into custom variable

fivenum(CopaCluster$accommodates)
fivenum(CopaCluster$beds) #So general rule is beds*2=accommodates

CopaCluster$Max.Guests <- CopaCluster$accommodates #Just renaming but this is our max guest custom variable 

row.names(CopaCluster) <- CopaCluster[,1] #Overwrites row num with last name
CopaCluster <- CopaCluster[,-1] #Drops row containing last name

CopaCluster.norm <- sapply(CopaCluster, scale) #Normalizes to z-scores and add to new df
row.names(CopaCluster.norm) <- row.names(CopaCluster) #Adds the last names back to df by overwrite row num

NewClusterScores <- data.frame(subset(CopaCluster.norm, select = c(Max.Guests,Max.Cost))) #new df with custom variables and last names over row num
class(NewClusterScores)

set.seed(110)
Copakm <- kmeans(NewClusterScores, 4, nstart=15) #kmeans(dataframe, count of centers, random sets chosen)
Copakm$cluster #Show which rows belong to which clusters
Copakm$centers #Matrix showing centers by cluster and variable
dist(Copakm$centers) #Distances between cluster centers

NewClusterScores <- cbind(NewClusterScores, Copakm$cluster) #Adds cluster assignments to df containing custom variables

class(NewClusterScores) #df proceed to ggplot

colnames(NewClusterScores)[colnames(NewClusterScores) == 'Copakm$cluster'] <- 'cluster' #Renames cluster column so more comprehensive

ggplot(data=NewClusterScores, aes(x=Max.Guests, y=Max.Cost, color=cluster)) + geom_point(size=3)

NewClusterScores$cluster <- as.character(NewClusterScores$cluster) #Coerce from factor to character so can name clusters meaningfully, not random num

NewClusterScores$cluster[NewClusterScores$cluster == "4"]  <- "Average Value"
NewClusterScores$cluster[NewClusterScores$cluster == "3"]  <- "Crash Pad"
NewClusterScores$cluster[NewClusterScores$cluster == "2"]  <- "Overpriced"
NewClusterScores$cluster[NewClusterScores$cluster == "1"]  <- "Best Value"

ggplot(data=NewClusterScores, aes(x=Max.Guests, y=Max.Cost, color=cluster)) + 
  geom_point(size=3) +
  ggtitle("Best Value of Airbnbs in Copacabana, BR")+
  theme(plot.title = element_text(hjust = 0.5))