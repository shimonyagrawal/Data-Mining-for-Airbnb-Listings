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

# Step 1: Data Preparation and Exploration 
# Part 2: Summary Statistics - Oscar 

str(alpha)
summary(alpha)

#Hosts with most properties
df.ss1 <- alpha %>% 
  group_by(host_id) %>% 
  tally(sort=TRUE)
View(df.ss1)

hp <- c(91654021,81876389,13580277,224192,31275569,
        66877715,74463624,1500426,1381764,2915201)  # Host properties 

#Property Types
df.ss2<- dflistings %>% 
  group_by(property_type) %>%
  tally(sort=TRUE)
View(df.ss2)

#Apartments

df.ss3 <- filter(dflistings,property_type=="Apartment")
df.ss3.1 <- filter(df.ss3, host_id %in% hp)

#Number of Reviews (LTM)
summary(df.ss3$number_of_reviews_ltm)
summary(df.ss3.1$number_of_reviews_ltm)

#Review Scores Rating
summary(df.ss3$review_scores_rating)
summary(df.ss3.1$review_scores_rating)

#Condominium, Loft & Serviced Apartment	
ss4<-c("Condominium","Loft", "Serviced apartment")
df.ss4 <- filter(dflistings,property_type %in% ss4)
df.ss4.1 <- filter(df.ss4,host_id %in% hp)

#Number of Reviews (LTM)
summary(df.ss4$number_of_reviews_ltm)
summary(df.ss4.1$number_of_reviews_ltm)

#Review Scores Rating
summary(df.ss4$review_scores_rating)
summary(df.ss4.1$review_scores_rating)

#Houses
df.ss5 <- filter(dflistings,property_type=="House")
df.ss5.1 <- filter(df.ss5,host_id %in% hp)

#Number of Reviews (LTM)
summary(df.ss5$number_of_reviews_ltm)
summary(df.ss5.1$number_of_reviews_ltm)

#Review Scores Rating
summary(df.ss5$review_scores_rating)
summary(df.ss5.1$review_scores_rating)
