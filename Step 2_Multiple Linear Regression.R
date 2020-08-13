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

# Step 2: Prediction
# Multiple Linear Regression 

#Alpha Data Frame
copacabana<-read_csv("alpha.csv")
copacabana<-mutate_if(copacabana, is.logical, as.factor)
copacabana<-mutate_if(copacabana, is.character, as.factor)
copacabana<-select(copacabana, host_response_rate, host_is_superhost, latitude, longitude, room_type,
                   accommodates, bathrooms, bedrooms, beds, bed_type, price, guests_included, extra_people,
                   minimum_nights, maximum_nights, number_of_reviews, review_scores_rating, review_scores_cleanliness,
                   review_scores_communication,  review_scores_location,  review_scores_value, cancellation_policy)
copacabana<-filter(copacabana, price<2501)


#Data Partitioning
sampledf<-sample(copacabana)
#sampledf<-select(sampledf, -orig.id)
training<-slice(sampledf, 0:4959) 
validate<-slice(sampledf, 4960:8265)


#Correlation
cor(training %>% select(host_response_rate, latitude, longitude, accommodates, bathrooms, bedrooms, beds,
                        guests_included, extra_people, minimum_nights, maximum_nights, number_of_reviews, 
                        review_scores_rating, review_scores_cleanliness, review_scores_communication,  
                        review_scores_location,  review_scores_value))
cor(training %>% select(host_response_rate, latitude, longitude, bathrooms, bedrooms, beds, guests_included, 
                        extra_people, minimum_nights, maximum_nights, number_of_reviews, review_scores_rating,
                        review_scores_cleanliness, review_scores_communication,  review_scores_location,  
                        review_scores_value))


#Original Model
mlrmodel<-lm(price~ host_response_rate + host_is_superhost + latitude + longitude + room_type +
               bathrooms + bedrooms + beds + bed_type + guests_included + extra_people + minimum_nights +
               maximum_nights + number_of_reviews + review_scores_rating + review_scores_cleanliness +
               review_scores_communication + review_scores_location + review_scores_value + cancellation_policy,
             data = training)
summary(mlrmodel)


#Backward Elimination
step(mlrmodel,direction = "backward")


#Final Model
mlrmodel<-lm(price ~ host_response_rate + host_is_superhost + latitude + longitude + room_type + bathrooms + bedrooms + 
               beds + guests_included + extra_people + number_of_reviews + review_scores_rating + review_scores_cleanliness + 
               review_scores_location + cancellation_policy, data = training)
summary(mlrmodel)


#Accuracy
ptmlrmodel<-predict(mlrmodel, training)
pvmlrmodel<-predict(mlrmodel, validate)
accuracy(ptmlrmodel, training$price)
accuracy(pvmlrmodel, validate$price)

