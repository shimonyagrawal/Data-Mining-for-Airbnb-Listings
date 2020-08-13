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

# Step 3: Classification 
# Part 2: Naive Bayes 
copacabana_nb <- alpha %>%
  filter(property_type == c('Aparthotel','Apartment', 'Condominium', 'Loft')) %>%
  select('bedrooms', 'review_scores_location','review_scores_cleanliness', 'property_type','room_type', 
         'instant_bookable', 'host_is_superhost', 'require_guest_phone_verification','cancellation_policy') 

copacabana_nb$bedrooms <- as.factor(copacabana_nb$bedrooms)
copacabana_nb$review_scores_location <- as.factor(copacabana_nb$review_scores_location)
copacabana_nb$review_scores_cleanliness <- as.factor(copacabana_nb$review_scores_cleanliness)

nrow(copacabana_nb); nrow(copacabana_nb) * .6
set.seed(150) 

copacabana_sample <- sample_n(copacabana_nb, 2009) 
Train_nb <- slice(copacabana_sample, 1:1205)
Valid_nb <- slice(copacabana_sample, 1206:2009)

nb_model <- naiveBayes(instant_bookable ~ ., data = Train_nb)
nb_model  #False: 60% , True: 39%

fictionalapartment <- data.frame(bedrooms = 3,
                                 review_scores_location = 8,
                                 review_scores_cleanliness = 9, 
                                 property_type = 'Apartment', 
                                 room_type = 'Entire home/apt',
                                 host_is_superhost = 'True', 
                                 require_guest_phone_verification = 'True', 
                                 cancellation_policy = 'flexible' )

fictionalapartment[c(1:8)] <- lapply(fictionalapartment[c(1:8)], factor)

# str(fictionalapartment)

#Predict new variable
predict(nb_model, newdata = fictionalapartment)

predict(nb_model, newdata = fictionalapartment, type = "raw") #55%,44%

instantbookablefalse <- 0.6091286 * 0.144414169 * 0.014986376 * 0.136239782 * 0.949591281 * 0.799727520 * 0.1362398 * 0.02588556 * 0.27929155
instantbookabletrue <-  0.3908714 * 0.112526539 * 0.014861996 * 0.208067941 * 0.938428875 * 0.798301486 * 0.2144374 * 0.01698514 * 0.303609342

instantbookablefalse / (instantbookabletrue+instantbookablefalse) # 54%
instantbookabletrue / (instantbookabletrue+instantbookablefalse) # 45% 

# Training data 
pred1 <- predict(nb_model, newdata = Train_nb) 
confusionMatrix(pred1, Train_nb$instant_bookable)

# Validation data
pred2 <- predict(nb_model, newdata = Valid_nb) 
confusionMatrix(pred2, Valid_nb$instant_bookable)
