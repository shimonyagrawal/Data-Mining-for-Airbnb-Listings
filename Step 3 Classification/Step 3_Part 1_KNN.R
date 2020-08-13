library("tidyverse")
library("DEoptim")
library("MultiRNG")
library("matrixcalc")
library("ggplot2")
library("parallel")
library("magrittr")
library("dplyr")
library("maps")
library("leaflet")
library("forcats")
library("labeling")
library("extraDistr")
library("caret")
library("MASS")
library("visualize")
library("tidyr")
library("forecast")
library("e1071")
library("vcd")
library("fitdistrplus")
library("qwraps2")
library("FNN")
library("mice")
library("Hmisc")

# Step 3: Classification 
# Part 1: K-Nearest Neighbours 

listings <- read.csv("C:/Users/oscar/OneDrive/Documents/Boston University/Summer 2/AD699 Data Mining for Business Analytics/Semester Project/listings.csv")

str(alpha)
names(alpha)
dfclass<- alpha[-c(2,3,5:7,10:12,17,34,36,37)]
str(dfclass)

#nrow(dfclass)
set.seed(699)
samplerclass <- sample_n(dfclass, 8415)
train.df.class <- slice(samplerclass, 1:5049)
valid.df.class <- slice(samplerclass, 5050:8415)

norm.values<-preProcess(train.df.class, method = c("center", "scale"))
train.norm.df<-predict(norm.values, train.df.class)
valid.norm.df<-predict(norm.values, valid.df.class)

names(train.norm.df)
#cancellation_policy [25]

accuracy<-data.frame(k = seq(1, 30, 1), accuracy = rep(0,30))
for (i in 1:30) {
  knn.pred<-knn(train.norm.df[,c(1:24,26)], valid.norm.df[,c(1:24,26)], 
                cl = train.norm.df$cancellation_policy, k = i)
  accuracy[i, 2]<-confusionMatrix(knn.pred,valid.norm.df$cancellation_policy)$overall[1]
}
accuracy

#K=22

names(train.norm.df)

n<-1
Brazil<-c(runif(n, min(train.norm.df$host_id), max(train.norm.df$host_id)),
             runif(n, min(train.norm.df$host_response_rate), max(train.norm.df$host_response_rate)),
             runif(n, min(train.norm.df$latitude), max(train.norm.df$latitude)),
             runif(n, min(train.norm.df$longitude), max(train.norm.df$longitude)),
             runif(n, min(train.norm.df$accommodates), max(train.norm.df$accommodates)),
             runif(n, min(train.norm.df$bathrooms), max(train.norm.df$bathrooms)),
             runif(n, min(train.norm.df$bedrooms), max(train.norm.df$bedrooms)),
             runif(n, min(train.norm.df$beds), max(train.norm.df$beds)),
             runif(n, min(train.norm.df$price), max(train.norm.df$price)),
             runif(n, min(train.norm.df$security_deposit), max(train.norm.df$security_deposit)),
             runif(n, min(train.norm.df$cleaning_fee), max(train.norm.df$cleaning_fee)),
             runif(n, min(train.norm.df$guests_included), max(train.norm.df$guests_included)),
             runif(n, min(train.norm.df$extra_people), max(train.norm.df$extra_people)),
             runif(n, min(train.norm.df$minimum_nights), max(train.norm.df$minimum_nights)),
             runif(n, min(train.norm.df$maximum_nights), max(train.norm.df$maximum_nights)),
             runif(n, min(train.norm.df$number_of_reviews), max(train.norm.df$number_of_reviews)),
             runif(n, min(train.norm.df$number_of_reviews_ltm), max(train.norm.df$number_of_reviews_ltm)),
             runif(n, min(train.norm.df$review_scores_rating), max(train.norm.df$review_scores_rating)),
             runif(n, min(train.norm.df$review_scores_accuracy), max(train.norm.df$review_scores_accuracy)),
             runif(n, min(train.norm.df$review_scores_cleanliness), max(train.norm.df$review_scores_cleanliness)),
             runif(n, min(train.norm.df$review_scores_checkin), max(train.norm.df$review_scores_checkin)),
             runif(n, min(train.norm.df$review_scores_communication), max(train.norm.df$review_scores_communication)),
             runif(n, min(train.norm.df$review_scores_location), max(train.norm.df$review_scores_location)),
             runif(n, min(train.norm.df$review_scores_value), max(train.norm.df$review_scores_value)),
             runif(n, min(train.norm.df$reviews_per_month), max(train.norm.df$reviews_per_month)))

knn.Brazil<-knn(train.norm.df[,c(1:24,26)], Brazil, 
              cl = train.norm.df$cancellation_policy, k = 22)
row.names(train.norm.df)[attr(knn.Brazil, "nn.index")]

data.frame(train.norm.df[4723,25],
           train.norm.df[1952,25],
           train.norm.df[1454,25],
           train.norm.df[4349,25],
           train.norm.df[3321,25],
           train.norm.df[4984,25],
           train.norm.df[2942,25],
           train.norm.df[1420,25],
           train.norm.df[701,25],
           train.norm.df[4310,25],
           train.norm.df[2338,25],
           train.norm.df[2630,25],
           train.norm.df[1049,25],
           train.norm.df[2756,25],
           train.norm.df[2008,25],
           train.norm.df[2869,25],
           train.norm.df[2046,25],
           train.norm.df[4048,25],
           train.norm.df[3277,25],
           train.norm.df[2244,25],
           train.norm.df[4263,25],
           train.norm.df[1493,25]) %>% t()

##############################################################