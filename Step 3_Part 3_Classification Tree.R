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
# Part 3: Classification Tree

options(scipen = 999)
str(alpha)

#Review of variables
pt_test<-c("Apartment","Condominium","Loft", "Serviced apartment","House","Aparthotel")
CopacabanaLM<-subset(alpha, select=-c(1:2,8:9)) %>% 
  filter(property_type %in% pt_test)

modelLM<-lm(cleaning_fee~., data=CopacabanaLM)
summary(modelLM)

modelLM1<-lm(cleaning_fee~price+host_identity_verified+property_type+room_type+
               bathrooms+bedrooms+security_deposit+guests_included+extra_people+
               minimum_nights+review_scores_value+cancellation_policy+
               require_guest_profile_picture+require_guest_phone_verification+reviews_per_month, 
             data=CopacabanaLM)
summary(modelLM1)

modelLM2<-lm(cleaning_fee~price+host_identity_verified+
               bathrooms+bedrooms+security_deposit+guests_included+extra_people+
               minimum_nights+review_scores_value+cancellation_policy+
               require_guest_profile_picture+require_guest_phone_verification+reviews_per_month, 
             data=CopacabanaLM)
summary(modelLM2)

#Dataset for Classification Tree
CopacabanaCT<-subset(alpha,select=c(7,14:15,18:23,33,35:38))
CopacabanaCTF <- CopacabanaCT %>% filter(price %in% (0:2500))
colSums(is.na(CopacabanaCTF))
summary(CopacabanaCTF$cleaning_fee)

#Lowest value is included
#CopacabanaNA$cleaning_fee<-cut(CopacabanaNA$cleaning_fee,breaks = c(0,100,156,200,2585),labels = c("Low", "Average", "Expensive", "Very Expensive"), include.lowest = TRUE)
CopacabanaCTF$cleaning_fee<-cut(CopacabanaCTF$cleaning_fee,breaks = c(0,105,150,170,2099),labels = c("Low", "Average", "Expensive", "Very Expensive"))

nrow(CopacabanaCTF);nrow(CopacabanaCTF)*.6
set.seed(999)
sampler<-sample_n(CopacabanaCTF,8265)
train.df<-slice(sampler,1:4959)
valid.df<-slice(sampler,4960:8265)

model<-rpart(cleaning_fee~., 
             method="class", xval= 5, cp=0.00, data=train.df)

#rpart.plot(model) too large for a good visualization. 

a<-printcp(model)
class(a)
a<-data.frame(a)
which.min(a$xerror)
which.min(a$xstd)
plotcp(model)

model.pred<-predict(model,train.df,type="class")
confusionMatrix(model.pred,train.df$cleaning_fee)

model.pred2<-predict(model,valid.df,type="class")
confusionMatrix(model.pred2,valid.df$cleaning_fee)

#The cp of 0.00164880, row # 10 of "a", provides the best option,as it has the lowest Xerror.
#However, the cp of 0.00288541, row# 6 of "a", provides a clearer decision tree at a similar Xerror.


modelA<-rpart(cleaning_fee~., 
             method="class", cp=0.00288541, data=train.df)

model.predA<-predict(modelA,train.df,type="class")
confusionMatrix(model.predA,train.df$cleaning_fee)

model.pred2A<-predict(modelA,valid.df,type="class")
confusionMatrix(model.pred2A,valid.df$cleaning_fee)

rpart.plot(modelA)
                            