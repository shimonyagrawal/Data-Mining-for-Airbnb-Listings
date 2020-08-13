# Packages for data analysis
install.packages("DBI")
install.packages("odbc")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("writexl")
install.packages("mice")
install.packages("Hmisc")
install.packages("caret") # machine learning library for R 
install.packages("forecast") # predicting values in regression 
install.packages("corrplot") # correlation 
install.packages ("visualize") # visualizing t-value 
install.packages("FNN") # for KNN 
install.packages("e1071") # for Naive Bayes 
# Installing packages for mapping
install.packages(c("ggspatial", "sf", "rnaturalearth", "rnaturalearthdata","rgeos"))
# Packages for Word Cloud
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Packages for Classification Tree
install.packages('rpart')
install.packages('rpart.plot')

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

# Step 1: Data Peparation and Exploration
# Part 1: Missing Values

listings = read.csv("/Users/shimonyagrawal/Desktop/Grad /Summer 2/AD699_Data Mining/RStudio/Group Project/Airbnb.csv")

#names(listings)
#str(listings)

#Columns to Factors
listings[c(26,29,36,37,51:53,58,77,94,97:101)] <- lapply(listings[c(26,29,36,37,51:53,58,77,94,97:101)], factor)

#Columns to Dates
listings<- listings%>%
  mutate_at(vars(c(4,23,82,85,86)),as.Date, format = "%Y-%m-%d")

#Column to Numeric (%)
listings$host_response_rate<-as.numeric(gsub("\\%","",
                                             as.character(listings$host_response_rate)))
listings$host_response_rate<-listings$host_response_rate/100

#Columns to Numeric
listings$price <- as.numeric(gsub("\\$|,","",as.character(listings$price)))

listings$weekly_price <- as.numeric(gsub("\\$|,","",as.character(listings$weekly_price)))

listings$monthly_price <- as.numeric(gsub("\\$|,","",as.character(listings$monthly_price)))

listings$security_deposit<-as.numeric(gsub("\\$|,","",
                                           as.character(listings$security_deposit)))

listings$cleaning_fee<-as.numeric(gsub("\\$|,","",
                                       as.character(listings$cleaning_fee)))

listings$extra_people<-as.numeric(gsub("\\$|,","",
                                       as.character(listings$extra_people)))

#Price Variables: "price", "weekly_price", "monthly_price", "security deposit", 
#"cleaning_fee", "extra_people"

df <- filter(listings,neighbourhood_cleansed=="Copacabana")


#colnames(df)[colSums(is.na(df)) > 0]

#View(df %>%
#   dplyr::select(everything()) %>%
#  summarise_all(funs(sum(is.na(.))))%>%
# t())

#sapply(df, function(x) sum (is.na(x)))

df2 <- df[-c(9,16,17,19,28,41,77,94,95,96)] 
df3 <- df2[-c(1:4,15,17,18,20,25:30,33,34,36:42,53,54,56,57,64:75,88,92:95)] 



dflistings<-df3[-c(1:10,13)]
#View(dflistings %>%
#      dplyr::select(everything()) %>%
#     summarise_all(funs(sum(is.na(.))))%>%
#    t())
#nrow(dflistings)
Mode <- function(x) { 
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))] 
}

#summary(dflistings$review_scores_accuracy)
#View(dflistings %>% group_by(review_scores_accuracy) %>% tally(sort=TRUE))

dflistings[30:36] <- lapply(dflistings[30:36], function(x)
  replace(x, is.na(x), Mode(x[!is.na(x)])))

dflistings$reviews_per_month <- impute(dflistings$reviews_per_month, median)

dflistings$security_deposit <- impute(dflistings$security_deposit, median)

dflistings$host_response_rate <- impute(dflistings$host_response_rate, mean)

#View(dflistings %>% group_by(cleaning_fee) %>% tally(sort=TRUE))
#summary(dflistings$cleaning_fee)
dflistings$cleaning_fee <- impute(dflistings$cleaning_fee, median)



dflistings <- dflistings[-c(28,29)]  



#View(dflistings %>%
#      dplyr::select(everything()) %>%
#     summarise_all(funs(sum(is.na(.))))%>%
#    t())

dflistings <- na.omit(dflistings)



#View(dflistings %>%
#     dplyr::select(everything()) %>%
#    summarise_all(funs(sum(is.na(.))))%>%
#   t())


#nrow(dflistings)

dflistings$host_identity_verified[dflistings$host_identity_verified==' ']=NA
dflistings$host_identity_verified = droplevels(dflistings$host_identity_verified)

dflistings$host_has_profile_pic[dflistings$host_has_profile_pic==' ']=NA
dflistings$host_has_profile_pic = droplevels(dflistings$host_has_profile_pic)

dflistings$host_is_superhost[dflistings$host_is_superhost==' ']=NA
dflistings$host_is_superhost = droplevels(dflistings$host_is_superhost)

dflistings<-dflistings[-c(8)]

alpha<-dflistings

write_xlsx(alpha,"C:\\Users\\alber\\Desktop\\alpha.xlsx")

