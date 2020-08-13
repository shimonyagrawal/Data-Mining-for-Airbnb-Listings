install.packages('ggthemes')
install.packages('forcats')

library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(caret)
library(ggthemes)
library(forcats)
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

# Part 1: Data Visualization 

#1 NUmber of Reviews by type of Host
gg1<-ggplot(alpha, aes(x=host_is_superhost, y=number_of_reviews))+
  geom_boxplot(color="black", fill="sienna1")+
  ggtitle("# of Reviews by Type of Host")+
  xlab("Superhost")+
  ylab("# of Reviews")+
  theme_economist()+
  coord_flip()
gg1

#2 Frequency per Room Type and Superhost

gg2<-ggplot(alpha, aes(x=fct_infreq(room_type), fill=host_is_superhost))+ 
  geom_bar(colour="black",width=.8)+
  ggtitle("Frequency per Room Type")+
  xlab("Room Type")+
  ylab("Frequency")+
  theme_economist()
gg2

#3 Frequency per Property Type
gg3<-ggplot(alpha, aes(x =fct_infreq(property_type))) + 
  geom_bar(colour="black", fill=rainbow(n=23),width=.8) + 
  ggtitle('Most preferred Property Type') + 
  xlab('Property Type') + 
  ylab('Frequency') + 
  coord_flip()+
  theme_economist()
gg3

#4 Price Distribution with and without filter
gg4.0<-ggplot(alpha, aes(x=price))+ geom_histogram(binwidth = 1000,fill="tomato4",color="black")+
  ggtitle("Price Distribution")+
  ylab("Frequency")+
  xlab("Price")+
  theme_economist()
gg4.0

gg4<-ggplot(alpha, aes(x=price))+ 
  geom_histogram(data= .%>% filter(price<2500),binwidth = 50,fill="tomato4",color="black")+
  ggtitle("Price Distribution")+
  ylab("Frequency")+
  xlab("Price")+
  theme_economist()
gg4

#5 
gg5<-ggplot(alpha, aes(x=price))+
  geom_histogram(data= .%>% filter(price<2500),binwidth = 50,color="black", fill="tan1")+
  ggtitle("Frequency by Price and Room Type")+
  ylab("Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~room_type)
gg5

# Extra Visualization
# Word Cloud depicting the most required Amenities 

# create a vector containing only the text and create a corpus 
df.wordcloud <- df %>%
  select(amenities)

wordcloudtext1 <- df.wordcloud$amenities
wordclouddoc1 <- Corpus(VectorSource(wordcloudtext1))

# clean the data to remove puncutations, numbers etc .
wordclouddoc1 <- wordclouddoc1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
wordclouddoc1 <- tm_map(wordclouddoc1, content_transformer(tolower))
wordclouddoc1 <- tm_map(wordclouddoc1, removeWords, stopwords("english"))

# Create a document term matrix to have words in the first column and frequency in the second 
dtm <- TermDocumentMatrix(wordclouddoc1)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
wordcloud1 <- data.frame(word = names(words), freq = words)

view(wordcloud1)

# generate the word cloud 
set.seed(1234)
wordcloud(words = wordcloud1$word, freq = wordcloud1$freq, min.freq = 1,    
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5,0.25),       
          colors=brewer.pal(8, "Dark2"))


# Part 4: Mapping 

world <- ne_countries(scale = "medium", returnclass = "sf") #Create object with geo locations

AirbnbLocationsPrice <- data.frame(longitude = alpha$longitude, latitude = alpha$latitude, price = as.numeric(alpha$price))
summary(AirbnbLocationsPrice) #Look @ min/max values for long/lat to "zoom in" on map Long=(-43.20, -43.17), Lat=(-22.99, -22.96)
AirbnbLocationsPrice <- filter(AirbnbLocationsPrice, price < 2500) #Filter outliers and remove according to Alpha Team threshold

theme_set(theme_bw())
CopaPriceMap <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = AirbnbLocationsPrice, aes(x = longitude, y = latitude, fill = price), size = 4, 
             shape = 23) +
  coord_sf(xlim = c(-43.20, -43.17), ylim = c(-22.99, -22.96), expand = FALSE) +
  ggtitle("Airbnb Cost in Copacabana, BR")+
  theme(plot.title = element_text(hjust = 0.5))

CopaPriceMap

# Part 5: Word Cloud (Neighbourhood Overview)
names(df)
df.wordcloud <- df %>%
  filter(neighbourhood_cleansed == 'Copacabana') %>%
  select(amenities, neighborhood_overview, neighbourhood_cleansed)

# create a vector containing only the text and create a corpus 
wordcloudtext <- df$neighborhood_overview
wordclouddoc <- Corpus(VectorSource(wordcloudtext))

# clean the data to remove puncutations, numbers etc .
wordclouddoc <- wordclouddoc %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
wordclouddoc <- tm_map(wordclouddoc, content_transformer(tolower))
wordclouddoc <- tm_map(wordclouddoc, removeWords, stopwords("english"))

# Create a document term matrix to have words in the first column and frequency in the second 
dtm <- TermDocumentMatrix(wordclouddoc)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
wordcloud <- data.frame(word = names(words), freq = words)

view(wordcloud)

# generate the word cloud 
set.seed(1234)
wordcloud(words = wordcloud$word, freq = wordcloud$freq, min.freq = 1,    
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5,0.25),       
          colors=brewer.pal(8, "Dark2"))