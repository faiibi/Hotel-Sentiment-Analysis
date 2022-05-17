#This project aims to perform sentimental analysis on the hotel data

#Install all the required packages
install.packages("tidyverse")
library(tidyverse)
library(tm)
library(wordcloud)
install.packages('tidytext')
library(tidytext)
install.packages('janitor')
library(janitor)
install.packages('skimr')
library(skimr)
library(glue)
library(stringr)


#import the dataset
sent_data <- read.csv("tourist_accommodation_reviews.csv", header =TRUE)

#inspect the imported dataset
names(sent_data)
head(sent_data)
tail(sent_data)
summary(sent_data)
str(sent_data)
dim(sent_data)

#clean variable names
sent_data<- sent_data %>% 
  janitor::clean_names()

#inspect new variable names
names(sent_data)

#check for missing values
skim(sent_data)

#get 30 samples of hotels from the dataset
set.seed(300)
hot_samples<- sample(sent_data$hotel_restaurant_name, size = 30, replace = FALSE)
hot_samples

#import positive and neative lexicon
pos_lexi <- read.csv("positive-lexicon.txt")
neg_lexi <- read.csv("negative-lexicon.txt")

#inspect lexicon
head(pos_lexi)
head(neg_lexi)
tail(pos_lexi)
tail(neg_lexi)

#create sentimental analysis function

sentiment <- function(stem_corpus) 
{
  #generate wordclouds
  wordcloud(stem_corpus,
            min.freq = 3,
            colors=brewer.pal(8, "Dark2"),
            random.color = TRUE,
            max.words = 100)
  
  #Calculating the count of total positive and negative words in each review 
    #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(stem_corpus)
  
  for(i in 1:size)
  {
    #All the words in current review
    corpus_words<- list(strsplit(stem_corpus[[i]]$content, split = " "))
    #positive words in current review
    
    pos_count <-length(intersect(unlist(corpus_words), unlist(pos_lexi)))
    #negative words in current review 
    neg_count <- length(intersect(unlist(corpus_words), unlist(neg_lexi)))
    
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
    
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  overall_positive_percentage ## overall positive percentage
  
  
  overall_positive_percentage<-paste(round(overall_positive_percentage,2),"%")
  
  overall_negative_percentage<-paste(round(overall_negative_percentage,2),"%")
  
  
  overall_sentiment_no <- total_pos_count - total_neg_count
  
  
  sent_table<- data.frame( Postive=c(total_pos_count),
                           Negative=c(total_neg_count ),
                           Sentiment = c(overall_sentiment_no),
                           Overall_Positive_Sentiment = c(overall_positive_percentage),
                           Overall_Negative_Sentiment = c(overall_negative_percentage))
  
  return(sent_table)
  
}

#create a dataframe


total_sent_table <- c()


  for (i in hot_samples) {
    hotel_name <- paste(i)
    hotel_data <- subset(sent_data, hotel_restaurant_name == hotel_name)
    hotel_review <- hotel_data$review
    hotel_review<- tolower(hotel_review)
    hotel_review <- gsub("http\\S+\\s*", "", hotel_review)
    hotel_review <- gsub("[[:punct:]]", "", hotel_review)
    hotel_review <- gsub("[[:digit:]]", "", hotel_review)
    hotel_review <- gsub("^ ", "", hotel_review)
    hotel_review <- gsub(" $", "", hotel_review)
    hotel_review <- gsub("restaurant", "", hotel_review)
    hotel_review <- gsub("hotel", "", hotel_review)
    hotel_review <- gsub("bar", "", hotel_review)
    hotel_review <- gsub("food", "", hotel_review)
    hotel_review_corpus <- Corpus(VectorSource(hotel_review))
    hotel_review_corpus <- tm_map(hotel_review_corpus, removeWords,stopwords("english"))
    hotel_review_corpus <- tm_map(hotel_review_corpus, stripWhitespace)
    hotel_review_corpus <- tm_map(hotel_review_corpus, stemDocument)
    hotel_review_sent <- sentiment(hotel_review_corpus)
    total_sent_table <- rbind(total_sent_table,hotel_review_sent)
  
    
  }


total_sent_table<- cbind(total_sent_table, hot_samples)
total_sent_table <- total_sent_table[, c(6,1,2,3,4,5)]
total_sent_table

# creat dataframe for hotel names and sentiment


sent_plot<- total_sent_table %>% select(1, 4)
sent_plot

#plot sentiments

ggplot(sent_plot, aes(x = hot_samples, y =Sentiment, color = hot_samples)) + 
  geom_boxplot()

ggplot(sent_plot, aes(x = hot_samples, y = Sentiment)) + 
  geom_point(aes(color = hot_samples))+ 
  geom_smooth(method = "auto")







