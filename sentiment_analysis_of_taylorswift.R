library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(textdata)


taylor_swift_lyrics <- read.csv("data/taylor_swift_lyrics.csv")

#First we must preprocess the corpus. Create a document-term matrix from the `lyrics` column of the `ts` data frame. Complete the following preprocessing steps
docs <- VCorpus(VectorSource(taylor_swift_lyrics$Lyrics))

dtm <- DocumentTermMatrix(docs,
                          control = list(tolower = TRUE,
                                         removeNumbers = TRUE,
                                         removePunctuation = TRUE,
                                         stopwords = TRUE
                          ))

inspect(dtm)

dtm <- as.data.frame(as.matrix(dtm))

#We're going to use sentiment dictionaries from the `tidytext` package. Using the `get_sentiments` function, load the "bing" and "afinn" dictionaries and store them in two objects called `bing_sentiments` and `afinn_sentiments`. You might need to install the "textdata" package

#The tidytext package contains 4 general purpose lexicons in the sentiments dataset.

#afinn - listing of English words rated for valence between -5 and +5

#bing - listing of positive and negative sentiment

#nrc - list of English words and their associations with 8 emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and 2 sentiments (negative and positive); binary

#loughran - list of sentiment words for accounting and finance by category (Negative, Positive, Uncertainty, Litigious, Strong Modal, Weak Modal, Constraining)


bing_sentiments  <- get_sentiments("bing")
afinn_sentiments <- get_sentiments("afinn")

head(bing_sentiments)
head(afinn_sentiments)

#The afinn_sentiments has the rating for valence between -5 and +5, while the bing_sentiments contains listing of positive and negative sentiment. Add a column to `bing_sentiments` called `score`. This column should hold a "1" for positive words and "-1" for negative words

bing_sentiments$score <- ifelse(bing_sentiments$sentiment=="positive", 1, -1)

#Create a dataframe that holds all the words in the dtm object along with their sentiment score

# get all the words in our dtm and put it in a dataframe
words <- data.frame(word = colnames(dtm))
head(words)

# get their sentiment scores
words <- merge(words, bing_sentiments, all.x = T)
head(words)

# replace NAs with 0s
words$score[is.na(words$score)] <- 0
head(words)

# calculate documents scores with matrix algebra! 
scores <- as.matrix(dtm) %*% words$score

taylor_swift_lyrics$sentiment_bing <- scores
head(taylor_swift_lyrics)

taylor_swift_lyrics[1:60,] %>% ggplot() +
  geom_col(aes(Title, sentiment_bing), fill = "lightgreen", alpha=.75) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Sentiment Score") +
  ggtitle("Sentiment Analysis of Taylor Swift Lyrics") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

taylor_swift_lyrics[61:132,] %>% ggplot() +
  geom_col(aes(Title, sentiment_bing), fill = "lightgreen", alpha=.75) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Sentiment Score") +
  ggtitle("Sentiment Analysis of Taylor Swift Lyrics") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

## Using the code you wrote above, below we create a function that gets 1) a vector of texts, and 2) a sentiment dictionary (i.e. a dataframe with words and scores), and returns a vector of sentiment scores for each text. Use this function to repeat your analysis with the `afinn` sentiment dictionary

# preprocess texts
sentiment_score <- function(texts, sentiment_dict){
  # preprocess texts
  docs <- Corpus(VectorSource(texts))
  dtm <- DocumentTermMatrix(docs,
                            control = list(stopwords = T,
                                           tolower = TRUE,
                                           removeNumbers = TRUE,
                                           removePunctuation = TRUE))
  dtm <- as.data.frame(as.matrix(dtm))
  
  # get all the words in dtm and put it in a dataframe
  words <- data.frame(word = colnames(dtm))
  
  # get their sentiment scores
  words <- merge(words, sentiment_dict, all.x = T)
  
  # replace NAs with 0s
  # words$score[is.na(words$score)] <- 0
  words$score[is.na(words$score)] <- 0
  
  # calculate documents scores with matrix algebra!
  scores <- as.matrix(dtm) %*% words$score
  
  return(scores)
}

colnames(afinn_sentiments)[2] <- "score"
taylor_swift_lyrics$sentiment_afinn <- sentiment_score(taylor_swift_lyrics$Lyrics, afinn_sentiments)

taylor_swift_lyrics[1:60,] %>% ggplot() +
  geom_col(aes(Title, sentiment_afinn), fill = "orange", alpha=.75) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Sentiment Score") +
  ggtitle("Sentiment Analysis of Taylor Swift Lyrics") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))


taylor_swift_lyrics[61:132,] %>% ggplot() +
  geom_col(aes(Title, sentiment_afinn), fill = "orange", alpha=.75) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Sentiment Score") +
  ggtitle("Sentiment Analysis of Taylor Swift Lyrics") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

#Compare the bing and afinn dictionaries by finding out which the most and least positive Taylor Swift album is. You can also plot the sentiments for albums

albums <- taylor_swift_lyrics %>% group_by(Album) %>%
  summarise(lyrics = paste0(Lyrics, collapse = ";"))

# add sentiments
albums$sentiment_bing <- sentiment_score(albums$lyrics, bing_sentiments)

# concatenate to make albums
# albums <- taylor_swift_lyrics %>% group_by(Album) %>%
#   summarise(lyrics = paste0(Lyrics, collapse = ";"))

# add sentiments
albums$sentiment_afinn <- sentiment_score(albums$lyrics, afinn_sentiments)

albums %>% ggplot() +
  geom_col(aes(Album, sentiment_bing), fill = "#edc948", alpha=.75) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Sentiment Score") +
  ggtitle("Sentiment Analysis of Taylor Swift Albums using the Bing dictionary") +
  coord_flip() +
  theme_minimal()

albums %>% ggplot() +
  geom_col(aes(Album, sentiment_afinn), fill = "lightblue", alpha=.75) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Sentiment Score") +
  ggtitle("Sentiment Analysis of Taylor Swift Albums using the Afinn dictionary") +
  coord_flip() +
  theme_minimal()

