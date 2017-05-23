
# step 1:create a text file
#Step 2 : Install and load the required packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Step 3 : Text mining/Load the text
filePath <-"~/Desktop/ml.txt"
text <- readLines(filePath)
# Load the data as a corpus
docs <- Corpus(VectorSource(text))
docs
inspect(docs)

#Step 4:Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Step 5:Cleaning the text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#Step 6 : Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
dtm
m <- as.matrix(dtm)
m
v <- sort(rowSums(m),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
head(d, 14)

#Step 7 : Generate the Word cloud
set.seed(1000)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(15, "Dark2"))
#Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 1)
findAssocs(dtm, terms = "center", corlimit = 0.5)
#The frequency table of words
head(d,10)
