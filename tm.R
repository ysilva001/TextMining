##PACKAGES##

library(readr) 
##provides several functions (read_delim(), read_tsv() and read_csv()), 
##which are faster than R base functions and import data into R as a tbl_df (pronounced as “tibble diff”)
library("tm")
library("SnowballC")

##LOADING TEXT##
#set directory
setwd("C:/Users/ysilva/Desktop/Data Science Practicum")

#read 1000 Reviews from providers with 5 stars directory data/txt
Rating5 <- read_csv("C:/Users/ysilva/Desktop/Data Science Practicum/Rating5.csv")
head(Rating5)
corpus5 <- Corpus(VectorSource(Rating5$CommentText))
summary(corpus5)

##TRANSFORM TEXT##

#use tm_map to transform the data  
corpus5 = tm_map(corpus5, content_transformer(tolower)) ##switch to lower case ##OR## corpus5 <- tm_map(corpus5, tolower)
corpus5 <- tm_map(corpus5, removeNumbers)
corpus5 <- tm_map(corpus5, removePunctuation)
corpus5 <- tm_map(corpus5, removeWords, stopwords("english")) ## remove common words (ie. the, and)
corpus5 <- tm_map(corpus5, removeWords, c("dr", "doctor"))  ##remove insignificant words

##Removing common word endings (e.g., “ing”, “es”, “s”)
##This is referred to as “stemming” documents. We stem the documents so that a word will be recognizable to the computer, 
##despite whether or not it may have a variety of possible endings in the original text.
corpus5 <- tm_map(corpus5, stemDocument, language="english")

corpus5 <- tm_map(corpus5, stripWhitespace) ##Extra whitespace is eliminated - Has to be done last 


##Review after the transformation
inspect(corpus5[1])
inspect(corpus5[3])

##Make sure to treat document as text
corpus5 <- tm_map(corpus5, PlainTextDocument) 




##STAGE DATA##

#creating term matrix with TF-IDF weighting
dtm5 <-DocumentTermMatrix(corpus5,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
dtm5
 ##OR##
review5 <- DocumentTermMatrix(corpus5)
review5
inspect(review5[500:505, 500:505])

review5 = removeSparseTerms(review5, 0.99)
review5

inspect(review5[1,1:20])
inspect(review5[1:20,1:20])

review5 <- TermDocumentMatrix(review5)  

###Simple Word Cloud
findFreqTerms(review5, 1000)

##A transpose of this matrix
tdm5 <- TermDocumentMatrix(docs)   
tdm5 

##EXPLORE DATA##

##draw a simple word cloud
library(wordcloud)
findFreqTerms(review_dtm5, 1000)
freq5 <- data.frame(sort(colSums(as.matrix(review_dtm5)), decreasing=TRUE))
lenght(freq)

##Export to Excel
x5 <- as.matrix(dtm5)   
dim(x5)   
write.csv(x5, file="dtm5.csv") 

##To do or not? Probably not. 
##To reduce the dimension of the DTM, we can Remove the less frequent terms such that the sparsity is less than 0.95
rem_dtm5 = removeSparseTerms(dtm5, 0.99) # This makes a matrix that is 99% empty space, maximum. 
rem_dtm5
inspect(rem_dtm5[1,1:20])

##WORD FREQUENCY##
freq5[head(ord)]  
freq5[tail(ord)]  

head(table(freq5), 20) ##Frequency of frequencies

##PLOT WORD FREQUENCIES
plot5 <- ggplot(subset(wf, freq5>50), aes(word, freq5))    
plot5 <- p + geom_bar(stat="identity")   
plot5 <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
plot5 

##RELATIONSHIP BETWEEN TERMS##
findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) ##find two words that appear often or can have a correlation
##.98 prevented the list from being overly long

##WORD CLOUDS##
##Words that appear at least 25 times
set.seed(142) ##make clouds consistent
wordcloud(names(freq5), freq, min.freq=25) 
wordcloud(rownames(freq5), freq[,1], max.words=50) ##, colors=brewer.pal(1, "Dark2"))

##add color
set.seed(142)   
wordcloud(names(freq5), freq, min.freq=25, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   
wordcloud(rownames(freq5), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

##Plot the 100 most frequent 
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq5), freq, max.words=100, rot.per=0.2, colors=dark2)   

##Since words such as Dr. Doctor do not carry too much meaning since reviews are about doctors, 
## we will measure the relative importance of a word to a document
review_dtm_tfidf5 <- DocumentTermMatrix(review5, control = list(weighting = weightTfIdf))
review_dtm_tfidf5 = removeSparseTerms(review_dtm_tfidf5, 0.95) ##OR## review_dtm_tfidf5 <- removeSparseTerms(review_dtm_tfidf5, 0.75)
review_dtm_tfidf5



## New word cloud
##freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf5)), decreasing=TRUE))
##wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

##Predictive Modeling
##START HERE
adtm <-DocumentTermMatrix(corpus5) 
adtm <- removeSparseTerms(adtm, 0.75)

#or compute cosine distance among documents
dissimilarity(tdm, method = "cosine")