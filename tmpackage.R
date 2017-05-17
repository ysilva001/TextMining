library(tm)
setwd("C:/Users/ysilva")
getwd() 
#Create Corpus - CHANGE PATH AS NEEDED
rating <- Corpus(DirSource("C:/Users/ysilva/Documents/RatingText"))
rating

#Check details
inspect(rating)

#inspect a particular document
writeLines(as.character(rating[[5]]))

#Start preprocessing
##create a function gsub function replaces all instances of a character by spaces
###This function takes a function as input, the input function should specify what transformation needs to be done.
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ",
                                                                   x))})
rating <- tm_map(rating, toSpace, "-")
rating <- tm_map(rating, toSpace, ":")
rating <- tm_map(rating, toSpace, "'")
rating <- tm_map(rating, toSpace, "'")
rating <- tm_map(rating, toSpace, " -")
#Good practice to check after each step.
writeLines(as.character(rating[[5]]))
#Remove punctuation - replace punctuation marks with " "
rating <- tm_map(rating, removePunctuation)
#Transform to lower case
rating <- tm_map(rating,content_transformer(tolower))
#Strip digits
rating <- tm_map(rating, removeNumbers)
#remove insignificant words
rating <- tm_map(rating, removeWords, c("dr", "doctor", "like", "ever", "also", "even", "just")) 
#Remove stopwords from standard stopword list (How to check this? How to add your own?)
rating <- tm_map(rating, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
rating <- tm_map(rating, stripWhitespace)
#inspect output
writeLines(as.character(rating[5]))
#Need SnowballC library for stemming
library(SnowballC)
#Stem document
###the process of reducing such related words to their common root
rating <- tm_map(rating,stemDocument)
#some clean up
###code to  clean up one off's
rating <- tm_map(rating, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
rating <- tm_map(rating, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
rating <- tm_map(rating, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
rating <- tm_map(rating, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
rating <- tm_map(rating, content_transformer(gsub),
               pattern = "team-", replacement = "team")
#inspect
writeLines(as.character(rating[[30]]))
#Create document-term matrix
### a matrix that lists all occurrences of words in the corpus, by document
dtm <- DocumentTermMatrix(rating)
##remove words that should have been eliminated by the stopword removal
###Here we have told R to include only those words that occur in  3 to 27 documents. 
###We have also enforced  lower and upper limit to length of the words included (between 4 and 20 characters)
dtmr <-DocumentTermMatrix(rating)

##Inspecting the new DTM (Note difference in doc/termsx)
dtmr
#inspect segment of document term matrix
inspect(dtm[1:2,1000:1020])

#collapse matrix by summing over columns - this gets total counts (over all rating) for each term
freq <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freq)

#create sort order (asc)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
#inspect least frequently occurring terms
freq[tail(ord)]
#remove very frequent and very rare words
dtmr <-DocumentTermMatrix(rating)
rownames(dtmr) <- c("Rating 1", "Rating 2", "Rating 3", "Rating 4", "Rating 5")
inspect(dtmr[1:5,1:10])


#list most frequent terms. Lower bound specified as second argument
## this includes words in all corpus and not per doc
findFreqTerms(dtmr,lowfreq=80)
#correlations with specific words
findAssocs(dtmr,"great",0.9)
findAssocs(dtmr,"best",0.9)
findAssocs(dtmr,"bad",0.9)
findAssocs(dtmr,"worst",0.9)

##Compute correlations and store in data frame
toi <- "great" # term of interest 1
toi2 <- "care"
toi3 <- "bad"
toi4 <- "best"
toi5 <- "worst"
corlimit <- 0.9 #  lower correlation bound limit.
corlimit2 <- 0.7
##OPTION FOR ONE 
great_0.9 <- data.frame(corr = findAssocs(dtmr, toi, corlimit)[[1]],
                      terms = names(findAssocs(dtmr, toi, corlimit)[[1]]))

care_0.9 <- data.frame(corr = findAssocs(dtmr, toi2, corlimit)[[1]],
                        terms = names(findAssocs(dtmr, toi2, corlimit)[[1]]))

bad_0.9 <- data.frame(corr = findAssocs(dtmr, toi3, corlimit2)[[1]],
                       terms = names(findAssocs(dtmr, toi3, corlimit)[[1]]))


##OPTION FOR MORE THAN ONE 
corr1 <- findAssocs(dtmr, toi1, corlimit)[[1]]
corr1 <- cbind(read.table(text = names(corr1), stringsAsFactors = FALSE), corr1)
corr2 <- findAssocs(dtmr, toi2, corlimit)[[1]]
corr2 <- cbind(read.table(text = names(corr2), stringsAsFactors = FALSE), corr2)
corr3 <- findAssocs(dtmr, toi3, corlimit)[[1]]
corr3 <- cbind(read.table(text = names(corr3), stringsAsFactors = FALSE), corr3)
                
# join them together
library(dplyr)
three_terms_corrs <- full_join(corr1, corr2, corr3)

# gather for plotting
library(tidyr)
three_terms_corrs_gathered <- gather(three_terms_corrs, term, correlation, corr1:corr3)

# insert the actual terms of interest so they show up on the legend
three_terms_corrs_gathered$term <- ifelse(three_terms_corrs_gathered$term  == "corr1", toi1, toi2, toi3)

# Draw the plot...
require(ggplot2)
ggplot(two_terms_corrs_gathered, aes(x = V1, y = correlation, colour =  term ) ) +
  geom_point(size = 3) +
  ylab(paste0("Correlation with the terms ", "\"", toi1,  "\"", " and ",  "\"", toi2, "\"")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


##Create a factor to allow ggplot to sort the dataframe...
great_0.7$terms <- factor(great_0.7$terms ,levels = great_0.7$terms)
care_0.9$terms <- factor(care_0.9$terms, levels = care_0.9$terms)
bad_0.9$terms <- factor(bad_0.9$terms, levels = bad_0.9$terms)

##Plot
require(ggplot2)
ggplot(bad_0.9, aes( y = terms  ) ) +
  geom_point(aes(x = corr), colour="goldenrod3", pch=19, cex=3, data = bad_0.9) +
  xlab(paste0("Correlation with the term ", "\"", toi3, "\""))

###COLOR
myplotpalette <-brewer.pal(11,"BrBG")
myplotpalette <- myplotpalette[-(5:7)]

#histogram
wf=data.frame(term=names(freq),occurrences=freq)
library(ggplot2)
p <- ggplot(subset(wf, freq>800), aes(term, occurrences))
p <- ggplot(subset(wf, freq>800), aes(term, occurrences), )
p <- p + geom_bar(stat="identity", fill=myplotpalette, colour= "black")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p <- p + xlab('The Most Frequent Words')
p
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=800)
#...add color
wordcloud(names(freq),freq,min.freq=300,colors=pal)
            ##brewer.pal(6,"Dark2"))
          ##(8,"Set3"))
           ## brewer.pal(6,"Dark2"))
pal <-brewer.pal(9,"Reds")
pal <- pal[-(1:2)]



