library(tm)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
setwd("C:/Users/ysilva")
getwd() 
#Create Corpus - CHANGE PATH AS NEEDED
rating5 <- Corpus(DirSource("C:/Users/ysilva/Documents/5"))
rating5

#Check details
inspect(rating5)


#Start preprocessing
##create a function gsub function replaces all instances of a character by spaces
###This function takes a function as input, the input function should specify what transformation needs to be done.
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ",
                                                                   x))})
rating5 <- tm_map(rating5, toSpace, "-")
rating5 <- tm_map(rating5, toSpace, ":")
rating5 <- tm_map(rating5, toSpace, "'")
rating5 <- tm_map(rating5, toSpace, "'")
rating5 <- tm_map(rating5, toSpace, " -")
#Good practice to check after each step.
writeLines(as.character(rating5[[5]]))
#Remove punctuation - replace punctuation marks with " "
rating5 <- tm_map(rating5, removePunctuation)
#Transform to lower case
rating5 <- tm_map(rating5,content_transformer(tolower))
#Strip digits
rating5 <- tm_map(rating5, removeNumbers)
#Remove stopwords from standard stopword list (How to check this? How to add your own?)
rating5 <- tm_map(rating5, removeWords, stopwords("english"))
#remove insignificant words
rating5 <- tm_map(rating5, removeWords, c("dr", "doctor", "like", "ever", "also", "even", "just","provid", "didn","s","t",
                                          "care","recommend","staff","time","pain","patient")) 
#Strip whitespace (cosmetic?)
rating5 <- tm_map(rating5, stripWhitespace)

#Stem document
###the process of reducing such related words to their common root
rating5 <- tm_map(rating5, stemDocument)
rating5 <- tm_map(rating5, removeWords, c("care","offic","great","friend","year"))

##remove words that should have been eliminated by the stopword removal
###Here we have told R to include only those words that occur in  3 to 27 documents. 
###We have also enforced  lower and upper limit to length of the words included (between 4 and 20 characters)
dtmr <-DocumentTermMatrix(rating5)

##Inspecting the new DTM (Note difference in doc/termsx)
dtmr


#collapse matrix by summing over columns - this gets total counts (over all rating5) for each term
freq <- sort(colSums(as.matrix(dtmr)), decreasing = TRUE)
#length should be total number of terms
length(freq)
head(freq, 14)

##COLOR
pal1 <-brewer.pal(9,"Reds")
pal1 <- pal1[-(1:3)]
pal2 <-brewer.pal(9,"Purples")
pal2 <- pal2[-(1:2)]
pal3 <-brewer.pal(9,"PuBu")
pal3 <- pal3[-(1:3)]
pal4 <-brewer.pal(9,"GnBu")
pal4 <- pal4[-(1:3)]
pal5 <-brewer.pal(9,"BuGn")
pal5 <- pal5[-(1:3)]



#histogram
wf=data.frame(term=names(freq),occurrences=freq)
wf = wf[1:6,]
barplot(wf$occurrences, names=wf$term, main="Word Freq", xlab="Terms", ylab="Coubts", ylim=c(0,250))
head(wf)


p <- ggplot(wf, aes(term, occurrences))
p <- ggplot((wf), aes(term, occurrences), )
p <- p + geom_bar(stat="identity", fill=pal5, colour= "black")
p <- p + theme(axis.text.x=element_text(angle=25, hjust=1))
p <- p + xlab('The Most Frequent Words') 
p


#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=30)
#...add color
wordcloud(names(freq),freq,min.freq=200,colors=pal4)brewer.pal(6,"Dark2"))
            pal4)
##(8,"Set3"))
## brewer.pal(6,"Dark2"))
wordcloud(names(freq),freq, min.freq = 50,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=wcpalblue)

wcpal <- brewer.pal(11,"BrBG")
wcpal <- wcpal[-(5:7)]
wcpalblue <- brewer.pal(9,"RdYlGn")
wcpalblue <- wcpalblue[-(1:4)]

