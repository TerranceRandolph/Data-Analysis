#install.packages(c("tidytext","readtext","OptimalCutpoints",
#                 "OptimalCutpoints","tm","wordcloud","slam"))
# Library to load list
lib <- c("tidytext","readtext","OptimalCutpoints",
  "OptimalCutpoints","tm","wordcloud","ggplot2","slam")

# load all
lapply(lib, library, character.only = TRUE)

# --- Step 1 --- #
# read and clean
setwd("C:/Users/terra/Desktop/Syracuse/IST 687")

mlk <- readLines(file("MLK.txt"))
mlk <- mlk[which(mlk != "")] #remove all blank lines in the text

#Create a term matrix
#interprets each element of the "mlk" as a document and create a vector source
words.vec <- VectorSource(mlk)
#create a Corpus
words.corpus <- Corpus(words.vec)
#first step transformation: make all of the letters in "words.corpus" lowercase
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
#second step transformation: remove the punctuation in "words.corpus"
words.corpus <- tm_map(words.corpus, removePunctuation)
#third step transformation: remove numbers in "words.corpus"
words.corpus <- tm_map(words.corpus, removeNumbers)
#final step transformation: take out the "stop" words, such as "the", "a" and "at"
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

#create a term-document matrix "tdm",  a "Bag of Words"
tdm <- TermDocumentMatrix(words.corpus)
#view term-document matrix "tdm"
tdm

#Create a list of counts for each word
#convert tdm into a matrix called "m"
m <- as.matrix(tdm)
View(m[1:20,])

#create a list of counts for each word named "wordCounts"
wordCounts <- rowSums(m)
wordCounts[1:10]

#sum the total number of words and store the value to "totalWords"
totalWords <- sum(wordCounts)
totalWords
#create a vector "words" that contains all the words in "wordCounts"
words <- names(wordCounts)
head(words)

#sort words in "wordCounts" by frequency
wordCounts <- sort(wordCounts, decreasing=TRUE)
#check the first several items in "wordCounts" to see if it is built correctly
head(wordCounts)

#Build Word Cloud
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
View(cloudFrame)
wordcloud(cloudFrame$word,cloudFrame$freq)

###################################
#Step 1: Sentiment / Affinity Score
###################################

#Read Afffinity file
afinn<-read.delim("HW10AFINN.txt",sep="\t",header = FALSE)
str(afinn)
colnames(afinn)<-c("Word", "Score")
#join the df match with AFINN by "word" col in match and "Word" col in AFINN
mergedTable<-merge(cloudFrame,afinn,by.x="word",by.y="Word")
str(mergedTable)



# --- Step 2 --- #
# get overall score of speech
overallScore<-sum(mergedTable$freq*mergedTable$Score)
overallScore 

# --- Step 3 --- #
#1st 25%:
mlk1 <- function(x,y){
  a <- mergedTable[x:y,] ; b <- sum(a$freq*a$Score)
  return(b)
}
mlk1(1,20) #1st 25% has score of 5

#
# IMPORTANT: Replace the upper function with one that takes 2 values as inputs 
#             to do the same measurments for the other breaking points 
#The inputs shouldn't be actual numbers, they should be similat to what I 
#             calculated in the lab example, 

# find quarter of data
paste("Quarter of the data is : ",round(tally(mergedTable) / 4))

#Check total score by summing
guess <- sum(mlk1(1,15)+mlk1(16,30)+mlk1(31,45)+mlk1(46,61)) #checks out

mlkQuarter<- c("1st Quarter","2nd Quarter", "3rd Quarter","4th Quarter")
#you need to insert different results of mlk1 inputs in mlkQuarterScore
mlkQuarterScore <- c(mlk1(1,15)+mlk1(16,30)+mlk1(31,45)+mlk1(46,61))
mlkQuarterTotal <- data.frame(mlkQuarter,mlkQuarterScore)


# --- Step 4 --- #
# plot results
#Plot the score of each quarter on a bar graph:
ggplot(mlkQuarterTotal, aes(x=mlkQuarterTotal$mlkQuarter, 
                            y=mlkQuarterTotal$mlkQuarterScore)) + 
                      geom_bar(stat="identity")+ 
                      labs(x="Quarter",
                           y="Score",
                           title="Sentiment Score for Each Quarter of MLK Speech")+
                      coord_cartesian(ylim=c(0, 100)) + 
                      scale_y_continuous(breaks=seq(0,100,10)) + 
                      theme(plot.title = element_text(hjust = 0.5))

# wordcoud with including the affinity score value
mergedTable$Cloud <- mergedTable$freq * mergedTable$Score
mergedTable$Cloud2  <- -(mergedTable$freq * mergedTable$Score)

wordcloud(mergedTable$word,mergedTable$Cloud2)



