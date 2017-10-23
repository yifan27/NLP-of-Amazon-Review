setwd("/Volumes/MAX BACK UP/NPL")
setwd("E:/NPL")
install.packages("CRAN")
library(CRAN)
install.packages("tm")
library(tm)
install.packages("lettercase")
library(lettercase)
install.packages("wordcloud") # word-cloud generator 
library("wordcloud")
install.packages("readr")
library(readr)
install.packages("SnowballC")
library("SnowballC")


#load AFINN
afinn_list <- read.delim(file="AFINN-111.txt", header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c("word", "score")
afinn_list$word <- tolower(afinn_list$word)
afinn_list$score <- as.numeric(afinn_list$score)

#####random sample 

top_25k_raw <- read.csv("Top250K.csv",stringsAsFactors = F)
set.seed(2727)
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}
top_25k <- randomRows(top_25k_raw,20000)


# convert uppercase to lowercase
top_25k$normalized <- str_lowercase(top_25k$Text)

#SUBSTITUTE n't to not
top_25k$normalized <- sub(c("isn't|aren't|wasn't|weren't|hasn't|
       haven't|hadn't|doesn't|don't|didn't|
       won't|wouldn't|shan't|shouldn't|can't|
       cannot|couldn't|mustn't"),"not", top_25k$normalized)

# rm stop words by mystopwordslist
mystopwords <- read.csv("mystoplist.csv",stringsAsFactors = F,header = FALSE)
mystopwords <- as.character(mystopwords$V1)
top_25k$normalized <- removeWords(top_25k$normalized,c(mystopwords))

# remove all punctuation
top_25k$normalized <- removePunctuation(top_25k$normalized, preserve_intra_word_dashes = T)

top_25k$normalized <- stemDocument(top_25k$normalized, language = "english")


# data("crude")
# inspect(crude[[1]])
# inspect(stemDocument(crude[[1]]))
##############################################
# wordclound for normolizaed
#############################################
# Load the data as a corpus
wordsnormalized <- Corpus(VectorSource(top_25k$normalized))

# Remove numbers
wordsnormalized <- tm_map(wordsnormalized, removeNumbers)
# Eliminate extra white spaces
words <- tm_map(wordsnormalized, stripWhitespace)




#wordcloud of normolized
dtm <- TermDocumentMatrix(wordsnormalized)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




##############################################
# wordclound for orginal
#############################################
top_25k$Textrmpunct <- removePunctuation(top_25k$Text, preserve_intra_word_dashes = T)

wordsText <- Corpus(VectorSource(top_25k$Textrmpunct))
# Remove numbers
wordsText <- tm_map(wordsText, removeNumbers)
# Eliminate extra white spaces
wordsText <- tm_map(wordsText, stripWhitespace)

#wordcloud of orignial
dtm <- TermDocumentMatrix(wordsText)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#########################################

rownames(top_25k) <- 1:nrow(top_25k)
top_25k$score <- NA
for (i in 1:nrow(top_25k)){
#remove unnecessary characters and split up by word
wordList <- str_split(top_25k$normalized[i]," ")
words <- unlist(wordList)
#build vector with matches between sentence and each category
Matches_index <- match(words, afinn_list[,1])
#sum up number of words in each category
Matches <- afinn_list[Matches_index,2]
Matches <- as.data.frame(Matches)
top_25k$score[i] <-sum(Matches[,1],na.rm = T)
next
rm(wordList,words,Matches_index,Matches)
 }



top_25k$PN <- ifelse(top_25k$score>0,"P","N")
library(plyr)
product_summary <- ddply(top_25k,c("ProductId"),summarize, N=length(Text), MeanRate = mean(Score),MeanRV = mean(score))

summary(lm(MeanRate~MeanRV,product_summary))
attach(product_summary)
product_top6 <-head(product_summary[order(-N),] )
product_top6

par(mfrow = c(2, 3))
top1 <- top_25k[which(top_25k$ProductId==product_top6[1,1]),]
plot(top1$Score,top1$score)
abline(lm(top1$score~top1$Score))

top2 <- top_25k[which(top_25k$ProductId==product_top6[2,1]),]
plot(top2$Score,top2$score)
abline(lm(top2$score~top2$Score))

top3 <- top_25k[which(top_25k$ProductId==product_top6[3,1]),]
plot(top3$Score,top3$score)
abline(lm(top3$score~top3$Score))

top4 <- top_25k[which(top_25k$ProductId==product_top6[4,1]),]
plot(top4$Score,top4$score)
abline(lm(top4$score~top4$Score))

top5 <- top_25k[which(top_25k$ProductId==product_top6[5,1]),]
plot(top5$Score,top5$score)
abline(lm(top5$score~top5$Score))

top6 <- top_25k[which(top_25k$ProductId==product_top6[6,1]),]
plot(top6$Score,top6$score)
abline(lm(top6$score~top6$Score))

