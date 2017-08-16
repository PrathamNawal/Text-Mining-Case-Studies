#load libraries
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(sentiment)

#Rename heading
names(df) <- c("Comments")

#Delete Leading Spaces
df$Comments <- str_trim(df$Comments)
class(df$Comments)

##Pre-processing
dfCorpus <- Corpus(VectorSource(df$Comments))
writeLines(as.character(df$Comments[1]))

#Case-Folding
dfCorpus <- tm_map(dfCorpus,tolower)
writeLines(as.character(dfCorpus[2:13]))

#Stop-Words
dfCorpus <- tm_map(dfCorpus,removeWords,stopwords('english'))
writeLines(as.character(dfCorpus[2:13]))

#Remove Numbers
dfCorpus <- tm_map(dfCorpus,removeNumbers)
writeLines(as.character(dfCorpus[2:13]))

#Remove WhiteSpace
dfCorpus <- tm_map(dfCorpus,stripWhitespace)
writeLines(as.character(dfCorpus[2:13]))

#Stemming
library(SnowballC)
dfCorpus <- tm_map(dfCorpus,stemDocument)

#Plain Text Document
dfCorpus <- tm_map(dfCorpus,PlainTextDocument)

#Create Corpus
dfCorpus <- Corpus(VectorSource(dfCorpus))

## Word-Cloud
#remove defined stop words
dfCorpus_WC <- dfCorpus
dfCorpus_WC <- tm_map(dfCorpus,removeWords, c('i','its','it','us','use','used','using','will','yes','say','can','take','one',
                                              stopwords('english')))

#Word-Cloud
wordcloud(dfCorpus_WC,max.words = 100,scale = c(10,0.1),colors=brewer.pal(6, "Dark2"))

#Term Document Matrix
tdm <- TermDocumentMatrix(dfCorpus)

#calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)
words_freq = as.matrix(words_freq)
words_freq = data.frame(words_freq)
words_freq$words = row.names(words_freq)
row.names(words_freq) = NULL
words_freq = words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 50)

dfCorpus = tm_map(dfCorpus, removeWords, c('i','its','it','us','use','used','using','will','yes','say','can','take','one','also',
                                               stopwords('english')))

#sentiment Analysis
#Another method
library(RSentiment)
Amazon_Sentiment_Analysis <- calculate_sentiment(df$Comments)

#Saving the Sentiment Analysis Results
write.csv(Amazon_Sentiment_Analysis,"Amazon_Sentiment_Analysis.R",row.names = FALSE)
