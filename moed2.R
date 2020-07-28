#Q1
#importing the file
text.raw <- read.csv('OnionOrNot.csv')
text <- text.raw

str(text)
summary(text)

#change values to yes/no
conv_01 <- function(x){
  ifelse(x==1,"YES","NO")
}

text_new <- sapply(text$label, conv_01)
text$label <- text_new

#convert to factor
text$label <- factor(text$label)
str(text)

#create pie chart for the (articles from onion/not from onion)
library(ggplot2)

# Pie Chart with Percentages
table(text$label)
slices <- c(15000,9000)
lbls <- c("YES", "NO")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Articles From Onion")


#Q2
ggplot(text, aes(label)) + geom_bar()

head(text)

#library for text mining
install.packages('tm')
library(tm)

#build our data set as Corpus
text.corpus <- Corpus(VectorSource(text$text))

clean.corpus <- tm_map(text.corpus, removePunctuation)
clean.corpus[[7]][[1]]
clean.corpus <- tm_map(clean.corpus, removeNumbers)
clean.corpus <- tm_map(clean.corpus, content_transformer(tolower))
clean.corpus <- tm_map(clean.corpus, removeWords, stopwords())
clean.corpus <- tm_map(clean.corpus, stripWhitespace)

dtm <- DocumentTermMatrix(clean.corpus)
dim(dtm)

#remove words that apear in under 10 documents
dtm.freq <- DocumentTermMatrix(clean.corpus, list(dictionary = findFreqTerms(dtm,10)))
dim(dtm.freq)

inspect(dtm.freq[1:10, 1:20])

install.packages('wordcloud')
library(wordcloud)

# color pallet for the word cloud 
pal <- brewer.pal(9,'Dark2')
set.seed(1234)
wordcloud(clean.corpus ,min.freq = 10, random.order = FALSE, colors = pal)

convert_01 <- function(x){
  x <- ifelse(x>0,1,0)
  return (as.integer(x))
}

dtm.final <- apply(dtm.freq, MARGIN = 1:2, convert_01)
dtm.df <- as.data.frame(dtm.final)
#############################################
#wordcloud(c(clean.corpus,[100:1000]), frequency(10), offset(5), random.order = FALSE, colors = pal)
?wordcloud
?frequency
wordcloud(clean.corpus, seq(100, 1000, len = 5),random.order = FALSE, colors = pal)
#############################################
#split to test and train
library(caTools)

filter <- sample.split(dtm.df$text, SplitRatio = 0.7)
spam.train = subset(dtm.df, filter==T)
spam.test = subset(dtm.df, filter==F)
dim(spam.train)
dim(spam.test)






