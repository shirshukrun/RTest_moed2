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
############################################################################################
wordcloud(clean.corpus ,freq = 10 ,min.words=100,max.words=1000,
          random.order=FALSE, colors(pal))
set.seed(1234)
wordcloud(words = clean.corpus, freq = dtm$ncol, min.freq = 10,
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


######################################################################################################
#split to test and train
library(caTools)

filter <- sample.split(dtm.df$text, SplitRatio = 0.7)
text.train = subset(dtm.df, filter==T)
text.test = subset(dtm.df, filter==F)
dim(spam.train)
dim(spam.test)


#Q3:
#1
model.lr <-  glm(text~.label = binomial(link = 'logit'), data = text.train)

prediction <- predict(model.lr,text.test, type = 'response')
actual <- text.test$label
prediction.lr <- prediction >0.5
cfNB <- table(prediction.lr, actual)

#2
install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
model <- rpart(text$label,churn.train)
rpart.plot(model, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#calculating the confusion matrix
str(text.test)
prediction <- predict(model,text.test)

#function is a matrix instead of a vectr
predict.prob.yes <- prediction[,'Yes']
prediciton.dt <- predict.prob.yes > 0.5
actual <- text.test$label
cf <- table(prediciton.dt,actual)
precision <-cf['TRUE','Yes']/(cf['TRUE','Yes'] + cf['TRUE','No'] )
recall <- cf['TRUE','Yes']/(cf['TRUE','Yes'] + cf['FALSE','Yes'] )

#3
#ROC curve 
install.packages('pROC')
library(pROC)
rocCurveDT <- roc(text.test$label, predict.prob.yes, direction = "<", levels = c("No","Yes"))
rocCurveLR <- roc(text.test$label, prediction, direction = "<", levels = c("No","Yes"))
#Calculate AUC
auc(rocCurveDT)
auc(rocCurveLR)
#Naive base is a little better
plot(rocCurveDT, col="red", main='ROC chart')
par(new=TRUE)
plot(rocCurveLR, col="blue", main='ROC chart')

#Q4


