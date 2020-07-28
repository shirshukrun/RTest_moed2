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









