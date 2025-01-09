setwd("C:/Users/Robert/OneDrive/Dokumente/HICCS")
getwd()
US<-read.table("USvideos.txt", sep = ",", header = T, encoding = "UTF-8")
head(US[1:10], n = 10)
us_title <- as.character(US$title)
us_title

library(tm)
us_title <- removePunctuation(us_title)
us_title <- removeNumbers(us_title)
us_title <- stripWhitespace(us_title)
us_title <- trimws(us_title)
head(us_title,10)

library(syuzhet)
library(xlsx)

syuzhet_dict <- get_sentiment_dictionary()
bing_dict <- get_sentiment_dictionary('bing')
afinn_dict <- get_sentiment_dictionary('afinn')

write.table(syuzhet_dict, file = "syushet_dictionary.txt")
write.table(bing_dict, file = "bing_dictionary.txt")
write.table(afinn_dict, file = "afinn_dictionary.txt")

syuzhet_vector <- get_sentiment(us_title, method="syuzhet")
bing_vector <- get_sentiment(us_title, method="bing")
afinn_vector <- get_sentiment(us_title, method="afinn")

us_title <-cbind(us_title, syuzhet_vector, bing_vector, afinn_vector, US$views, US$likes, US$dislikes, US$comment_count)

colnames(us_title)[5] <- "views"
colnames(us_title)[6] <- "likes"
colnames(us_title)[7] <- "dislikes"
colnames(us_title)[8] <- "comment_count"
head(us_title, 20)

write.table(us_title[,c(1,2)], file = "syushet_scores.txt")
write.table(us_title[,c(1,3)], file = "bing_scores.txt")
write.table(us_title[,c(1,4)], file = "afinn_scores.txt")
write.table(us_title, file = "all_scores.txt")

pa <-plot(us_title[,4], us_title[,5], xlab = "afinn", ylab = "views")

r1 <- sum(as.numeric(us_title[,5]))*10^-6  #Anzahl aller Views unabhänig vom Score
r1
l1 <-length(us_title[which(us_title[,4] == -7)])   #Anzahl Videos mit Score XX
l1
s1 <-subset(us_title, us_title[,4] < 0)    #Videos mit Score XX
s1
t1 <- sum(as.numeric(s1[,5]))*10^-6    #Anzahl Views von Videos mit Score XX (in Mio)
t1
p1 <- t1/r1*100     #Anteil der Views der Videos mit Score XX an allen Views in %
p1

h <- hist(as.numeric(us_title[,5]), freq = F, col="deepskyblue", xlab = "afinn", main = "Histogram of afinn scores")

plot(us_title[,6], us_title[,4], xlab = "views", ylab = "afinn")
plot(us_title[,4], us_title[,6], xlab = "afinn", ylab = "likes")
plot(us_title[,4], us_title[,7], xlab = "afinn", ylab = "dislikes")
plot(us_title[,4], us_title[,8], xlab = "afinn", ylab = "comment_count")
plot(us_title[,6], us_title[,6], xlab = "views", ylab = "likes")

ha <- hist(as.numeric(us_title[,4]), freq = F, breaks = seq(-10,10, length= 21), col="deepskyblue", xlab = "afinn", main = "Histogram of afinn scores")
ha$breaks
ha$counts
sum(ha$counts)
ha$counts <- ha$counts / sum(ha$counts)*100
ha$counts

a1 <-aggregate(as.numeric(us_title[,5]), by=list(Category=us_title[,4]), FUN=sum)   #Anzahl Views gruppiert nach Scores
a1
a2 <- a1[order(as.numeric(a1$Category)),]   #Scores aufsteigend geordnet
a2
b1 <- a2$x*10^-6    #nur die Views in Millionen
b1
c1 <- b1 / r1*100   #Anteil Views gruppiert nach Scores
c1
barplot(c1, col = "yellow")

d <- data.frame(x2=as.numeric(us_title[,2]),
                x3=as.numeric(us_title[,3]),
                x4=as.numeric(us_title[,4]), 
                x5=as.numeric(us_title[,5]),
                x6=as.numeric(us_title[,6]),
                x7=as.numeric(us_title[,7]),
                x8=as.numeric(us_title[,8]))
head(d,10)
?aggregate
M <- cor(d) # get correlations
M
library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix
