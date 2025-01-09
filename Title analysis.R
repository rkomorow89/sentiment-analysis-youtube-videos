setwd("C:/Users/Robert/OneDrive/Dokumente/HICCS")
getwd()
us<-read.table("USvideos.txt", sep = ",", header = T, encoding = "UTF-8")
head(us$title, n = 10)
library(tm)
a <- removePunctuation(as.character(us$title))
b <- removeNumbers(a)
c <- stripWhitespace(b)
d <- trimws(c)
e <- tolower(d)
head(e,10)
f <- strsplit(e, " ")
head(f,10)
library(plyr)
g <-ldply(f,rbind)
typeof(g)
head(g, 10)

af <-read.csv("afinn.txt", sep = "\t", header = F)
ag <- is.numeric(af)
typeof(ag)
head(af, 10)
g[1,1]
af[1,1]
g <- g[1:15,]
g
af <- af[1:15,]
af
for (i in 1:dim(g)[1]) {
  for (j in 1:dim(g)[2]) {
    for (k in 1:dim(af)[1]){
      ifelse(g[i,j] == af[k,1], af[k,2], 0)
    }
  }
}
head(g, 20)
g
af

h <- ifelse(g[1,1] == af[1,1], af[1,2], 0)
head(h, 10)





if (g[1,1] == ag[1,1]) {
    g[1,1] == ag[1,2]
} else {
     g[1,1] = "k"
}
head(g, 10)
g[g == "NA"] <- 0

head(g, 10)
is.na(g[1,1])= 0
g[1,1]
h <- ifelse(g == "fake", -3,
          ifelse(g == "racist", -3,
                ifelse(g == "collapse", -2,
                       ifelse(g == "greatest", 3, 0)
)))

head(h, 10)

i <- cbind(h, rowSums(h, na.rm = T), rowMeans(h, na.rm = T))
j <-cbind(us$trending_date, i, us$views, us$likes, us$dislikes, us$comment_count)

head(j, 10)


