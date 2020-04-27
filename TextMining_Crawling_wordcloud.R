install.packages("wordcloud")

library(stringr)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

#extractNoun: key world extract

useSejongDic()

data<- read.csv("climate_news03_preAll.csv") ## processed file 불러오기

text <- as.character(data$X)

raw_noun <- sapply(text, extractNoun, USE.NAMES = F)

unlist_noun <- unlist(raw_noun)

noun <- Filter(function(x){nchar(x)>=2}, unlist_noun)

noun <- gsub("[0-9]+","", noun)
noun <- gsub("[a-zA-z]","", noun)

words <- table(noun)

word_count <- words[nchar(names(words))>1]
sort(word_count, decreasing = T)[1:30]

# wordcloud start

palete <- brewer.pal(9,"Set1")

png("extractnoun.png", width = 500, height = 500)
wordcloud(names(word_count),freq = word_count,scale = c(5,1),rot.per = 0.25,min.freq = 30, random.order = F,random.color = T,colors = palete)
dev.off()

#SimplePos09

data<- read.csv("climate_news03_preAll.csv") ## processed file 불러오기

text <- as.character(data$X)

raw_noun <- sapply(text, SimplePos09, USE.NAMES = F)

unlist_noun <- unlist(raw_noun)

split_noun <- sapply(str_split(unlist_noun, "/"), function(x){x[1]} )

noun <- Filter(function(x){nchar(x)>=2}, split_noun)

noun <- gsub("[0-9]+","", noun)
noun <- gsub("[a-zA-z]","", noun)

words <- table(noun)

word_count <- words[nchar(names(words))>1]
sort(word_count, decreasing = T)[1:30]

# wordcloud start

palete <- brewer.pal(9,"Set1")

png("SimplePose09.png", width = 500, height = 500)
wordcloud(names(word_count),freq = word_count,scale = c(5,1),rot.per = 0.25,min.freq = 30, random.order = F,random.color = T,colors = palete)
dev.off()
