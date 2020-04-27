library(stringr)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

# processed file 불러오기
data<- read.csv("pdfdata.csv")
# data <- read.csv("Klima142.csv")

# 열 이름 바꾸기
colnames(data) <- c("contents")

# 열 변수 타입 변환하기
text <- as.character(data$contents)

#extractNoun: key world extract
useNIADic()
#useSejongDic()

raw_noun <- sapply(text, extractNoun, USE.NAMES = F)

unlist_noun <- unlist(raw_noun)

noun <- Filter(function(x){nchar(x)>=2}, unlist_noun)

words <- table(noun)

word_count <- words[nchar(names(words))>1]
sort(word_count, decreasing = T)[1:30]

# wordcloud start

palete <- brewer.pal(9,"Set1")
#set.seed(1234)

png("pdfdata_extractnoun.png", width = 500, height = 500)
wordcloud(names(word_count),freq = word_count,scale = c(6,0,2),rot.per = 0.25,min.freq = 10, random.order = F,random.color = T,colors = palete)
dev.off()

#SimplePos09
data<- read.csv("pdfdata.csv")
# data <- read.csv("Klima1428.csv")
colnames(data) <- c("contents")
text <- as.character(data$contents)

raw_noun <- sapply(text, SimplePos09, USE.NAMES = F)

unlist_noun <- unlist(raw_noun)

split_noun <- sapply(str_split(unlist_noun, "/"), function(x){x[1]} )

noun <- Filter(function(x){nchar(x)>=2}, split_noun)

words <- table(noun)

word_count <- words[nchar(names(words))>1]
sort(word_count, decreasing = T)[1:30]

# wordcloud start

palete <- brewer.pal(9,"Set1")

png("pdfdata_SimplePose09.png", width = 500, height = 500)
wordcloud(names(word_count),freq = word_count,scale = c(5,1),rot.per = 0.25,min.freq = 10, random.order = F,random.color = T,colors = palete)
dev.off()
