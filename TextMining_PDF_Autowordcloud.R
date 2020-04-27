library(stringr)
library(pdftools)
library(KoNLP)
library(wordcloud)


library(RColorBrewer)

useNIADic()
setwd("E:/R_Climate/")
file_list <- list.files()
file_extension <- str_detect(file_list,"csv")|str_detect(file_list,"png")|str_detect(file_list,"R")|str_detect(file_list,"pdf")

folder_list <- file_list[!file_extension]


for (i in 1:length(folder_list)) {
   # i <- 1
  setwd(paste0("E:/R_Climate/",folder_list[i]))
  subfile_list <- list.files()

  subfile_extension <-str_detect(subfile_list,"csv")
  csvfile_list <- subfile_list[subfile_extension]
  
  data <- NULL  
  for (j in 1:length(csvfile_list)) {
    # j <- 1
    data <- read.csv(paste0(csvfile_list[j],""))
    
    colnames(data) <- c("contents")
    text <- as.character(data$contents)
    
    
    raw_noun <- sapply(text, extractNoun, USE.NAMES = F)
    
    unlist_noun <- unlist(raw_noun)
    
    noun <- Filter(function(x){nchar(x)>=2}, unlist_noun)
    
    words <- table(noun)
    
    word_count <- words[nchar(names(words))>1]
    sort(word_count, decreasing = T)[1:30]
    
    palete <- brewer.pal(8,"Dark2")
    #set.seed(1234)
    
    png(paste0(csvfile_list[j],".png"), width = 500, height = 500)
    wordcloud(names(word_count),freq = word_count,scale = c(6,0,2),rot.per = 0.25,min.freq = 10, random.order = F,random.color = T,colors = palete)
    title(paste0(csvfile_list[j],".png"))
    dev.off()
    
    
    cat("\n",i,"-",j)
  }
 
}

