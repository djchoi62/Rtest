# 패키지 설치
install.packages("pdftools")


# 패키지 가져오기
library(pdftools)
library(stringr)

# pdf파일 가져오기
# pdfdata <- pdf_text("비정형.pdf")
pdfdata <- pdf_text("Klima 142.pdf")


# 전처리작업 

pdfdata<-gsub("[ \t\n\r\f\v]"," ",pdfdata)
pdfdata<-gsub("[a-z0-9]+","",pdfdata)
pdfdata<-gsub("[\t\r\n]","",pdfdata)
pdfdata <-gsub("[!?#$%*&+,:;<=-@_/()“”~‘’°C……O]", "", pdfdata)
pdfdata<-gsub("\\]|\\[|\\(|\\)|:|-|\\,|\\.","",pdfdata)
pdfdata <- str_trim(pdfdata)

# 파일 저장하기
write.csv(pdfdata,"Klima142.csv", row.names = F)
