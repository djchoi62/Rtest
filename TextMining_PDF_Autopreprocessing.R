library(stringr)
library(pdftools)

setwd("E:/R_Climate/")
file_list <- list.files()
file_extension <- str_detect(file_list,"csv")|str_detect(file_list,"png")|str_detect(file_list,"R")|str_detect(file_list,"pdf")

folder_list <- file_list[!file_extension]


for (i in 1:length(folder_list)) {
   # i <- 1
  setwd(paste0("E:/R_Climate/",folder_list[i]))
  subfile_list <- list.files()

  data <- NULL  
  for (j in 1:length(subfile_list)) {
    # j <- 1
    data <- pdf_text(subfile_list[j])
    data<-gsub("[ \t\n\r\f\v]"," ",data)
    data<-gsub("[a-z0-9]+","",data)
    data<-gsub("[\t\r\n]","",data)
    data <-gsub("[!?#$%*&+,:;<=-@_/()“”~‘’°C……O]", "", data)
    data<-gsub("\\]|\\[|\\(|\\)|:|-|\\,|\\.","",data)
    data <- str_trim(data)
    
    subfile_name <- gsub("[ \t\n\r\f\v]|\\.pdf","",subfile_list[j])
    write.csv(data,paste0(subfile_name,".csv"), row.names=F)
    
    cat("\n",i,"-",j)
  }
 
}

