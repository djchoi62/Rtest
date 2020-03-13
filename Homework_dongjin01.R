
library(tidyverse)

a <- c(80,60,70,50,90)
a <- mean(a)

a <- c(80,60,70,50,90) %>% 
  mean() 
 
# product <- c("사과", "딸기", "수박")
# price <- c(1800,1500,3000)
# sales <- c(24,38,13)
# 
# homework <- data.frame(product,price,sales)
# 
# a <- c(mean(homework$price),mean(homework$sales))
# 
# homework <- homework %>% 
#   select(product,price,sales) %>% 
#   mutate(price_mean=mean(price), sales_mean=mean(sales))
# 
# as_tibble(homework)
# 
# homework

# write.csv(,row.names=F) # 행번호 붙이지 마세요
# read.csv(,stringsAsFactors=F) # 문자열로 읽으세요

mpg <- ggplot2::mpg

write.csv(mpg,"mpg.csv")

load("mpg.Rdata")
save(mpg,file="mpg.Rdata")

mpg1 <- mpg
mpg1 <- rename(mpg1, city=cty, highway=hwy)
mpg1

midwest <- ggplot2::midwest
midwest
midwest <- rename(midwest, total=poptotal, asian=popasian)

head(midwest, 10)

midwest$ratio <- midwest$asian/midwest$total*100

hist(midwest$ratio)

midwest$grade <- ifelse(midwest$ratio > mean(midwest$ratio),"large","small")

midwest$grade

a <- table(midwest$grade)

names(a)

hist(midwest$grade)

qplot(midwest$grade)


