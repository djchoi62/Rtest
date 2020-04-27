score<-c(80,60,70,50,90)
score

mean(score)
a<-mean(score)
  library(tidyverse)
a <-c(80,60,70,50,90) %>% 
  mean()
a

item <-c("사과","딸기", "수박")
price <-c(1800,1500,3000)
sale <- c(24,38,13)
note<- df.frame(item, price, sale)

homework <- data.frame(item = c("사과","딸기","수박"), price=c(1800,1500,3000), sale=c(24,38,13))
homework <-homework %>% 
  select(item, price, sale) %>% 
  mutate(가격평균=mean(price),판매평균=mean(sale))
homework
as_tibble(homework)

mpg <- ggplot2::mpg
midwest <- ggplot2::midwest

load()
save(mpg,file="mpg.rda")

mpg <-rename(mpg,city=cty)
mpg <-rename(mpg,highway=hwy)
mpg


midwest
midwest <-rename(midwest,total=poptotal)
midwest<-rename(midwest,asian=popasian)
midwest
View(midwest)
head(midwest,5)
asianRatio <-  midwest$asian/midwest$total*100
hist(asianRatio)

midwest$grade <- ifelse(asianRatio>mean(asianRatio),"large","small")
midwest$grade
hist()
a <- table(midwest$grade)
a
names(a)
qplot(midwest$grade)

