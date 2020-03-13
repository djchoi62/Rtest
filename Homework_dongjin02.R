library(dplyr)
library(ggplot2)

exam <- read.csv("D:/Dropbox/KRIED 데이터팀/Workshop/R_RStudio/Doit_쉽게 배우는 R 데이터 분석/RMD/csv_exam.csv")

getwd()

exam %>% filter(class != 1 & math >= 50)

exam %>% filter(english < 90 | science < 50)

exam %>% filter(class %in% c(1,3,5))

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
#  - Q1. 자동차 배기량에 따라 
# 고속도로 연비가 다른지 알아보려고 합니다. 
# `displ`(배기량)이 `4` 이하인 자동차와 `5` 이상인 자동차 중
# 어떤 자동차의 `hwy`(고속도로 연비)가 
# 평균적으로 더 높은지 알아보세요.

mpg_a <- mpg %>% filter(displ <= 4)
mpg_b <- mpg %>% filter(displ >5)

c <- c(mean(mpg_a$hwy), mean(mpg_b$hwy))

print(c)

# Q2. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다. 
# "audi"와 "toyota" 중 어느 manufacturer(자동차 제조 회사)의
# cty(도시 연비)가 평균적으로 더 높은지 알아보세요.

mpg_audi <- mpg %>% filter(manufacturer=="audi")
mpg_toyota <- mpg %>% filter(manufacturer=="toyota")
d <- c(mean(mpg_audi$cty), mean(mpg_toyota$cty))
print(d)

# Q3. "chevrolet", "ford", "honda" 자동차의 
# 고속도로 연비 평균을 알아보려고 합니다. 
# 이 회사들의 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요.

mpgHwy1 <- mpg %>% filter(manufacturer=="chevrolet" | manufacturer=="ford" | manufacturer=="honda") 
mean(mpgHwy1$hwy)

mpgHwy2 <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpgHwy2$hwy)
  

# mpg 데이터를 이용해서 분석 문제를 해결해보세요.

mpgTest <- as.data.frame(ggplot2::mpg) %>% 
  select(class,cty)
 
head(mpgTest)
View(mpgTest)
dim(mpgTest)
summary(mpgTest)

Suv <- mpgTest %>% filter(class =="suv") 
Compact <- mpgTest %>% filter(class =="compact")
meanComp <- c(mean(Suv$cty),mean(Compact$cty))
meanComp

mpgTest2 <- as.data.frame(ggplot2::mpg) %>% 
  # select(manufacturer,hwy) %>%
  filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head()

summary(mpgTest2)
head(mpgTest2,5)

# 파생변수 추가하기

mpgNew <- mpg
mpgNew %>%
  mutate(ty = hwy + cty, tyMean =(hwy + cty)/2) %>% 
  arrange(desc(tyMean)) %>% 
  head(3)

exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n=n())

  mpg %>% 
    group_by(manufacturer) %>% 
    filter(class=="suv") %>% 
    mutate(tot=(cty+hwy)/2) %>% 
    summarise(mean_tot=mean(tot)) %>% 
    arrange(desc(mean_tot)) %>% 
    head(5)

  mpg %>% group_by(class) %>% 
    summarise(ctyMean=mean(cty)) %>% 
    arrange(desc(ctyMean))

  mpg %>% group_by(manufacturer) %>% 
    summarise(hwyMean=mean(hwy)) %>% 
    arrange(desc(hwyMean)) %>% 
    head(3)

  mpg %>% select(manufacturer,class) %>% 
    filter(class=="compact") %>% 
    group_by(manufacturer) %>% 
    summarise(numCar=n()) %>% 
    arrange(desc(numCar))
  
  fuel <- data.frame(fl=c("c","d","e","p","r"),
                     price_fl=c(2.35,2.38,2.11,2.76,2.22),
                     stringAsFactors=F)
  fuel  
  
  mpg <- as.data.frame(ggplot2::mpg)
  mpgNew <- left_join(mpg, fuel, by="fl") %>% 
    select(model,fl,price_fl) %>% 
    head(5)
  
  mpg %>% group_by(manufacturer) %>% 
    summarise(mean_com = mean(hwy))
  
  # 문제1. popadults는 해당 지역의 성인 인구, poptotal은
  # 전체 인구를 나타냅니다. midwest 데이터에
  # '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요.
  
  midwest <- as.data.frame(ggplot2::midwest)

  midwest %>%
   mutate(nonAdult = (poptotal-popadults)/poptotal*100) %>% 
   arrange(desc(nonAdult)) %>%
    select(county,nonAdult) %>% 
    head(5)
 
  midwest %>% 
    mutate(nonAdult = (poptotal-popadults)/poptotal*100) %>%
    mutate(grade = ifelse(nonAdult > 40, "large",
                          ifelse(nonAdult > 30, "middle","small"))) %>% 
    select(grade) %>% 
    table()
  
  midwest %>% 
    mutate(asianRatio = (popasian*100)/poptotal) %>% 
    arrange(asianRatio) %>%
    select(state,county,asianRatio) %>% 
    head(10)
    
 