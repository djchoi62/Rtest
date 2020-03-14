install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)



raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- raw_welfare
head(welfare)
tail(welfare)
View(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

# 성별 월급차이

class(welfare$sex)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex ==9, NA, welfare$sex)
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + 
  xlim(0,1000)+
  ylim(0,600)

summary(welfare$income)
head(welfare$income,20)
table(is.na(welfare$income))
View(welfare$income)

welfare$income <- ifelse(welfare$income %in% c(0,9999),NA, welfare$income)
table(is.na(welfare$income))

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income)) +
  geom_col()

# 나이와 월급

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$bir))

welfare$age <- 2015 - welfare$birth +1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income,20)

ggplot(data=age_income, aes(x=age, y=mean_income))+
  geom_line()

# 연령대별 월급차이

welfare <- welfare %>% 
  mutate(ageg = ifelse(age <30, "young", ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
age_income
ggplot(data=age_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))

# 연령대 및 성별 월급차이

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

ggplot(data = sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)  

ggplot(data = sex_age, aes(x=age, y=mean_income, col=sex)) +
geom_line()  

# 직업별 월급차이

class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names=T, sheet=2)
head(list_job)
dim(list_job)
welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head()

job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)

top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data=top10, aes(x=reorder(job,mean_income), y=mean_income))+
  geom_col()+
  coord_flip()

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10

ggplot(data=bottom10, aes(x = reorder(job, -mean_income), y=mean_income))+
  geom_col()+
  coord_flip()+
  ylim(0,850)

# 성별 직업빈도
job_male <- welfare %>% 
  filter(!is.na(job) & sex=="male") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_male

job_female <- welfare %>% 
  filter(!is.na(job) & sex=="female") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

head(job_female)

job_female

ggplot(data = job_male, aes(x=reorder(job,n), y=n))+
  geom_col()+
  coord_flip()

ggplot(data=job_female, aes(x=reorder(job,n), y=n))+
  geom_col()+
  coord_flip()

job_gender <- welfare %>% 
  filter(!is.na(job)) %>% 
  group_by(job,sex) %>% 
  summarise(n=n()) %>% 
  head(10)

ggplot(data=job_gender, aes(x=reorder(job,n), y=n, fill=sex))+
  geom_col(position="dodge") +
  coord_flip()

ggplot(data=job_gender, aes(x=reorder(job,n), y=n, fill=sex))+
  geom_col() +
  coord_flip()

# 종교 유무에따른 이혼율

class(welfare$religion)
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion==1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)


class(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage==1, "marriage",
                           ifelse(welfare$marriage==3, "divorced", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion,group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct=round(n/sum(n)*100,2))

religion_marriage

divorce <- religion_marriage %>% 
  filter(group_marriage == "divorced") %>% 
  select(religion,pct)

divorce
head(divorce)

ggplot(data=divorce, aes(x=religion, y=pct))+
  geom_col()+
  coord_flip()

ageg_marriage <-  welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,1))

welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg,group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct=round(n/sum(n)*100,2))

ageg_divorce <- ageg_marriage %>% 
  filter(ageg != "young" & group_marriage=="divorced") %>% 
  select(ageg,pct)

ggplot(data = ageg_divorce, aes(x=ageg, y=pct))+
  geom_col()+
  coord_flip()
  
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg!="young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,2))
ageg_religion_marriage

welfare %>% 
  filter(!is.na(group_marriage) & ageg !="young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100,1))

# ageg_religion_marriage <- welfare %>% 
#   filter(!is.na(group_marriage) & ageg!="young") %>% 
#   group_by(ageg, religion, group_marriage) %>% 
#   summarise(n=n(), tot_group=sum(n), pct=round(n/tot_group*100,2))

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage=="divorced") %>% 
  select(ageg,religion,pct)

df_divorce

ggplot(data=df_divorce, aes(x=ageg, y=pct, fill=religion))+
  geom_col(position="dodge")

# 지역별 연령대 비율

class(welfare$code_region)
table(welfare$code_region)
list_region <- data.frame(code_region=c(1:7),
                          region=c("Seoul",
                                  "CapitalRegion",
                                  "PKRegion",
                                  "TKRegion",
                                  "ChungNamDajeon",
                                  "GangwonChungBook",
                                  "HonamJeju"))
list_region

welfare <- left_join(welfare, list_region, id="code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

region_ageg <- welfare %>% 
  group_by(region,ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,3))

head(region_ageg)

region_ageg <- welfare %>% 
  count(region, ageg) %>% 
  group_by(region) %>% 
  mutate(pct = round(n/sum(n)*100,4))

ggplot(data = region_ageg, aes(x = region, y=pct, fill=ageg)) +
  geom_col()+
  coord_flip()
  
list_order_old <- region_ageg %>% 
  filter(ageg=="old") %>% 
  arrange(pct)

list_order_old

order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y=pct, fill=ageg)) +
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = order)

class(region_ageg$ageg)

levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old", "middle", "young"))
class(region_ageg$ageg)
levels(region_ageg$ageg)

ggplot(data = region_ageg, aes(x = region, y=pct, fill=ageg)) +
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = order)
