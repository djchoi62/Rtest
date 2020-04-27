library(tidyverse)
library(readxl)
library(stringr)

# Data import
president <- read_xlsx("President2017raw.xlsx")  

# Modify column
candidate <- president[1, 7:19] %>%
  str_split("\r\n")

i=0
for (i in 1:13) {
  colnames(president)[i+6] =candidate[[i]][2]
} 

colnames(president)[20]="합계"
president <- president[-1,]

write.csv(president,"president_00.csv", row.names = F)

#Preprocessing
president[,5:22] <- data.matrix(president[,5:22])

# colnames(president)
president <- president %>% 
  as_tibble() %>% 
  filter(읍면동명=="합계") %>% 
  dplyr::select(시도명, 구시군명, 문재인, 홍준표, 안철수, 유승민, 심상정, 조원진, 오영국, 장성민, 이재오, 김선동, 이경희, 윤홍식, 김민찬, 합계) %>% 
  mutate(moon=문재인/합계*100) %>% 
  mutate(hong=홍준표/합계*100) %>%
  mutate(ahn=안철수/합계*100) %>%
  mutate(yoo=유승민/합계*100) %>%
  mutate(shim=심상정/합계*100) %>%
  mutate(etc=(조원진+오영국+장성민+이재오+김선동+이경희+윤홍식+김민찬)/합계*100)

write.csv(president,"president_01.csv", row.names = F)

president <- rename(president,
                    sido=시도명,
                    sigun=구시군명)


president <- president %>% 
  dplyr::select(sido, sigun, moon, hong, ahn, yoo, shim, etc)

write.csv(president,"president_02.csv", row.names = F)

president <- president %>% 
  mutate(winN=apply(X=president[,3:8], 1, FUN=function(x) which(x==max(x)))) %>% 
  mutate(winner=ifelse(winN==1, "Moon", 
                       ifelse(winN==2, "Hong",
                              ifelse(winN==3, "Ahn",
                                     ifelse(winN==4, "Yoo",
                                            ifelse(winN==5, "Shim", "ETC")))))) %>% 
  mutate(color=ifelse(winN==1, "blue", 
                      ifelse(winN==2, "red",
                             ifelse(winN==3, "green",
                                    ifelse(winN==4, "grey",
                                           ifelse(winN==5, "yellow", "white"))))))
# president <- president %>% 
#   mutate(SecN=apply(X=president[,3:8], 1, FUN=function(x) which(x==maxn(2))))
# 
# # maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

library(ggmap)
register_google(key="AIzaSyBH2jQQ3scJrWbffPCbFV4b6RdcpNTs6zc")

president <- president %>% 
  mutate(address=paste(sido, sigun, sep = " ")) 

president$areacode=geocode(paste0(president$address, "청"))

write.csv(president,"president_03.csv", row.names = F)

# president <- read.csv("president_03.csv", stringsAsFactors = F)
# 
# rename(president, 
#        lon=areacode.lon,
#        lat=areacode.lat)
# 
# head(president)

id <- read.table("dongcode.txt", sep = "\t")

id$V1 <- as.character(id$V1)
id$V2 <- as.character(id$V2)
id$V3 <- as.character(id$V3)

colnames(id)[1]=id[1,1]
colnames(id)[2]=id[1,2]
colnames(id)[3]=id[1,3]

id <- id[-1,]





install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("viridis")

library(tidyverse)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(viridis)

register_google(key="AIzaSyBH2jQQ3scJrWbffPCbFV4b6RdcpNTs6zc")

background<- get_map(location="south korea", zoom=7, maptype = "roadmap", color = "bw")

poll <- read.csv("president2017.csv", stringsAsFactors = F)

korea <- shapefile("SIG_201703/TL_SCCO_SIG.shp")

# ggplot()+
#   geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill="white", color="black")

korea <- fortify(korea, region="SIG_CD")

korea <- merge(korea, poll, by="id")

View(korea)

ggmap(background)+
  geom_polygon(data = korea, aes(x=long, y=lat, group=group, fill=moon), alpha=0.75)+
  # scale_fill_gradient(low="white", high="blue")
  scale_fill_viridis(direction = -1)+
  theme_void()
