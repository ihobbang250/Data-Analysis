library(data.table)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load( "coffee_shop.rdata")
data1 <- read.csv("LOCAL_PEOPLE_GU_2021.csv")
people <- data1 %>% select(기준일ID, 시간대구분, 자치구코드, 총생활인구수)
new_people <- aggregate(people$총생활인구수, by = list(people$자치구코드), FUN = mean)
data2 <- fread("localcode.csv", encoding = "UTF-8")
new_data2 <- data2[data2$RESD_DO_NM == "서울"]
new_data2 <- new_data2 %>% rename ("자치구코드" = "RESD_DO_NM" )
final <- merge(new_people, new_data2, by.x = 'Group.1',  by.y = 'RESD_CD', all = T)
final <- final[-c(1, 3)]
final <- final %>% rename ("자치구" = "RESC_CT_NM")
final <- final %>% rename ("평균생활인구" = "x" )

new <- coffee_shop %>% separate('juso', c("시", "구"),  sep = " ")
new <- new[c(4)]
new[, "count"] <- rep(1)
y <- aggregate(new$count, by = list(new$구), FUN = sum)
y <- y %>% rename("자치구" = "Group.1")
y <- y %>% rename("점포수" = "x")
final <- merge(final, y, all = TRUE)

price <- read.csv("공시지가_2021년.csv")
price <- price[c(2,5)]
price <- aggregate(price$공시지가.원..., by = list(price$시군구명), FUN = mean)
price <- price %>% rename("자치구" = "Group.1")
price <- price %>% rename("평균공시지가" = "x")
final <- merge(final, price, all = T)

station <- read.csv("station.csv")
station <- station %>% separate('구주소', c("시", "구"), sep = " ")
station <- station[c(4)]
station[, "count"] <- rep(1)
station <- aggregate(station$count, by = list(station$구), FUN = sum)
station <- station %>% rename("자치구" = "Group.1")
station <- station %>% rename("지하철역수" = "x")
final <- merge(final, station, all = T)
final <- na.omit(final)

inc_change <- read.csv("inc_change.csv")
inc_change <- inc_change[inc_change$기준_년_코드 == "2021",]
inc_change <- inc_change[c(1, 4, 5)]
inc_change$상권_변화_지표 <- dplyr::recode(inc_change$상권_변화_지표, "LL" = 1, "LH" = 2, "HL" = 3, "HH" = 4)
inc_change <- inc_change[-c(1)]
inc_change <- aggregate(inc_change$상권_변화_지표, by = list(inc_change$시군구_코드_명), FUN = mean)
inc_change <- inc_change %>% rename("자치구" = "Group.1")
inc_change <- inc_change %>% rename("상권변화지표" = "x")
final <- merge(final, inc_change, all = T)

inc_enviro <- fread("inc_enviro.csv", encoding = "UTF-8")
inc_enviro <- inc_enviro %>% rename("자치구" = "V1")
inc_enviro <- inc_enviro %>% rename("만족도지표" = "V2")
final <- merge(final, inc_enviro, all = T)

gender <- fread("gender.csv", encoding = "UTF-8")
ageband <- fread("ageband.csv", encoding = "UTF-8")
gender <- gender %>% rename("자치구" = "V1")
gender <- gender %>% rename("성비" = "V2")
ageband <- ageband[,-2]
head(ageband, 25)
ageband <- ageband %>% rename("자치구" = "V1")
ageband <- ageband %>% rename("노령화지표" = "V3")
head(gender)

final1 <- merge(gender, ageband, all = T)
final <- merge(final, gender, all = T)
head(final)

write.csv(final, file = "final.csv")
check <- read.csv("final.csv")
check <- check[-c(9)]
check <- check[-c(2)]
check <- merge(check, ageband, all = T)
write.csv(check, file = "check.csv")
