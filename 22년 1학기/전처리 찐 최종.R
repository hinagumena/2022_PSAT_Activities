library(tidyverse)
library(data.table)
library(magrittr)
library(lubridate)
#setwd("C:/Users/shn20/Desktop/작업물/통계분석학회/주제 분석")
setwd("C:/Users/shn20/Desktop/작업물/통계분석학회/주제 분석/전처리")

#불러오기
data<-fread("kbl_2021_플오_선수_전처리_2차.csv",encoding='UTF-8')
data %>% as.data.frame
data$Min<-data$m*60+data$s
data$name<-ifelse(data$name=='리카르도 라틀리프','라건아',data$name)
data$team<-ifelse(data$team=='인천 전자랜드 엘리펀츠','대구 한국가스공사 페가수스',data$team)
data$team<-ifelse(data$team=='부산 KT 소닉붐','수원 KT 소닉붐',data$team)
data<-data %>% select(-m,-s)

data$`2PT성공` %<>% as.numeric
data$`2PT시도` %<>% as.numeric
data$`3PT성공` %<>% as.numeric
data$`3PT시도` %<>% as.numeric
data$FT성공 %<>% as.numeric
data$FT시도 %<>% as.numeric

data$Year<-year(ymd(data$Date))
data$Month<-month(ymd(data$Date))
y<-ymd(data$Date)
z<-day(y)
data$Day<-z

train<-data %>% filter(ymd(data$Date)<ymd('2021-10-09'))
test<-data %>% filter(ymd(data$Date)>=ymd("2021-10-09"))

train<-train %>% select(-Date,-player_num)
test<-test %>% select(-Date,-player_num)
write.csv(data,"전처리 완료2.csv",fileEncoding='UTF-8')