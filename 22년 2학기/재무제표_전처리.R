library(data.table)
library(magrittr)
library(tidyverse)


report<-fread("2021재무제표.csv")
worker<-fread('업종별_시총_사업계획서(직원).csv')

colreset<-function(x){
  x %<>% select(-c(V1)) %>% 
    rename("접수번호"="rcept_no",
           "보고서코드"="reprt_code",
           "사업연도"="bsns_year",
           "고유번호"="corp_code",
           "재무제표구분"="sj_div",
           "재무제표명"="sj_nm",
           "계정ID"="account_nm",
           "당기명"="thstrm_nm",
           "당기기간"="thstrm_dt",
           "당기금액"="thstrm_amount",
           "전기명"="frmtrm_nm",
           "전기기간"="frmtrm_dt",
           "전기금액"="frmtrm_amount",
           "전전기명"="bfefrmtrm_nm",
           "전전기기간"="bfefrmtrm_dt",
           "전전기금액"="bfefrmtrm_amount",
           "계정과목정렬순서"="ord",
           "통화 단위"="currency")
  return(x)}

report %<>% colreset
report$fs_nm %>% table
report %<>% filter(report$fs_nm=='재무제표') %>% 
  mutate_at(vars(당기금액,전기금액,전전기금액),as.numeric)

report2021<-report %>% 
  select(c(고유번호,계정ID,당기기간,당기금액)) %>% 
  spread(key='계정ID',value='당기금액') %>% 
  rename('2021당기순이익'='당기순이익',
         '2021매출액'='매출액',
         '2021법인세차감전순이익'='법인세차감전 순이익',
         '2021영업이익'='영업이익')

report2020<-report %>% 
  select(c(고유번호,계정ID,전기기간,전기금액)) %>% 
  spread(key='계정ID',value='전기금액') %>% 
  rename('2020당기순이익'='당기순이익',
         '2020매출액'='매출액',
         '2020법인세차감전순이익'='법인세차감전 순이익',
         '2020영업이익'='영업이익')
  
  
report2019<-report %>% 
  select(c(고유번호,계정ID,전전기기간,전전기금액)) %>%
  spread(key='계정ID',value='전전기금액') %>% 
  rename('2019당기순이익'='당기순이익',
         '2019매출액'='매출액',
         '2019법인세차감전순이익'='법인세차감전 순이익',
         '2019영업이익'='영업이익')

report_3years<-list(report2021,report2020,report2019)

report_all<-plyr::join_all(report_3years,by='고유번호',type='full')

interested<-fread('우리가_관심있는_기업들.csv')

top100<-fread('업종별_시총_상위_10위_기업.csv',encoding='UTF-8')
top100_2<-left_join(top100,interested,
                    by=c('회사명'='회사명',
                         '종목코드'='종목코드'))
report_sum<-left_join(top100_2,report_all,by='고유번호')
report_final<-left_join(report_sum,worker,
                        by=c('회사명'='corp_name',
                             '고유번호'='corp_code'))
write.csv(report_final,'기업정보_전처리_1차_완료.csv')
