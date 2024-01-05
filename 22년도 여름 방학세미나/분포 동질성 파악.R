library(tidyverse)
library(data.table)
library(magrittr)
library(stringr)

train<-fread('missforest_imputed_train.csv')

colnames<-colnames(train)
col_cat<-c(colnames[which(str_detect(colnames,'cat')==T)])
col_bin<-c(colnames[which(str_detect(colnames,'bin')==T)])
col_num<-c(colnames[which(str_detect(colnames,'cat')==F&
                            str_detect(colnames,'bin')==F)])
col_num<-col_num[-1]

pyo=function(x){
  colname<-col_cat[x]
  table<-train %>% select(colname,target) %>% table %>% t
  return(table)
}

pyo2=function(x){
  colname<-col_bin[x]
  table<-train %>% select(colname,target) %>% table %>% t
  return(table)
}

test=function(x){
  chi<-chisq.test(x)
  return(chi$p.value)
}

# 범주형 변수 분포 동질성 검정 

p_value<-c()

for (i in 1:length(col_cat)){
  p_value<-c(p_value,test(pyo(i)))
}

result<-cbind(col_cat,p_value)

# binary 분포 동질성 비교 

p_value<-c()

for (i in 1:length(col_bin)){
  p_value<-c(p_value,test(pyo2(i)))
}
result2<-cbind(col_bin,p_value)


# numeric 동질성 검정

train0<-train %>% filter(target==0) %>% na.omit
train1<-train %>% filter(target==1) %>% na.omit

p_value<-c()

for (i in 1:length(col_num)){
  zero<-train0 %>% select(col_num[i]) %>% unlist
  one<-train1 %>% select(col_num[i]) %>% unlist
  gof<-ks.test(zero,one)
  p_value<-c(p_value,gof$p.value)

}
result3<-cbind(col_num,p_value) 

distr<-rbind(result,result2,result3) %>% as.data.frame
distr %<>% arrange(p_value)
distr$p_value<-if_else(distr$p_value<0.05,'p-value<0.05',distr$p_value)

