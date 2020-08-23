#### library 설치 ####
library(dplyr)


#### 분석 대상 일자 추출 ####

# 2019/04/01~2019/05/10
measurement <- measurement %>% 
  filter(mjobdate >= '2019-04-01' & mjobdate < '2019-08-16')

producti <- producti %>% 
  filter(ljobdate >= '2019-04-01' & ljobdate < '2019-08-16')

qualityparm <- qualityparm %>% 
  filter(qjobdate >= '2019-04-01' & qjobdate < '2019-08-16')

statisticdata <- statisticdata %>% 
  filter(sjobdate >= '2019-04-01' & sjobdate < '2019-08-16')

mes_autodata <- mes_autodata %>% 
  filter(fdate >= '2019-04-01' & fdate < '2019-08-22')
