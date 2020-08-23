#### library 설치 ####
install.packages("rJava", "DBI", "RMySQL")

library(DBI)
library(RMySQL)
library(dplyr)

#### DB CONNECT ####
con <- dbConnect(MySQL(), user="riskmg", password="riskmg", dbname="riskmg",
                 host="172.21.0.153")


dbListTables(con)


#### data input ####

# measurement : 측정정보
measurement <- con %>% dbGetQuery("select * from measurement where mjobdate >= '2019-04-01'")

# producti : 생산실적
producti <- con %>% dbGetQuery("select * from producti where ljobdate >= '2019-04-01'")

# qualityparm : 단자압착 품질데이터
qualityparm <- con %>% dbGetQuery("select * from qualityparm where qjobdate >= '2019-04-01'")

# statistic : 품질관련 통계데이터
statisticdata <- con %>% dbGetQuery("select * from statisticdata where sjobdate >= '2019-04-01'")

# mes_autodata : MES(POP) 데이터의 일부
mes_autodata <- con %>% dbGetQuery("select * from mes_autodata where fdate >= '2019-04-01'")


# false data : gate test 불량 데이터(MES)
library(readxl)
false_data1 <- read_xlsx("D:/workspace/베트남(설비데이터)/dataset/베트남가공_불량리스트_190401~190510.xlsx",
                        sheet = "Sheet1")

false_data2 <- read_xlsx("D:/workspace/베트남(설비데이터)/dataset/베트남가공_불량리스트_190701~190816.xlsx",
                        sheet = "Sheet1")

false_data3 <- read_xlsx("D:/workspace/베트남(설비데이터)/dataset/베트남가공_불량리스트_190816~191107.xlsx",
                        sheet = "Sheet1")

false_data4 <- read_xlsx("D:/workspace/베트남(설비데이터)/dataset/베트남가공_불량리스트_190510~190630.xlsx",
                         sheet = "Sheet1")

library(dplyr)
false_data1 <- false_data1 %>% select(LOT, 선번, 재질, 선경, 색상, 품번, 작업호기, 검사수량, 불량수량, 
                                      터미널, 작업일자, 발생장소)

false_data2 <- false_data2 %>% select(LOT, 선번, 재질, 선경, 색상, 품번, 작업호기, 검사수량, 불량수량, 
                                      터미널, 작업일자, 불량금형)

false_data3 <- false_data3 %>% select(LOT, 선번, 재질, 선경, 색상, 품번, 작업호기, 검사수량, 불량수량, 
                                      터미널, 작업일자, 불량금형)

false_data4 <- false_data4 %>% select(LOT, 선번, 재질, 선경, 색상, 품번, 작업호기, 검사수량, 불량수량, 
                                      터미널, 작업일자, 불량금형)

colnames(false_data1)[12] <- "불량금형"


false_data <- rbind(false_data1, false_data2)
false_data <- rbind(false_data, false_data3)
false_data <- rbind(false_data, false_data4)



# errorldc : 설비 알람 코드 
errorldc <- read.csv("S:/정보라/유라테크/새 폴더/errorldc_1904.csv", header=T)


# kijun : 연구소 선종/선경 별 BLO 기준값
kijun <- read.csv("D:/workspace/유라테크(베트남)/dataset/BLO기준.csv", header=TRUE)



tmp <- read.csv("S:/talk3/게이트검사이력0701~0816(사번추가).csv", header=TRUE)
