######################################################
# 분석 대상인 key 데이터만 추출 
# table에서 일치하는 key 추출 - 테이블 별 row 수
######################################################


#### library 설치 ####
library(dplyr)




#### key 생성 ####
measurement$key   <- paste(measurement$mlotno, measurement$msunbun, sep="_")
producti$key      <- paste(producti$lorder, producti$lsunbun, sep="_")
qualityparm$key   <- paste(qualityparm$qlotno, qualityparm$qsunbun, sep="_")
statisticdata$key <- paste(statisticdata$slotno, statisticdata$ssunbun, sep="_")
mes_autodata$key  <- paste(mes_autodata$www_nbr, mes_autodata$xxx_sub, sep="_")



#### 테이블 별 - key 별로 행 수 구하기 ####

# measurement
measurement$mvalue4 <- as.numeric(measurement$mvalue4)

measurement.uniq <- measurement %>% 
  group_by(key) %>% 
  summarise(value = mean(mvalue4, na.rm = T), measurement_n = n())


# producti
producti.uniq <- producti %>% 
  group_by(key) %>% 
  summarise(time = sum(ljobtime, na.rm = T), producti_n = n())


# qualityparm
qualityparm$qvalue2 <- as.numeric(qualityparm$qvalue2)

qualityparm.uniq <- qualityparm %>% 
  group_by(key) %>% 
  summarise(value = mean(qvalue2, na.rm = T), quality_n = n())


# statistic
statisticdata$scp1value1 <- as.numeric(statisticdata$scp1value1)

statisticdata.uniq <- statisticdata %>% 
  group_by(key) %>% 
  summarise(value = mean(scp1value1, na.rm = T), stat_n = n())


# mes
mes.uniq <- mes_autodata %>% 
  group_by(key) %>% 
  summarise(mes_n = n())





#### 데이터 병합 ####

# t = measurement + producti
t <- merge(measurement.uniq, producti.uniq,
           by = 'key', 
           all = FALSE)

# tt = qualityparm + statistic
tt <- merge(qualityparm.uniq, statisticdata.uniq,
            by = 'key',
            all = FALSE)

# t+tt = measurement + producti + qualityparm + statistic
all <- merge(t, tt,
             by = 'key',
             all = FALSE)


# 전체 = measurement + producti + qualityparm + statistic + mes
all_mes <- merge(all, mes.uniq,
                 by = 'key',
                 all = FALSE)


# unique한 key(lotno+선번)별 각 테이블 행 수
# write.csv(all_mes, "S:/vietnam_key_unique.csv", row.names = FALSE)





#### 분석 대상 key가 포함되는 데이터만 추출하기 ####
meas <- measurement   %>% filter(key %in% all_mes$key)
stat <- statisticdata %>% filter(key %in% all_mes$key)
prod <- producti      %>% filter(key %in% all_mes$key)
mes  <- mes_autodata  %>% filter(key %in% all_mes$key)
qual <- qualityparm   %>% filter(key %in% all_mes$key)




#### 변수명 정의 ####
colnames(meas) <- c("id", "lotno", "sunbun", "jobdatetime", "terminalkey", "end", "jobno",
                    "okng", "BLO_SDC", "RDI", "FP", "ip", "computer", "wtime", "key")


colnames(qual) <- c("id", "lotno", "sunbun", "jobdate", "crimpid", "BLO", 
                    "z1_width", "z1_sensi", "z2_width", "z2_sensi", "z3_sensi", 
                    "ip", "computer", "time", "key")

