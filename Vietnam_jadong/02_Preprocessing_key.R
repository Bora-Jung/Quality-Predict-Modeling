library(reshape2)

### key 별 불량률
# 불량률 = measurement의 key 별 ng비율 (ng개수/key개수)

meas.ngrate <- meas
meas.ngrate <- meas %>% 
  group_by(key, okng) %>% 
  summarise(n=n())

meas.ngrate <- meas.ngrate %>% 
  group_by(key) %>%
  mutate(odds = n/sum(n))


meas.ngrate <- dcast(meas.ngrate, key ~ okng)

colnames(meas.ngrate) <- c("key", "false_rate", "true_rate")
meas.ngrate$ng_rate <- 1 - meas.ngrate$true_rate
meas.ngrate <- meas.ngrate[,c(1,4)]



# 각 테이블에 불량률 조인
meas <- merge(meas, meas.ngrate,
              by = "key",
              all.x = TRUE)

qual <- merge(qual, meas.ngrate,
              by = "key",
              all.x = TRUE)

prod <- merge(prod, meas.ngrate,
              by = "key",
              all.x = TRUE)

stat <- merge(stat, meas.ngrate,
              by = "key",
              all.x = TRUE)


### mes 데이터랑 병합

### meas + mes
m <- mes %>% distinct(key, www_nbr, xxx_sub, www_spc_cd, lcom_color, lcom_dia, www_terminal1, www_terminal2)
meas_mes <- merge(meas, m, 
                  by = 'key',
                  all = TRUE)
m <- c()

meas_mes$key2 <-  paste(meas_mes$key, meas_mes$www_spc_cd, meas_mes$lcom_dia, meas_mes$lcom_color, sep='_')

meas_mes$key3 <- paste(meas_mes$key, meas_mes$www_spc_cd, meas_mes$lcom_dia, meas_mes$lcom_color, 
                       meas_mes$mterminalkey, sep='_')



colnames(meas_mes) <- c("lotno_sunbun", "id", "lotno", "sunbun", "jobdatetime", "terminalkey", "end", "jobno",
                        "okng", "BLO_SDC", "RDI", "FP", "ip", "computer", "wtime", "ng_rate",
                        "lotno2", "sunbun2", "sunjong", "suncolor", "sunwidth", "terminal1", "terminal2",
                        "key", "key2")





meas_mes$no <- as.numeric(as.factor(meas_mes$key))
meas_ng <- meas_mes %>% filter(ng_rate != 0)
meas_ok <- meas_mes %>% filter(ng_rate == 0)
meas_ng$no <- as.numeric(as.factor(meas_ng$key))
meas_ok$no <- as.numeric(as.factor(meas_ok$key))


