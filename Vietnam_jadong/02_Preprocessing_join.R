######################################################
# �м� ����� key �����͸� ���� 
# table���� ��ġ�ϴ� key ���� - ���̺� �� row ��
######################################################


#### library ��ġ ####
library(dplyr)




#### key ���� ####
measurement$key   <- paste(measurement$mlotno, measurement$msunbun, sep="_")
producti$key      <- paste(producti$lorder, producti$lsunbun, sep="_")
qualityparm$key   <- paste(qualityparm$qlotno, qualityparm$qsunbun, sep="_")
statisticdata$key <- paste(statisticdata$slotno, statisticdata$ssunbun, sep="_")
mes_autodata$key  <- paste(mes_autodata$www_nbr, mes_autodata$xxx_sub, sep="_")



#### ���̺� �� - key ���� �� �� ���ϱ� ####

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





#### ������ ���� ####

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


# ��ü = measurement + producti + qualityparm + statistic + mes
all_mes <- merge(all, mes.uniq,
                 by = 'key',
                 all = FALSE)


# unique�� key(lotno+����)�� �� ���̺� �� ��
# write.csv(all_mes, "S:/vietnam_key_unique.csv", row.names = FALSE)





#### �м� ��� key�� ���ԵǴ� �����͸� �����ϱ� ####
meas <- measurement   %>% filter(key %in% all_mes$key)
stat <- statisticdata %>% filter(key %in% all_mes$key)
prod <- producti      %>% filter(key %in% all_mes$key)
mes  <- mes_autodata  %>% filter(key %in% all_mes$key)
qual <- qualityparm   %>% filter(key %in% all_mes$key)




#### ������ ���� ####
colnames(meas) <- c("id", "lotno", "sunbun", "jobdatetime", "terminalkey", "end", "jobno",
                    "okng", "BLO_SDC", "RDI", "FP", "ip", "computer", "wtime", "key")


colnames(qual) <- c("id", "lotno", "sunbun", "jobdate", "crimpid", "BLO", 
                    "z1_width", "z1_sensi", "z2_width", "z2_sensi", "z3_sensi", 
                    "ip", "computer", "time", "key")
