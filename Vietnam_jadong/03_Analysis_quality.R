##########################################
### qualityparm(생산정보)
##########################################



### data cleansing

# column name 정의
str(qual)
colnames(qual) <- c("id", "lotno", "sunbun", "jobdate", "crimpid", "BLO", 
                    "z1_width", "z1_sensi", "z2_width", "z2_sensi", "z3_sensi", 
                    "ip", "computer", "time", "key")

qual <- qual[,-15]  #time column 지우기: 모두 결측치임



# data format change
qual[, 7:12] <- sapply(qual[, 7:12], 
                       function(a){
                         as.numeric(as.character(a))
                       })

qual$crimpid <- as.factor(qual$crimpid)






### qualityparm 기초 분석


# crimpid 별 측정값 기초통계량 확인

tmp <- qual %>% 
  group_by(key, crimpid) %>% 
  summarise(n=n(),
            w1_mean=mean(z1_width, na.rm=TRUE), w1_sd=sd(z1_width, na.rm=TRUE),
            w2_mean=mean(z2_width, na.rm=TRUE), w2_sd=sd(z2_width, na.rm=TRUE),
            s1_mean=mean(z1_sensi, na.rm=TRUE), s1_sd=sd(z1_sensi, na.rm=TRUE),
            s2_mean=mean(z2_sensi, na.rm=TRUE), s2_sd=sd(z2_sensi, na.rm=TRUE),
            s3_mean=mean(z3_sensi, na.rm=TRUE), s3_sd=sd(z3_sensi, na.rm=TRUE),
            b1_mean=mean(BLO, na.rm=TRUE),      b1_sd=sd(BLO, na.rm=TRUE))


# 일자별 측정값 plot
ggplot(qual, aes(x=jobdate, y=z1_width))+
  geom_boxplot()








### qualityparm + 불량률
meas_qual <- merge(qual, m, by='key', all.x=TRUE)
str(meas_qual)








########################
### 측정값 plot
########################
library(ggplot2)
library(stringr)


meas_qual$key2 <- paste(meas_qual$key, meas_qual$sun, sep="_")
meas_qual$keyno <- as.numeric(as.factor(meas_qual$key2))
meas_qual[,19] <- str_trim(meas_qual[,19], side=c("both", "left", "right"))
meas_qual$sun <- paste(meas_qual$www_spc_cd, meas_qual$lcom_dia, sep="_")



### 연구소 기준 - 민감도 그래프

# zone1 민감도 plot
setwd("D:/workspace/zone1민감도")

for(i in 1:9){
  
  tmp <- meas_qual %>% 
    filter(keyno == i) 
  
  sun <- as.character(tmp %>% distinct(sun))
  
  c <- (kijun %>% filter(sun2 == sun))$z1[1]
  
  title <- paste(tmp$key2, "BLO", sep="_")
  
  meas_qual %>% filter(keyno == i) %>% 
    ggplot(aes(x=id, y=BLO, color=ng_rate)) +
    geom_point() +
    geom_hline(yintercept = c, linetype = 'dashed', color = 'blue') +
    #     geom_hline(yintercept = c2, linetype = 'dashed', color = 'blue') +
    scale_color_gradient(low="blue", high="red") +
    annotate("text", x = 1, y = c, label = c) +
    #      annotate("text", x = 1, y = c2, label = c2) +
    ggtitle(title)
  
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}




# zone 별 민감도 plot
setwd("D:/workspace/zone3민감도")

for(i in 1:9){
  
  a <- as.character(kijun$sun2[i])
  
  c <- (kijun %>% filter(sun2 == a))$z3[1]
  
  title <- paste(a, "Z3 민감도", sep="_")
  
  meas_qual %>% filter(sun == a) %>% 
    ggplot(aes(x=id, y=z3_sensi, color=ng_rate)) +
    geom_point() +
    geom_hline(yintercept = c, linetype = 'dashed', color = 'blue') +
    scale_color_gradient(low="blue", high="red") +
    annotate("text", x = 1, y = c, label = paste("Z3",c, sep=": ")) +
    annotate("rect", xmin = 1, xmax = 250000, ymin = c-c*0.1, ymax = c+c*0.1, alpha = 0.3, fill="blue") +
    #      annotate("text", x = 1, y = c2, label = c2) +
    ggtitle(title)
  
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}





# zone 별 민감도 plot
setwd("D:/workspace/zone3민감도")

for(i in 1:9){
  
  a <- as.character(kijun$sun2[i])
  
  c <- (kijun %>% filter(sun2 == a))$BLO[1]
  
  title <- paste(a, "BLO", sep="_")
  
  meas_qual %>% filter(sun == a) %>% 
    ggplot(aes(x=id, y=BLO, color=ng_rate)) +
    geom_point() +
    geom_hline(yintercept = c, linetype = 'dashed', color = 'blue') +
    scale_color_gradient(low="blue", high="red") +
    annotate("text", x = 1, y = c, label = paste("BLO",c, sep=": ")) +
    annotate("rect", xmin = 1, xmax = 544510, ymin = c-10, ymax = c+10, alpha = 0.3, fill="blue") +
    #      annotate("text", x = 1, y = c2, label = c2) +
    ggtitle(title)
  
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}





### zone 기준 별 양불 비율

a <- as.character(kijun$sun2[4])

c <- (kijun %>% filter(sun2 == a))$z1[1]

meas_qual %>% filter(sun == a) %>% 
  filter(z1_sensi >= c) %>% 
  distinct(key, ng_rate) %>% 
  arrange(ng_rate)


i <- meas_qual %>% group_by(key, ng_rate) %>%
  filter(sun == 'CIVUS_0.35') %>% 
  filter(z2_sensi >= 40)  %>% 
  distinct(key)


i2 <- meas_qual %>% group_by(key, ng_rate) %>%
  filter(sun == 'CHFUS_0.22') %>% 
  filter(z2_sensi < 40) %>% 
  distinct(key)

merge(i, i2,
      by = 'ng_rate',
      all = TRUE)







### 불량 상위 5개 선번에 대해 분석 
ng <- c("WL111", "DU27", "YE119", "IC1", "JS559")
meas.sunbun_top5 <- meas %>% 
  filter(sunbun == ng)

meas.sunbun_top5 %>% 
  ggplot(aes(x=id, y=BLO_SDC, color == okng)) +
  geom_point() +
  facet_wrap(~computer)

meas.sunbun_top5 %>% 
  group_by(sunbun) %>% 
  summarise(min = min(BLO_SDC, na.rm=T),
            max = max(BLO_SDC, na.rm=T)) %>% 
  mutate(range = max-min)


qual.ngrate$sunbun <- as.factor(qual.ngrate$sunbun)
qual.ngrate.sunbun_top5 <- qual.ngrate %>% 
  filter(sunbun == c("WL111", "DU27", "YE119", "IC1", "JS559"))

str(qual.ngrate.sunbun_top5)




# 불량 상위 선번 - 단자별 불량률
qual.ngrate.sunbun_top5 %>% ggplot(aes(x=id, y=z1_sensi, color=sunbun))+
  geom_point() +
  facet_wrap(~crimpid)

# 상위 선번에 포함된 단자만 추출

meas_danza <- meas %>% filter(terminalkey %in% qual.ngrate.sunbun_top5$crimpid)

meas_danza %>% ggplot(aes(x=id, y=FP, color=okng))+
  geom_point() +
  facet_wrap(~BLO_SDC)

unique(meas_danza$terminalkey)

