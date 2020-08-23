
### data input
tasu <- read.csv("D:/workspace/유라테크(베트남)/타수관련 데이터/베트남 autodata_1.csv", header = T)
kh_master <- read.csv("D:/workspace/유라테크(베트남)/타수관련 데이터/베트남 금형 마스터.csv", header = T)
kh <- read.csv("D:/workspace/유라테크(베트남)/타수관련 데이터/베트남 금형타수.csv", header = T)


library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)


### data cleaning

# 공백 제거
for(i in 1:ncol(kh)){
  kh[,i] <- str_trim(kh[,i], side=c("both", "left", "right"))
}

for(i in 1:ncol(kh_master)){
  kh_master[,i] <- str_trim(kh_master[,i], side=c("both", "left", "right"))
}

for(i in 1:ncol(tasu)){
  tasu[,i] <- str_trim(tasu[,i], side=c("both", "left", "right"))
}



# 호기 중 A로 시작하는거만 추출
tasu <- tasu %>% filter(substr(sil_mch, 1, 1) == 'A')


# KEY 생성 = lotno + 선번
tasu$key <- paste(tasu$www_nbr, tasu$xxx_sub, sep="_")


### mes데이터 + 금형데이터

# autodata 단자 추출
auto <- meas_uniq %>% select(key, terminal1, terminal2, ng_rate)
auto <- melt(auto,
             id.vars = c("key", "ng_rate"),
             measure.vars = c("terminal1", "terminal2"))
auto <- unique(auto)
colnames(auto) <- c("key", "ng_rate", "terminal", "app_code")
auto$app_code <- substr(auto$app_code, 1, 10)


# 금형마스터 / 금형 중복된게있음
# measurement date와 chg_date 차이가 적은 금형데이터로 추출
meas_date <- meas[,c(1,5)]
meas_date$dt <- substr(meas_date$jobdatetime, 1, 10)
meas_date <- meas_date[,-c(2)]
meas_date <- unique(meas_date)

auto <- merge(auto, meas_date,
              by = "key",
              all.x = TRUE)

auto <- auto %>% group_by(key, terminal) %>% 
  arrange(key, terminal, desc(dt)) %>% 
  dplyr::mutate(numbering = order(desc(dt)))

# 가장 마지막 날짜만 추출
auto <- auto %>% filter(numbering == 1) 




# 금형 데이터 + 마스터데이터 병합
kh <- kh[,c(1,2,4:8)]
kh_master <- kh_master[,c(1,2,5:8)]

kh$key <- paste(kh$app_code, kh$jig_code, sep="_")
kh_master$key <- paste(kh_master$app_code, kh_master$jigcode, sep="_")

tmp <- merge(kh, kh_master,
             by = "key")
tmp <- unique(tmp)

tmp <- tmp[,-c(9,10)]
colnames(tmp)[1:3] <- c("key", "app_code", "jig_code")

kh_all <- merge(auto, tmp,
                by = "app_code",
                all.x = TRUE)

kh_all <- kh_all[,-c(6,7)]
kh_all$chdt <- substr(kh_all$chg_date, 1, 10)
colnames(kh_all)[2] <- "key"


kh_all$diff <- as.Date(kh_all$chg_date) - as.Date(kh_all$dt)

# 결측치 제거
kh_all <- kh_all %>% filter(!is.na(jig_code))

# 날짜 차이가 작은 변수 별 순번 
kh_all <- kh_all %>% dplyr::group_by(key, terminal, app_code, jig_code) %>% 
  arrange(key, terminal, app_code, jig_code, diff) %>% 
  dplyr::mutate(numbering = order(diff))


# 날짜 차이가 가장 작은 금형 데이터만 추출
kh_all <- kh_all %>% filter(numbering == 1)


kh <- kh %>% dplyr::group_by(app_code, jig_code) %>% 
  dplyr::mutate(numbering = order(order(app_code)))

kh <- kh[order(kh$app_code, kh$jig_code, kh$chg_date),]

# key 별 넘버링
kh_all$tmp <- paste(kh_all$key, kh_all$app_code, sep="_")
kh_all$numbering <- as.numeric(as.factor(kh_all$tmp))

colnames(kh_all)[1] <- "lotno_sunbun"
kh_all$key <- paste(kh_all$app_code, kh_all$jig_code, sep="_")


###########################################
### 금형 별 타수 데이터 plot

setwd("D:/workspace/kh_jig2")


for(i in 1:max(kh_all$numbering)) {
    tmp1 <- kh_all %>% filter(numbering == i) %>% 
      select(key, app_code, jig_code, lotno_sunbun, chktasu, chktasu2, chktasu3, chgtasu,
                             cur_tasu, last_tasu, ng_rate, numbering)
    
    tmp2 <- melt(data = tmp1,
                 id.vars = c("key", "lotno_sunbun", "app_code", "jig_code"),
                 measure.vars = c("chktasu", "chktasu2", "chktasu3", "chgtasu"))
    
    tmp2 <- merge(tmp2, tmp1,
                  key = "key",
                  all.x = TRUE)
    
    for(j in 6:12){
      tmp2[,j] <- as.numeric(tmp2[,j])
    }
    
    # graph
    title <- paste0(tmp2$lotno_sunbun[1], "_", tmp2$app_code[1], "  불량률 = ", tmp2$ng_rate[1])
    
    tmp2$jig_code <- as.factor(tmp2$jig_code)
    
    tmp2 %>% ggplot() +
      geom_bar(aes(x = jig_code, y = cur_tasu, fill = "#ed5f55"),
               stat = "identity") +
      geom_text(aes(x = jig_code, y = cur_tasu, label=cur_tasu), 
                position = position_dodge(width = 0.9), vjust = 0,
                size = 2.75, colour = "black") +
      geom_point(aes(x = jig_code, y = value, colour = variable),
                 #pch = "-", 
                 size = 3,  stat = "identity") +
      scale_colour_manual(values = c("chktasu" = "#8abcb6",
                                     "chktasu2" = "#77a39e",
                                     "chktasu3" = "#618783",
                                     "chgtasu" = "#476360")) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "none",
            axis.title.x = element_blank()) +   
      labs(title = title) +
      theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
            text = element_text(family="Tahoma"),
            axis.text.x = element_text(colour="black", size = 10),
            axis.text.y = element_text(colour="black", size = 10),
            legend.key = element_rect(fill="white", colour="white"))
    
    
      filename <- paste0(title, ".", "jpg")
    
      ggsave(file=filename, width=20, height=15, units=c("cm"))

  
}



### 체크타수보다 현재타수가 더 클 경우?
kh_all$cur_tasu <- as.numeric(kh_all$cur_tasu)
kh_all$chgtasu <- as.numeric(kh_all$chgtasu)
kh_all$chktasu <- as.numeric(kh_all$chktasu)
kh_all$chktasu2 <- as.numeric(kh_all$chktasu2)
kh_all$chktasu3 <- as.numeric(kh_all$chktasu3)



temp0 <- kh_all %>% filter(cur_tasu < chktasu)
temp1 <- kh_all %>% filter(cur_tasu > chktasu) 
temp2 <- kh_all %>% filter(cur_tasu > chktasu2) 
temp3 <- kh_all %>% filter(cur_tasu > chktasu3) 
temp4 <- kh_all %>% filter(cur_tasu > chgtasu) 

temp1 <- setdiff(temp1, temp2)
temp2 <- setdiff(temp2, temp3)
temp3 <- setdiff(temp3, temp4)

temp4 %>% group_by(key) %>% summarise(mean = mean(ng_rate))


kh_all$tasudiff <- kh_all$chgtasu - kh_all$cur_tasu
kh_all$tasuratio <- kh_all$cur_tasu/kh_all$chktasu2

cor(kh_all$tasuratio, kh_all$ng_rate)
plot(kh_all$tasuratio, kh_all$ng_rate)

ggplot(kh_all, aes(x=tasuratio, y=ng_rate, color=ng_rate))+
  geom_point()+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank()) 









### 추가 20191115
kk <- kh_all[,-c(15, 16,18)]

kk$last_tasu <- as.numeric(kk$last_tasu)
kk$chg_tasu <- as.numeric(kk$chg_tasu)
kk$chg_cnt <- as.numeric(kk$chg_cnt)
kk$chgtasu <- as.numeric(kk$chgtasu)
kk$chktasu2 <- as.numeric(kk$chktasu2)
kk$chktasu3 <- as.numeric(kk$chktasu3)

kk <- kk[,-15]
kk <- kk[,-c(5,6)]
kk <- kk[,-c(4,8,9)]
kk <- kk[,-2]
kk <- kk[,-1]

kk$rate1 <- kk$last_tasu/kk$chktasu2
kk$rate2 <- kk$last_tasu/kk$chktasu3
kk$rate3 <- kk$last_tasu/kk$chgtasu

cor(kk)
summary(kk)


kk_ng <- kk %>% filter(ng_rate > 0)
cor(kk_ng)

kk2 <- kk[,c(1,2,3,5,8,9,10)]



### correlation
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# 원본 데이터 correlation
chart.Correlation(kk2, histogram=TRUE, col="grey10", pch=1)

library(GGally)
ggcorr(kk2, name="corr", label=T)
