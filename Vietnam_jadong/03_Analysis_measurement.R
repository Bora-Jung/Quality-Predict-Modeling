####################################
### measurement(측정정보)
####################################




### 변수 조정 

# 변수명 변경
#colnames(meas) <- c("id", "lotno", "sunbun", "jobdatetime", "terminalkey", "end", "jobno",
#                    "okng", "BLO_SDC", "RDI", "FP", "ip", "computer", "wtime", "key")


# measurement data format change
str(meas)

for(i in 10:12){
  meas[,i] <- as.numeric(meas[,i])}

meas$okng <- as.factor(meas$okng)




for(i in 10:12){
  meas_mes[,i] <- as.numeric(meas_mes[,i])}




################
### meas + mes
################

colnames(meas_mes) <- c("key", "id", "lotno", "sunbun", "jobdatetime", "terminalkey", "end", "jobno",
                        "okng", "BLO_SDC", "RDI", "FP", "ip", "computer", "wtime", "ng_rate",
                        "lotno2", "sunbun2", "sunjong", "suncolor", "sunwidth", "terminal1", "terminal2", "hogi")


# key 별 관련정보
mm <- meas_mes %>% select(lotno_sunbun, sunjong, suncolor, sunwidth, terminal1, terminal2, ng_rate)
mm <- mm %>% distinct(lotno_sunbun, sunjong, suncolor, sunwidth, terminal1, terminal2, ng_rate)



############################
### 측정값 PLOT
############################

### 1. FP plot
setwd("D:/workspace/FPgraph")


# 1) 모두양품인 경우 
n_ok <- length(unique(meas_ok$key))


for(i in 1:n_ok) {
  
  title <- meas_ok %>% 
    filter(no == i) %>% 
    select(key) %>% distinct(key)
  
  
  meas_ok %>% filter(no == i) %>% 
    ggplot(aes(x = id, y = FP, color = okng)) +
    geom_point(size=3) +
    scale_color_manual(values = c("#87CEFF")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    ggtitle(title) 
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}



# 2) 불량이 있는 경우
n_ng <- length(unique(meas_ng$key))

for(i in 1:n_ng) {
  
  temp <- meas_ng %>% 
    filter(no == i) %>% 
    select(key, ng_rate) %>% distinct(key, ng_rate)
  
  title <- paste(temp$key, temp$ng_rate, sep="  불량률 - ")
  
  meas_ng %>% filter(no == i) %>% 
    ggplot(aes(x = id, y = FP, color = okng)) +
    geom_point(size=3) +
    scale_color_manual(values = c("#CE357A", "#87CEFF")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    ggtitle(title) 
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}




# FP 양/불별 통계량
FP_summary <- meas_mes %>% 
      group_by(key, okng) %>% summarise(min = min(FP, na.rm=T),
                                        max = max(FP, na.rm=T),
                                        mean = mean(FP, na.rm=T),
                                        #mid = median(FP, na.rm=T),
                                        sd = sd(FP, na.rm=T),
                                        n=n())

FP_summary <- FP_summary %>% filter(!is.na(mean)) #결측치인값 제거
FP_summary$sd <- ifelse(is.na(FP_summary$sd), 0, FP_summary$sd)
FP_summary <- FP_summary %>% filter(n != '1')


setwd("S:/")
# 양품/불량 boxplot
name <- "FP_mean.jpg"

meas_mes %>% ggplot(aes(x = okng, y = FP, color = okng)) +
  geom_boxplot() +
  scale_color_manual(values = c("#CE357A", "#87CEFF")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  ggsave(file=name, width=20, height=15, units=c("cm"))


meas_mes %>% group_by(okng) %>% 
  summarise(mean = mean(FP, na.rm = T),
            min = min(FP, na.rm = T),
            max = max(FP, na.rm = T))

model <- glm(meas_mes$ng_rate ~ meas_mes$FP)
(summary(model))


# 양품/불량 편차 boxplot
name <- "FP_sd.jpg"

FP_summary %>% ggplot(aes(x = okng, y = sd, color = okng)) +
  geom_boxplot() +
  scale_color_manual(values = c("#CE357A", "#87CEFF")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  ggsave(file=name, width=20, height=15, units=c("cm"))



FP_summary %>% group_by(okng) %>% 
  summarise(mean = mean(sd, na.rm = T),
            min = min(sd, na.rm = T),
            max = max(sd, na.rm = T))

model <- glm(FP_summary$okng ~ FP_summary$sd)
(summary(model))







### 2. BLO plot
setwd("D:/workspace/BLOgraph")



# 1) 모두양품인 경우 

for(i in 1:n_ok) {
  
  title <- meas_ok %>% 
    filter(no == i) %>% 
    select(key) %>% distinct(key)
  
  
  meas_ok %>% filter(no == i) %>% 
    ggplot(aes(x = id, y = BLO_SDC, color = okng)) +
    geom_point(size=3) +
    scale_color_manual(values = c("#87CEFF")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    ggtitle(title) 
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}


# 2) 불량이 있는 경우

for(i in 1:n_ng) {
  
  temp <- meas_ng %>% 
    filter(no == i) %>% 
    select(key, ng_rate) %>% distinct(key, ng_rate)
  
  title <- paste(temp$key, temp$ng_rate, sep="  불량률 - ")
  
  meas_ng %>% filter(no == i) %>% 
    ggplot(aes(x = id, y = BLO_SDC, color = okng)) +
    geom_point(size=3) +
    scale_color_manual(values = c("#CE357A", "#87CEFF")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    ggtitle(title) 
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}








### 3. 연구소 기준과 자동기 BLO 값 비교
#      ㄴkijun 테이블(연구소 기준) 과 비교


# meas_mes에서 필요 변수만 추출

meas_mes$sun2 <- paste(meas_mes$sunjong, meas_mes$sunwidth, sep="_")


# 선종 별 - 연구소 기준 vs 자동기 BLO plot
setwd("D:/workspace/BLO_kijun_plot")
library(ggplot2)
a <- c()
c <- c()

for(i in 1:9){
  
  a <- as.character(kijun$sun2[i])
  
  c <- (kijun %>% filter(sun2 == a))$BLO[1]
  
  title <- paste(a, "BLO_real", sep="_")
  
  meas_mes %>% filter(sun2 == a) %>% 
    ggplot(aes(x=id, y=BLO_SDC, color=okng)) +
    geom_point() +
    geom_hline(yintercept = c, linetype = 'dashed', color = 'blue') +
    scale_color_manual(values = c("#CE357A", "#87CEFF")) +
    annotate("text", x = 1, y = c, label = paste("BLO",c, sep=": ")) +
    annotate("rect", xmin = 1, xmax = 3600692, ymin = c-10, ymax = c+10, alpha = 0.3, fill="blue") +
    #      annotate("text", x = 1, y = c2, label = c2) +
    ggtitle(title)
  
  
  filename <- paste0(title, ".", "jpg")
  
  ggsave(file=filename, width=20, height=15, units=c("cm"))
  
}




# 기초통계
blo_summary <- m %>% group_by(key, okng) %>% 
        summarise(min = min(BLO_SDC, na.rm=T),
                  max = max(BLO_SDC, na.rm=T)) %>% 
        mutate(range = max-min)








###################################
### mes data
##################################

### 1. 선종/선경 별 불량률

# plot
meas2 %>% ggplot(aes(x=sun, y=ng_rate, color=sun))+
  geom_boxplot() +
  xlab("선종/선경") +
  ylab("NG 발생 비율") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))



# 기초통계량
sun2_summary <- meas2 %>% group_by(sun2) %>% 
  summarise(mean = mean(ng_rate, na.rm=T),
            sd   = sd(ng_rate, na.rm=T),
            min  = min(ng_rate, na.rm=T),
            max  = max(ng_rate, na.rm=T),
            n = n())



# 선종/선경 별 불량률 차이
model <- glm(meas2$ng_rate ~ meas$sun2)
(summary(model))

require(nparcomp)

result = mctp(ng_rate ~ sun2, data=meas2)
summary(result)





### 2. 선종/선경/색상 별 불량률

# 선종/선경/색상 별 불량률
meas_mes$sun <- paste0(meas_mes$sunjong, paste= "_", meas_mes$sunwidth)

sun_summary <- meas_mes %>% group_by(sun) %>% 
  summarise(mean = mean(ng_rate, na.rm=T),
            sd   = sd(ng_rate, na.rm=T),
            min  = min(ng_rate, na.rm=T),
            max  = max(ng_rate, na.rm=T),
            n = n())


#plot
meas_mes %>% ggplot(aes(x=sun, y=ng_rate, color=sun))+
  geom_boxplot() +
  xlab("선종") +
  ylab("NG 발생 비율") +
  theme(legend.position = "none")
      #, axis.text.x = element_text(angle = 90))

# anova
model <- glm(meas_mes$ng_rate ~ meas_mes$sun)
(summary(model))







### 3. 호기별 - 선종/선경 차이 비교

# 호기 변경(mes 호기랑 똑같이 변경)
meas_mes$computer <- as.numeric(meas_mes$computer)
meas_mes$hogi <- ifelse(meas_mes$computer >= 20, meas_mes$computer + 9, meas_mes$computer)
meas_mes$hogi <- as.character(meas_mes$hogi)


meas_uniq <- meas_mes %>% select(key, lotno, sunbun, terminalkey, sunjong, sunwidth, suncolor,
                                 terminal1, terminal2, hogi, computer, ng_rate) %>% 
                          distinct(key, lotno, sunbun, terminalkey, sunjong, sunwidth, suncolor,
                                   terminal1, terminal2, hogi, computer, ng_rate)


# plot
ggplot(meas_uniq, aes(x=hogi, y=ng_rate, color=hogi)) +
  geom_boxplot() +
  theme(legend.position = "none")

# 
gra <- meas_uniq %>% filter(ng_rate < 0.005)



ggplot(gra, aes(x=hogi, y=ng_rate, color=hogi)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")




ggplot(gra, aes(x=key, y=ng_rate, color=hogi)) +
  geom_point() +
  facet_wrap(~hogi) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")




# 호기별 차이
model <- glm(gra$ng_rate ~ gra$hogi)
(summary(model))


# 호기 별 불량률 평균
temp <- gra %>% group_by(hogi, sunjong, sunwidth) %>% 
  summarise(mean = mean(ng_rate, na.rm=T))





#### 호기 + 선종/선경 별 차이
gra$sun <- paste(gra$sunjong, gra$sunwidth, sep="_")
str(gra)

gra$hogi_sun <- paste(gra$hogi, gra$sun, sep="_")
model <- glm(gra$ng_rate ~ gra$hogi_sun)
(summary(model))


### 선종 별 호기에서 차이가 있는지?
tmp1 <- gra %>% filter(sun == 'CIVUS_0.35') 
 
ggplot(tmp1, aes(x=hogi, y=ng_rate, color=hogi)) +
geom_boxplot() +
theme_bw() +
theme(panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position = "none")


tmp1 %>% group_by(hogi) %>% 
  summarise(mean = mean(ng_rate, na.rm = T),
            n= n())

model <- glm(tmp1$ng_rate ~ tmp1$hogi)
(summary(model))



### 호기 별 선종에서 차이가 있는지?
tmp1 <- gra %>% filter(hogi == '9') 

ggplot(tmp1, aes(x=sun, y=ng_rate, color=sun)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")


tmp1 %>% group_by(sun) %>% 
  summarise(mean = mean(ng_rate, na.rm = T),
            n= n())

model <- glm(tmp1$ng_rate ~ tmp1$sun)
(summary(model))




######################
### 단자 별 분석
######################

meas_uniq$danza <- paste(meas_uniq$terminal1, meas_uniq$terminal2, sep="_")


library(dplyr)

# 단자별 불량률 기초통계량
tmp <- meas_uniq %>% 
  group_by(sunjong, terminal1, terminal2, hogi) %>% 
  filter(ng_rate != 0) %>% 
  summarise(n= n(),
            sum = sum(ng_rate),
            mean = mean(ng_rate),
            medi = median(ng_rate), 
            max = max(ng_rate),
            min = min(ng_rate),
            sd = sd(ng_rate)) %>% 
  mutate(range = max-min)





# plot
library(ggplot2)
p <- meas_uniq %>% 
#  filter(ng_rate != 0) %>% 
  ggplot(aes(x=danza, y=ng_rate, color=danza))+
  geom_boxplot() +
  theme(legend.position = "none")


install.packages("plotly")
library(plotly)
ggplotly(p)

install.packages("htmlwidgets")
library(htmlwidgets)

# 단자 별 차이
model <- glm(meas_uniq$ng_rate ~ meas_uniq$danza)
(summary(model))

danza$dz <- paste(danza$terminal1, danza$terminal2, sep="_")


# 불량률 0인것 제외하고
dz_all <- meas_uniq %>% group_by(terminal1, terminal2, computer) %>% 
  filter(ng_rate != 0) %>% 
  summarise(n= n(),
            sum = sum(ng_rate),  
            mean = mean(ng_rate),
            mid = median(ng_rate),
            max = max(ng_rate),
            min = min(ng_rate),
            sd = sd(ng_rate)) %>% 
  mutate(range = max-min)

tmp <- danza %>% 
  filter(ng_rate != 0) 

model <- glm(tmp$ng_rate ~ tmp$dz)
(summary(model))





#####################################
### mes data
#######################################

#작업자 별 차이

t <- mes %>% select(key, juya)
t <- unique(t)
merge(meas_uniq, t,
      by = "key")



### 작업시간
tmp <- mes %>% select(key, sigan)
tmp$time <- substr(tmp$sigan, 1,2)
tmp <- tmp[,c(1,3)]

meas_u2 <- merge(meas_uniq, tmp,
                 by = "key")
meas_u2 <- meas_u2[,-c(4)]
meas_u2 <- unique(meas_u2)

ggplot(meas_u2, aes(x=time, y=ng_rate,  color=time)) +
  geom_boxplot() +
#  facet_wrap(~hogi) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")


z <- meas_u2 %>% group_by(time) %>% 
  summarise(mean = mean(ng_rate))

model <- lm(meas_u2$ng_rate ~ meas_u2$time, data=meas_u2)
summary(model)


