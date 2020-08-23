###################################
### producti (생산정보)
###################################



#작업유형 별 작업시간
p <- prod %>% dplyr::group_by(key, laction) %>% 
  dplyr::summarise(sum=sum(ljobtime))

p$key <- as.factor(p$key)
p$laction <- as.factor(p$laction)

p <- p %>% filter(key != '2619520179_YH202')
p <- p %>% filter(sum >= 0)

ggplot(p, aes(x=laction, y=sum, color=laction)) +
  geom_boxplot()

p2 <- dcast(data=p, key ~ laction, value.var = "sum", fun.aggregate = sum)

p2 <- merge(p2, meas_u2, key='key')


#write.csv(p2, "S:/measurement_producti.csv")

p <- merge(p, meas_uniq,
           by = "key",
           all.x = TRUE)

#작업 유형 별 작업시간 plot
p %>% ggplot(aes(x=sum, y=ng_rate, color=laction)) +
  geom_point(alpha=0.5) +
  facet_wrap(~laction) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")


# sample < 0 --> delete
p2 <- p2 %>% filter(SampleStarted >= 0)

#선종 별 샘플 작업시간 합계
p2 %>% 
  dplyr::group_by(sunjong) %>% 
  dplyr::summarise(sum = sum(SampleStarted))

p2$month <- substr(p2$dt, 6,7)

tmp <- p2 %>% filter(ng_rate < 0.05)
hist(p2$SampleStarted)
ggplot(p2, aes(x=SampleStarted, y=ng_rate,  color=hogi)) +
  geom_point() +
  facet_wrap(~hogi) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

meas_u2 <- merge(meas_uniq, meas_date,
                   by = "key")

meas_u2 <- meas_u2[,-c(4)]

meas_u2 <- unique(meas_u2)

model <- lm(p2$SampleStarted ~ p2$hogi, data=p2)
summary(model)




tmp <- p2[,c(2:11,22)]
cor <- cor(tmp, use="pairwise.complete.obs")
corrplot(cor, method='shade', shade.col=NA, tl.col='black', tl.srt=90,
         addCoef.col=TRUE
         )


tmp <- tmp %>% filter(ng_rate > 0)
cor(tmp, use="pairwise.complete.obs")

p2 <- p2[,-c(14)]
p2 <- unique(p2)


tmp <- p2[,c(1:11,22)]


##key-선종-작업시간-불량률 로 데이터 합치기
#선종 특성 정보 제거
p3 <- p2[,c(1:11,44)]

p3 <- melt(p3, id=c("key", "ng_rate"))

p3$index <- 1:nrow(p3)

p3 %>% filter(value > 0) %>% 
  ggplot(aes(x=value, y=ng_rate, color=variable)) +
  geom_point() +
  facet_wrap(~variable)


## 작업유형이 불량률에 영향을 미치는지?
pp <- p2[,c(1:11,15)]
pp <- pp[,-1]
str(pp)

model <- lm(pp$ng_rate ~ ., data=pp)
summary(model)

#상관계수 확인
cor(pp, use="pairwise.complete.obs")

#샘플작업시간 높은 순서로 정렬
p2 <- p2[order(p2$SampleStarted, decreasing=T),]


tmp <- prod[,c(33,31)] #호기 데이터 추출
tmp <- unique(tmp)

#작업시간 + 호기
p2 <- merge(p2, tmp, by='key')

p2$id <- 1:nrow(p2)

#작업시간.melt + 호기
p3 <- merge(p3, tmp, by='key')

p2 %>% ggplot(aes(x=SampleStarted, y=ng_rate, color=lcomputername)) +
  geom_point(alpha=0.5) +
  facet_wrap(~lcomputername) +
  theme(legend.position = "none")


#호기의 불량률 영향 확인
model <- glm(p3$value ~ p3$lcomputername)
summary(model)

#호기의 샘플작업시간 영향 확인
model <- glm(p2$SampleStarted ~ p2$lcomputername)
summary(model)