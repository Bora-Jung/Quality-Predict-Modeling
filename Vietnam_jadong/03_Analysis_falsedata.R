#####################################
# false data 
# MES 데이터 중 불량발생 데이터
#####################################

library(dplyr)
library(ggplot2)


### key 생성
false_data$key <- paste(false_data$LOT, false_data$선번, sep="_")


# measurement랑 매핑되는 key가 있는지 확인
# 결론 -- 없음
c <- meas$key
false_data %>% filter(key %in% c)




### 불량률 변수 생성
# 불량률 = 불량수량/검사수량
false_data$불량률 <- false_data$불량수량/false_data$검사수량



############################
# 1. 시간 별 불량률 차이
############################


### 1) 시간대별 불량률

# 시간 변수 추가
false_data$hour <- substr(false_data$작업시간, 12, 13)

# 시간대별 불량률 plot
ggplot(false_data, aes(x=hour, y=불량률, color=hour)) +
  geom_boxplot() 

# 시간대별 통계량
hour_summary <- false_data %>% 
  group_by(hour) %>%
  summarise(sum_gumsa = sum(검사수량),
            sum_bulyang = sum(불량수량),
            mean = mean(불량률, na.rm=T),
            sd = sd(불량률, na.rm=T),
            max = max(불량률, na.rm=T),
            min = min(불량률, na.rm=T),
            n=n())


hour_summary$nnn <- hour_summary$불량수량/hour_summary$검사수량

hour_summary$ng <- hour_summary$sum_bulyang/hour_summary$sum_gumsa


# 시간대별 불량률 차이
model <- glm(false_data$불량률 ~ false_data$hour)
summary(model)





### 2) 주/야에 따른 불량률

# 주간/야간 구분
false_data$juya2 <- NA

for(i in 1:nrow(false_data)){
  ifelse(substr(false_data$작업시간[i], 12, 15) >= '08:00' && 
           substr(false_data$작업시간[i], 12, 13) <= '17:00',
         false_data$juya2[i] <- "ju", 
         false_data$juya2[i] <- "ya")
}


# 주/야 불량률 plot
ggplot(false_data, aes(x=작업시간, y=불량률, color=as.factor(juya2))) +
  geom_point()


# 주/야 불량률 차이
model <- glm(false_data$불량률 ~ false_data$juya2)
summary(model)







### 3) 관리자 유무에 따른 불량률

false_data$gr <- NA

# 실제 생산시간만 추출
false_data_gr <- false_data %>% 
  filter(hour <= '22') %>% 
  filter(hour >= '06')

# 관리자 유무 구분
for(i in 1:nrow(false_data_gr)){
  ifelse(false_data_gr$hour[i] >= '08' && false_data_gr$hour[i] <= '17',
         false_data_gr$gr[i] <- "gwanri_o", 
         false_data_gr$gr[i] <- "gwanri_x")
}

# plot
ggplot(false_data_gr, aes(x=작업시간, y=불량률, color=as.factor(gr))) +
  geom_point() 

# 불량률 차이
model <- glm(false_data_gr$불량률 ~ false_data_gr$gr)
summary(model)



ggplot(false_data_gr, aes(x=hour, y=불량률, color=hour)) +
  geom_boxplot() 

model <- glm(false_data_gr$불량률 ~ false_data_gr$hour)
summary(model)






##############################
### 2. 작업호기 간 불량률 차이
##############################

# 작업호기 A로 시작하는것만 추출
# A로 시작하는게 자동기
false_data <- false_data %>% filter(substr(작업호기, 1, 1) == "A")


# 호기별 불량률 평균
t <- false_data %>% group_by(작업호기) %>% 
  summarise(mean = mean(불량률, na.rm=T))


# plot
ggplot(false_data, aes(x=작업호기, y=불량률, color=작업호기)) +
  geom_boxplot() 


# 작업호기 간 차이 있냐
model <- glm(false_data$불량률 ~ false_data$작업호기)
summary(model)




##############################
### 3. 작업자 간 불량률 차이
##############################




