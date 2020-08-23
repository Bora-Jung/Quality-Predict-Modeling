library(readxl)
hrinfo <- read_xlsx("S:/01. 정보라/사전예방/베트남인사정보_20190827.xlsx", 
          sheet = "Sheet1")

hrinfo_result <- read_xls("S:/01. 정보라/사전예방/종합정보시스템_퇴사여부포함_20190926.xls",
                           sheet = "Sheet1")

hr1 <- read.csv("S:/01. 정보라/사전예방/인사정보1.csv", header = TRUE)
hr2 <- read.csv("S:/01. 정보라/사전예방/인사정보2.csv", header = TRUE)


hr1$사번 <- as.character(hr1$사번)
hr2$사번 <- as.character(hr2$사번)

hr1$key <- paste(hr1$사번, hr1$생년월일, sep="_")
hr2$key <- paste(hr2$사번, hr2$생년월일, sep="_")

# 인사정보(2018, 2019) 병합
hr_tmp <- rbind(hr1, hr2)


# 중복 key 제거
hr_tmp <- hr_tmp[-which(duplicated(hr_tmp$key)),]


hrinfo_result$key <- paste(hrinfo_result$사번, hrinfo_result$생년월일, sep="_")

# 인사정보 중, 퇴사정보가 있는것만 추출 
tmp <- hr_tmp %>% filter(key %in% hrinfo_result$key)

tmp2 <- merge(tmp, hrinfo_result,
              by = "key",
              all.x = TRUE)

hr <- tmp2


# 퇴사여부 글자 변환
hr$퇴사여부 <- gsub("R", "퇴사", hr$퇴사여부)
hr$퇴사여부 <- gsub("W", "재직", hr$퇴사여부)
hr$퇴사여부 <- gsub("1", "재직", hr$퇴사여부)
hr$퇴사여부 <- as.factor(hr$퇴사여부)
levels(hr$퇴사여부) <- c("퇴사", "재직", "1")

library(ggplot2)
library(colorspace)

# 퇴사자 수 plot
ggplot(hr, aes(x=성별.x, fill=퇴사여부)) +
  geom_bar() +
#  coord_flip() +
  scale_fill_brewer(palette = "Greys") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill="none") +
  geom_text(stat = "count",
            aes(label=..count..),
            position = position_dodge(width = 0.8),
            vjust = -0.5)
  

hr %>% filter(퇴사여부 == "퇴사") %>% 
  group_by(퇴사사유) %>% 
  mutate(n=n()) %>% 
  ggplot(aes(x=reorder(퇴사사유,-n), y=n, fill=퇴사사유)) +
  geom_bar(stat = "identity", fill = "#d9d9d9") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  guides(fill="none") +
  geom_text(aes(label= n),
            position = position_dodge(width = 0.8),
            vjust = -0.5)

hr %>% filter(퇴사여부 == "퇴사") %>% 
  group_by(퇴사사유) %>% 
  summarise(n=n()) %>% 
  mutate(ratio=n*100/sum(n))
  

# chisq test
chisq.test(x=hr$성별.x, p=hr$퇴사여부)

t <- chisq.test(hr$성별.x, hr$퇴사여부)

table(hr$국적, hr$퇴사여부)
chisq.test(hr$라인, hr$퇴사여부)

hr$퇴사일자 - hr$입사일


ggplot(hr, aes(x=숙련도, fill = 퇴사여부)) +
  geom_bar() +
  scale_fill_brewer(palette = "Greys") +
  facet_wrap(~퇴사여부) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill="none") 



hr_bye <- hr %>% filter(퇴사여부 == "퇴사")
hr_bye$퇴사일자-hr_bye$입사일
hr_bye$퇴사일자


library(lubridate)

#근속 일수
hr_bye$jobday <- ymd(hr_bye$퇴사일자) - ymd(hr_bye$입사일)

#근속 개월수
hr_bye$jobmonth <- (year(hr_bye$퇴사일자) - year(hr_bye$입사일))*12 + month(hr_bye$퇴사일자)-month(hr_bye$입사일)


#근속 개월수(재직자 포함) : 재직자는 2019-09-26 기준
ifelse(hr$퇴사여부 == '퇴사', 
       hr$jobmonth <- (year(hr$퇴사일자) - year(hr$입사일))*12 + month(hr$퇴사일자) - month(hr$입사일),
       hr$jobmonth <- (year('2019-09-26') - year(hr$입사일))*12 + month('2019-09-26') - month(hr$입사일))


plot(hr$jobmonth)
ggplot(hr, aes(x=jobmonth)) +
  geom_bar()+
  facet_wrap(~퇴사여부)



# age : 2019-09-26 기준, 퇴사자는 퇴사일자 기준

ifelse(hr$퇴사여부 == '퇴사', 
         hr$age <- (year(hr$퇴사일자) - year(hr$생년월일.x)) + 1, 
         hr$age <- (year('2019-09-26') - year(hr$생년월일.x)) + 1)



### 수치형 변수 차이 검정

library(MASS)

# 정규성검정
# p-value < 0.05인 경우 정규성 만족하지 않음
qqnorm(hr$X3월근무시간)
qqline(hr$X3월근무시간, col=2)
shapiro.test(hr$X3월근무시간)

# 등분산검정
# p-value < 0.05인 경우 등분산성 만족하지 않음
bartlett.test(X3월근무시간 ~ 퇴사여부, data = hr)



# 정규성 만족하지 않으므로 wilcoxon t-test
wilcox.test(hr$jobmonth ~ hr$퇴사여부, data = hr)

wilcox.test(hr$age ~ hr$퇴사여부, data = hr)
wilcox.test(hr$키 ~ hr$퇴사여부, data = hr)
wilcox.test(hr$몸무게 ~ hr$퇴사여부, data = hr)
wilcox.test(hr$X3월.특근.횟수.월. ~ hr$퇴사여부, data = hr)
wilcox.test(hr$X3월근무시간 ~ hr$퇴사여부, data = hr)

hr$급여 <- str_trim(t, side=c("both", "left", "right"))
hr$급여 <- gsub(",", "", hr$급여)
hr$급여 <- as.numeric(hr$급여)
wilcox.test(hr$급여 ~ hr$퇴사여부, data = hr)



# between x valiable's correlation
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

hr_numeric <- hr[,c(33:37, 51:53)]
hr_numeric <- hr_numeric[,c(1:5, 7:8, 6)]
chart.Correlation(hr_numeric[,c(1:7)], histogram=TRUE, col="grey10", pch=1)

install.packages("GGally")
library(GGally)
ggcorr(hr_numeric, name="corr", label=T)




# remove X value - VIF
install.packages("fmsb")
library(fmsb)
VIF(lm(급여 ~ ., data = hr_numeric))
VIF(lm(키 ~ ., data = hr_numeric))
VIF(lm(몸무게 ~ ., data = hr_numeric))
VIF(lm(X3월근무시간 ~ ., data = hr_numeric))
VIF(lm(X3월.특근.횟수.월. ~ ., data = hr_numeric))
VIF(lm(jobmonth ~ ., data = hr_numeric))
VIF(lm(age ~ ., data = hr_numeric))


# standardization
hr_numeric_z <- scale(hr_numeric[,1:7])
summary(hr_numeric_z)
summary(hr_numeric)


# feature selection
names <- names(data.frame(hr_numeric_z))
t.test_p.value_df <- data.frame()

for(i in 1:length(names)){
  t.test_p.value <- t.test(hr_numeric_z[,names[i]] ~ hr_numeric$퇴사여부, var.equal = TRUE)$p.value
  
  t.test_p.value_df[i, 1] <- names[i]
  t.test_p.value_df[i, 2] <- t.test_p.value
}

colnames(t.test_p.value_df) <- c("x_var_name", "p.value")

t.test_p.value_df <- arrange(t.test_p.value_df, desc(p.value))

t.test_p.value_df <- t.test_p.value_df %>% filter(p.value < 0.05)

x_names_sorted <- t.test_p.value_df$x_var_name
x_names_sorted

hr_numeric_z <- as.data.frame(hr_numeric_z)
x5 <- hr_numeric_z[x_names_sorted]


#y변수, x변수 병합
com <- data.frame(hr_numeric$퇴사여부, x5)
colnames(com)[1] <- "퇴사여부"

library(reshape2)

meltdata <- melt(com, id.vars = "퇴사여부")
ggplot(meltdata[meltdata$value < 3,], aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = as.factor(퇴사여부))) + 
  theme_bw() +
  coord_flip()


ggplot(data=com, aes(x=X3월근무시간, y=X3월.특근.횟수.월., colour=as.factor(퇴사여부), alpha=0.5)) +
   geom_point(shape=19, size=3) 



# logistic regression
data <- data.frame(hr$퇴사여부, hr[,x_names_sorted])
str(data)
colnames(data)[1] <- "퇴사여부"

hr$퇴사여부 <- gsub("재직", "0", hr$퇴사여부)
hr$퇴사여부 <- gsub("퇴사", "1", hr$퇴사여부)


set.seed(123)
train <- sample(1:nrow(data), size = 0.8*nrow(data), replace = F)
test <- (-train)
y.test <- data$퇴사여부[test]


glm.fit <- glm(퇴사여부 ~., data = data,
                   family = binomial(link = "logit"),
                   subset = train)
summary(glm.fit)




dt <- hr
dt <- dt[,-c(1:5)]
glm.fit <- glm(퇴사여부 ~ 부서 + 공정 + 성별 + 학력 + 직군, data = dt, family = binomial(link = "logit"))
summary(glm.fit)









# randomforest sample
library(randomForest)

tmp <- hr[,c(33:37, 51:53)]
model <- randomForest(퇴사여부 ~., data = tmp, importance = T, na.action = na.omit)
model
importance(model)



### over sampling
install.packages("ROSE")
library(ROSE)
data_over <- ovun.sample(퇴사여부 ~ ., data = tmp, method = "over", N=2000)$data
tree <- rpart(퇴사여부 ~ . , data = data_over)


# glm
model_over <- randomForest(퇴사여부 ~., data = data_over, importance = T, na.action = na.omit)
model_over
importance(model_over)

g1 <- glm(퇴사여부~ C(성별.x) + jobmonth, data = hr_tmp, family = binomial(link="logit"))
summary(g1)



g1 <- glm(퇴사여부~., data = tmp, family = binomial(link="logit"))
summary(g1)


g <- glm(퇴사여부~., data = data_over, family = binomial(link="logit"))
summary(g)


data_over2 <- data_over[,-c(2)]
g2 <- glm(퇴사여부~., data = data_over2, family = binomial(link="logit"))
summary(g2)

# beta's 95% 
confint(g2)

# test
pre <- predict(g1, tmp, type="response")
glm.pre <- rep(0, 1192)
glm.pre[pre > .5] =1
table(tmp$퇴사여부, glm.pre)
#oversampling 하지않은 모델이 정확도가 더 높음

install.packages("ROCR")
library(ROCR)
pr <- prediction(glm.pre, tmp$퇴사여부)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")



