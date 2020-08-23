library(readxl)
hrinfo <- read_xlsx("S:/01. ������/��������/��Ʈ���λ�����_20190827.xlsx", 
          sheet = "Sheet1")

hrinfo_result <- read_xls("S:/01. ������/��������/���������ý���_��翩������_20190926.xls",
                           sheet = "Sheet1")

hr1 <- read.csv("S:/01. ������/��������/�λ�����1.csv", header = TRUE)
hr2 <- read.csv("S:/01. ������/��������/�λ�����2.csv", header = TRUE)


hr1$��� <- as.character(hr1$���)
hr2$��� <- as.character(hr2$���)

hr1$key <- paste(hr1$���, hr1$�������, sep="_")
hr2$key <- paste(hr2$���, hr2$�������, sep="_")

# �λ�����(2018, 2019) ����
hr_tmp <- rbind(hr1, hr2)


# �ߺ� key ����
hr_tmp <- hr_tmp[-which(duplicated(hr_tmp$key)),]


hrinfo_result$key <- paste(hrinfo_result$���, hrinfo_result$�������, sep="_")

# �λ����� ��, ��������� �ִ°͸� ���� 
tmp <- hr_tmp %>% filter(key %in% hrinfo_result$key)

tmp2 <- merge(tmp, hrinfo_result,
              by = "key",
              all.x = TRUE)

hr <- tmp2


# ��翩�� ���� ��ȯ
hr$��翩�� <- gsub("R", "���", hr$��翩��)
hr$��翩�� <- gsub("W", "����", hr$��翩��)
hr$��翩�� <- gsub("1", "����", hr$��翩��)
hr$��翩�� <- as.factor(hr$��翩��)
levels(hr$��翩��) <- c("���", "����", "1")

library(ggplot2)
library(colorspace)

# ����� �� plot
ggplot(hr, aes(x=����.x, fill=��翩��)) +
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
  

hr %>% filter(��翩�� == "���") %>% 
  group_by(������) %>% 
  mutate(n=n()) %>% 
  ggplot(aes(x=reorder(������,-n), y=n, fill=������)) +
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

hr %>% filter(��翩�� == "���") %>% 
  group_by(������) %>% 
  summarise(n=n()) %>% 
  mutate(ratio=n*100/sum(n))
  

# chisq test
chisq.test(x=hr$����.x, p=hr$��翩��)

t <- chisq.test(hr$����.x, hr$��翩��)

table(hr$����, hr$��翩��)
chisq.test(hr$����, hr$��翩��)

hr$������� - hr$�Ի���


ggplot(hr, aes(x=���õ�, fill = ��翩��)) +
  geom_bar() +
  scale_fill_brewer(palette = "Greys") +
  facet_wrap(~��翩��) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  guides(fill="none") 



hr_bye <- hr %>% filter(��翩�� == "���")
hr_bye$�������-hr_bye$�Ի���
hr_bye$�������


library(lubridate)

#�ټ� �ϼ�
hr_bye$jobday <- ymd(hr_bye$�������) - ymd(hr_bye$�Ի���)

#�ټ� ������
hr_bye$jobmonth <- (year(hr_bye$�������) - year(hr_bye$�Ի���))*12 + month(hr_bye$�������)-month(hr_bye$�Ի���)


#�ټ� ������(������ ����) : �����ڴ� 2019-09-26 ����
ifelse(hr$��翩�� == '���', 
       hr$jobmonth <- (year(hr$�������) - year(hr$�Ի���))*12 + month(hr$�������) - month(hr$�Ի���),
       hr$jobmonth <- (year('2019-09-26') - year(hr$�Ի���))*12 + month('2019-09-26') - month(hr$�Ի���))


plot(hr$jobmonth)
ggplot(hr, aes(x=jobmonth)) +
  geom_bar()+
  facet_wrap(~��翩��)



# age : 2019-09-26 ����, ����ڴ� ������� ����

ifelse(hr$��翩�� == '���', 
         hr$age <- (year(hr$�������) - year(hr$�������.x)) + 1, 
         hr$age <- (year('2019-09-26') - year(hr$�������.x)) + 1)



### ��ġ�� ���� ���� ����

library(MASS)

# ���Լ�����
# p-value < 0.05�� ��� ���Լ� �������� ����
qqnorm(hr$X3���ٹ��ð�)
qqline(hr$X3���ٹ��ð�, col=2)
shapiro.test(hr$X3���ٹ��ð�)

# ��л����
# p-value < 0.05�� ��� ��л꼺 �������� ����
bartlett.test(X3���ٹ��ð� ~ ��翩��, data = hr)



# ���Լ� �������� �����Ƿ� wilcoxon t-test
wilcox.test(hr$jobmonth ~ hr$��翩��, data = hr)

wilcox.test(hr$age ~ hr$��翩��, data = hr)
wilcox.test(hr$Ű ~ hr$��翩��, data = hr)
wilcox.test(hr$������ ~ hr$��翩��, data = hr)
wilcox.test(hr$X3��.Ư��.Ƚ��.��. ~ hr$��翩��, data = hr)
wilcox.test(hr$X3���ٹ��ð� ~ hr$��翩��, data = hr)

hr$�޿� <- str_trim(t, side=c("both", "left", "right"))
hr$�޿� <- gsub(",", "", hr$�޿�)
hr$�޿� <- as.numeric(hr$�޿�)
wilcox.test(hr$�޿� ~ hr$��翩��, data = hr)



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
VIF(lm(�޿� ~ ., data = hr_numeric))
VIF(lm(Ű ~ ., data = hr_numeric))
VIF(lm(������ ~ ., data = hr_numeric))
VIF(lm(X3���ٹ��ð� ~ ., data = hr_numeric))
VIF(lm(X3��.Ư��.Ƚ��.��. ~ ., data = hr_numeric))
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
  t.test_p.value <- t.test(hr_numeric_z[,names[i]] ~ hr_numeric$��翩��, var.equal = TRUE)$p.value
  
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


#y����, x���� ����
com <- data.frame(hr_numeric$��翩��, x5)
colnames(com)[1] <- "��翩��"

library(reshape2)

meltdata <- melt(com, id.vars = "��翩��")
ggplot(meltdata[meltdata$value < 3,], aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = as.factor(��翩��))) + 
  theme_bw() +
  coord_flip()


ggplot(data=com, aes(x=X3���ٹ��ð�, y=X3��.Ư��.Ƚ��.��., colour=as.factor(��翩��), alpha=0.5)) +
   geom_point(shape=19, size=3) 



# logistic regression
data <- data.frame(hr$��翩��, hr[,x_names_sorted])
str(data)
colnames(data)[1] <- "��翩��"

hr$��翩�� <- gsub("����", "0", hr$��翩��)
hr$��翩�� <- gsub("���", "1", hr$��翩��)


set.seed(123)
train <- sample(1:nrow(data), size = 0.8*nrow(data), replace = F)
test <- (-train)
y.test <- data$��翩��[test]


glm.fit <- glm(��翩�� ~., data = data,
                   family = binomial(link = "logit"),
                   subset = train)
summary(glm.fit)




dt <- hr
dt <- dt[,-c(1:5)]
glm.fit <- glm(��翩�� ~ �μ� + ���� + ���� + �з� + ����, data = dt, family = binomial(link = "logit"))
summary(glm.fit)









# randomforest sample
library(randomForest)

tmp <- hr[,c(33:37, 51:53)]
model <- randomForest(��翩�� ~., data = tmp, importance = T, na.action = na.omit)
model
importance(model)



### over sampling
install.packages("ROSE")
library(ROSE)
data_over <- ovun.sample(��翩�� ~ ., data = tmp, method = "over", N=2000)$data
tree <- rpart(��翩�� ~ . , data = data_over)


# glm
model_over <- randomForest(��翩�� ~., data = data_over, importance = T, na.action = na.omit)
model_over
importance(model_over)

g1 <- glm(��翩��~ C(����.x) + jobmonth, data = hr_tmp, family = binomial(link="logit"))
summary(g1)



g1 <- glm(��翩��~., data = tmp, family = binomial(link="logit"))
summary(g1)


g <- glm(��翩��~., data = data_over, family = binomial(link="logit"))
summary(g)


data_over2 <- data_over[,-c(2)]
g2 <- glm(��翩��~., data = data_over2, family = binomial(link="logit"))
summary(g2)

# beta's 95% 
confint(g2)

# test
pre <- predict(g1, tmp, type="response")
glm.pre <- rep(0, 1192)
glm.pre[pre > .5] =1
table(tmp$��翩��, glm.pre)
#oversampling �������� ���� ��Ȯ���� �� ����

install.packages("ROCR")
library(ROCR)
pr <- prediction(glm.pre, tmp$��翩��)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")


