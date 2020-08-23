###################################
### producti (��������)
###################################



#�۾����� �� �۾��ð�
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

#�۾� ���� �� �۾��ð� plot
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

#���� �� ���� �۾��ð� �հ�
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


##key-����-�۾��ð�-�ҷ��� �� ������ ��ġ��
#���� Ư�� ���� ����
p3 <- p2[,c(1:11,44)]

p3 <- melt(p3, id=c("key", "ng_rate"))

p3$index <- 1:nrow(p3)

p3 %>% filter(value > 0) %>% 
  ggplot(aes(x=value, y=ng_rate, color=variable)) +
  geom_point() +
  facet_wrap(~variable)


## �۾������� �ҷ����� ������ ��ġ����?
pp <- p2[,c(1:11,15)]
pp <- pp[,-1]
str(pp)

model <- lm(pp$ng_rate ~ ., data=pp)
summary(model)

#������ Ȯ��
cor(pp, use="pairwise.complete.obs")

#�����۾��ð� ���� ������ ����
p2 <- p2[order(p2$SampleStarted, decreasing=T),]


tmp <- prod[,c(33,31)] #ȣ�� ������ ����
tmp <- unique(tmp)

#�۾��ð� + ȣ��
p2 <- merge(p2, tmp, by='key')

p2$id <- 1:nrow(p2)

#�۾��ð�.melt + ȣ��
p3 <- merge(p3, tmp, by='key')

p2 %>% ggplot(aes(x=SampleStarted, y=ng_rate, color=lcomputername)) +
  geom_point(alpha=0.5) +
  facet_wrap(~lcomputername) +
  theme(legend.position = "none")


#ȣ���� �ҷ��� ���� Ȯ��
model <- glm(p3$value ~ p3$lcomputername)
summary(model)

#ȣ���� �����۾��ð� ���� Ȯ��
model <- glm(p2$SampleStarted ~ p2$lcomputername)
summary(model)