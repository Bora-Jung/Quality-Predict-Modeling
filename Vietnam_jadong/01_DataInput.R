#### library ��ġ ####
install.packages("rJava", "DBI", "RMySQL")

library(DBI)
library(RMySQL)
library(dplyr)

#### DB CONNECT ####
con <- dbConnect(MySQL(), user="riskmg", password="riskmg", dbname="riskmg",
                 host="172.21.0.153")


dbListTables(con)


#### data input ####

# measurement : ��������
measurement <- con %>% dbGetQuery("select * from measurement where mjobdate >= '2019-04-01'")

# producti : �������
producti <- con %>% dbGetQuery("select * from producti where ljobdate >= '2019-04-01'")

# qualityparm : ���ھ��� ǰ��������
qualityparm <- con %>% dbGetQuery("select * from qualityparm where qjobdate >= '2019-04-01'")

# statistic : ǰ������ ��赥����
statisticdata <- con %>% dbGetQuery("select * from statisticdata where sjobdate >= '2019-04-01'")

# mes_autodata : MES(POP) �������� �Ϻ�
mes_autodata <- con %>% dbGetQuery("select * from mes_autodata where fdate >= '2019-04-01'")


# false data : gate test �ҷ� ������(MES)
library(readxl)
false_data1 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190401~190510.xlsx",
                        sheet = "Sheet1")

false_data2 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190701~190816.xlsx",
                        sheet = "Sheet1")

false_data3 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190816~191107.xlsx",
                        sheet = "Sheet1")

false_data4 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190510~190630.xlsx",
                         sheet = "Sheet1")

library(dplyr)
false_data1 <- false_data1 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �߻����)

false_data2 <- false_data2 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �ҷ�����)

false_data3 <- false_data3 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �ҷ�����)

false_data4 <- false_data4 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �ҷ�����)

colnames(false_data1)[12] <- "�ҷ�����"


false_data <- rbind(false_data1, false_data2)
false_data <- rbind(false_data, false_data3)
false_data <- rbind(false_data, false_data4)



# errorldc : ���� �˶� �ڵ� 
errorldc <- read.csv("S:/������/������ũ/�� ����/errorldc_1904.csv", header=T)


# kijun : ������ ����/���� �� BLO ���ذ�
kijun <- read.csv("D:/workspace/������ũ(��Ʈ��)/dataset/BLO����.csv", header=TRUE)



tmp <- read.csv("S:/talk3/����Ʈ�˻��̷�0701~0816(����߰�).csv", header=TRUE)