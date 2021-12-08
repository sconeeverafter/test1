###���� ���� �м� Script!!!!

#----��ŷ ���丮 ����------
setwd("C:/Users/HOON/Desktop/Dropbox/stat/Code") ##��ǥ�� ��ŷ ���丮
setwd("")
getwd()


#----��Ű�� ��ġ �� ����-----
install.packages("car")
install.packages("mlogit")
library(car)
library(mlogit)

library(Rcmdr)


#*********************����********************

#������ �ҷ�����(eel)
eelData<-read.delim("eel.dat", header = TRUE)

#������ Ȯ��
head(eelData)
levels(eelData$Cured)
levels(eelData$Intervention)

#���� ���� ����, Not Cured, No Treatment�� ���������� ����
eelData$Cured<-factor(eelData$Cured, levels = c("Not Cured", "Cured")) 
levels(eelData$Cured)
eelData$Intervention<-factor(eelData$Intervention, levels = c("No Treatment", "Intervention"))
levels(eelData$Intervention)

#ȸ�ͺм� ����
###���� ������ glm �Լ�, family �ɼ��� binomial()
#cured������Ӻ����� ���ؼ� 
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial()) 
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

summary(eelModel.1)
summary(eelModel.2)
