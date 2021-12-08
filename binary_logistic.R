###이항 로짓 분석 Script!!!!

#----워킹 디렉토리 설정------
setwd("C:/Users/HOON/Desktop/Dropbox/stat/Code") ##발표자 워킹 디렉토리
setwd("")
getwd()


#----패키지 설치 및 실행-----
install.packages("car")
install.packages("mlogit")
library(car)
library(mlogit)

library(Rcmdr)


#*********************예제********************

#데이터 불러오기(eel)
eelData<-read.delim("eel.dat", header = TRUE)

#데이터 확인
head(eelData)
levels(eelData$Cured)
levels(eelData$Intervention)

#팩터 순서 지정, Not Cured, No Treatment를 기준점으로 설정
eelData$Cured<-factor(eelData$Cured, levels = c("Not Cured", "Cured")) 
levels(eelData$Cured)
eelData$Intervention<-factor(eelData$Intervention, levels = c("No Treatment", "Intervention"))
levels(eelData$Intervention)

#회귀분석 실행
###이중 로짓은 glm 함수, family 옵션은 binomial()
#cured라는종속변수에 대해서 
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial()) 
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

summary(eelModel.1)
summary(eelModel.2)

