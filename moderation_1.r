
#############################################################################################
#############################################################################################
#######################조절분석의 기초##############################
#############################################################################################
#############################################################################################


## 현재 디렉토리 확인하기
getwd()


##파일이 있는 디렉토리로 설정하기
setwd("C:/Users/LG/Desktop/Presentation_stat")


##필요한 패키지 설치하기
install.packages("processR")
install.packages("flextable")
install.packages("lavaan")
install.packages("nortest")
install.packages("dplyr")

##데이터 불러오기
library(processR)
library(flextable)
library(lavaan)
library(nortest)
library(dplyr)
require(processR)
require(flextable)
require(lavaan)
require(nortest)
require(dplyr)


## Disaster 데이터에서 기술통계량
labels=list(X="frame", Y="justify", W="skeptic")
xlabels=c("Natural causes condition", "Climate change condition")
meanSummaryTable(data=disaster, labels=labels, xlabels=xlabels)


## t-test

t.test(justify~frame,data=disaster, var.equal=TRUE)


## Variuos Regression Models Estimating Justifications for Withholding Aid
library(ggplot2)

fit1=lm(justify~frame, data=disaster)
fit2=lm(justify~frame+skeptic, data=disaster)
fit3=lm(justify~frame*skeptic, data=disaster)
disaster=meanCentering(disaster, names=c("frame", "skeptic"))
disaster$frame2=ifelse(disaster$frame== 0,-0.5,0.5)
fit4=lm(justify~frame*skeptic.c, data=disaster)
fit5=lm(justify~frame2*skeptic.c, data=disaster)
fitlist=list(fit1,fit2,fit3,fit4,fit5)
labels=list(X="frame", W="skeptic", X.c="frame.c", W.c="skeptic.c", X2="frame2")
fitlabels=c("","","","(mean-centerd W)","(mean-centerd W)", "(mean-centered W, X coded -0.5 and o.5)")
modelsSummary2(fitlist,labels=labels, fitlabels=fitlabels, autoPrefix=FALSE) 


## 조절효과모형 - 프로세스 마크로 모형
labels=list(X="frame", Y="justify", W="skeptic")
pmacroModel(1, labels=labels, rady=0.08, radx=0.12)


## 조절효과모형 - 통계적 모형
statisticalDiagram(1, labels=labels, rady=0.08, radx=0.12)


## 통계적 모형 분석
fit3=lm(justify~frame*skeptic, data=disaster)
summary(fit3)


## b3에 대한 신뢰구간
confint(fit3)


## 조절변수의 평균중심화
disaster*meanCentering(disaster, names = c("skeptic"))
mean(disaster$skeptic.c)


## 평균중심화된 새로운 변수를 이용한 새로운 회귀모형
fit4=lm(justify~frame*skeptic.c, data = disaster)
summary(fit4)


## 조절모형의 시각화 - Y추정한 값들
fit=lm(justify~frame*skeptic, data = disaster)
modSummary2Table(fit) %>% width(width = 1)


## 조절모형의 시각화 
condPlot(fit, mode = 2, xpos = 0.7)


## 상호작용 탐색 - 특정값 선택방법 - 조절변수 skeptic의 값이 1.592인 조건부효과
fit1 = lm(justify~frame*I(skeptic-1.592), data = disaster)
summary(fit1)


## 상호작용 탐색 - 특정값 선택방법 - plot그리기
condPlot(fit, rangemode = 2, xpos = 0.7, labels = c("Clamte change(X=1)", "Natural causes(X=0)"))


## Johnson-Neyman 방법
jnPlot(fit)
