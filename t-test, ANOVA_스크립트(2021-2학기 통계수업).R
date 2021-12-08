
#######################################################
####################  t-test  #########################
#######################################################


##----Set the working directory------

getwd()
setwd("C:\\mileg\\graduates")


##----이전작업 객체 지우기--------
rm(list=ls())
#rm(): 더이상 사용하지 않는 객체 제거
#list=: 여러 객체를 보다 쉽게 제거, 
#ls(): 리턴값이 character vector인 특성을 이용하여 현재 저장되어 있는 모든 변수 삭제
#rm(list=ls()): 모든 객체를 리스트에 할당한 후 다 삭제하도록 해줌


##----Install Packages-----
install.packages("ggplot2")
install.packages("pastecs")
install.packages("Hmisc")
install.packages("reshape")
#ggplot2: 그림을 그리는 과정을 절차에 따른 요소(components)로 나누어서 각 요소를 차례대로 그림판에 추가하는 형식
#pastecs: 기술통계, stat.desc()함수를 포함, 측정단위 효과를 없애준다. 변동계수가 필요할 때 좋음
#Hmisc: 전체 변수 대상 기술통계량 제공(평균, 중위수, 분산, 표준편차 등 분위수에 따른 분석에 좋음)
#reshape: 데이터를 재구성하여 구조를 바꾸기

##--------기술통계 정리(연속형 변수)-------------
#summary(): base package에 속한 함수, 결과물(min, 1Q, median, mean, 3Q, max), base package에 있으므로, 사용하기에 편리한 반면, 약간 부족한 듯한 면이 있음
#stat.desc(): pastecs package에 속한 함수, 가장 많이 쓰임, 결과물(nbr.val, nbr.null, nbr.na, min, max, range, sum, median, mean, var, std.dev, coef.var, skewness, kurtosis, normtest.W, normtest.p, SE.mean, CI.mean.p)
#describe(): psych package에 속한 함수, 결과물(n, mean, std.dev, median, trimmed(절삭평균/10%절삭평균), mad(중위값절대편차), min, max, range, skew, kurtosis, se), summary 보다는 많고, stat.desc보다는 적은 기술통계량을 보여줌


##--------stat.desc() 함수의 옵션 별로 제공하는 통계량-------------
#basic=true(case #(관측치 개수), null #, na #, min, max, range, sum)
#desc=true(median, mean, variance, sd)
#norm=true(skew(왜도), kurt(첨도), normtest(정규성 검정통계량), norm test.p(정규성 검정 P-value)



##-------패키지 로딩하기---------
library(ggplot2)
library(pastecs)
library(Hmisc)
library(reshape)


##-------객체 생성---------
spiderLong<-read.delim("spiderLong.dat", header = TRUE)
spiderWide<-read.delim("spiderWide.dat", header = TRUE)
#read.delim: 구분자 tab을 자동인식
#header: 첫번째를 열이름으로 인식


##-------데이터 구조 살펴보기---------
str(spiderLong)
str(spiderWide)


##---------기술통계----------------
by(spiderLong$Anxiety, spiderLong$Group, stat.desc, basic=FALSE, norm=TRUE)
#by() 함수: 회귀분석, 호출 시  첫번째 인수에는 데이터, 두번째에는 그룹을 만들 팩터, 세번째에는 각 그룹에 적용할 함수를 넣음


##--------ggplot을 통해 Error Bar Chart 그리기--------------
spiderLongBar <- ggplot(spiderLong, aes(Group, Anxiety))
spiderLongBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "group", y = "anxiety")
#stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black"): 평균값을 계산하고, 막대형태로, 내부는 흰색으로, 테두리는 검은색으로 표시
#stat_summary(fun.data = mean_cl_normal, geom = "pointrange"): 표준95% 신뢰구간을 점 범위(pointtange) 기하 객체로 표시한 계층을 그래프에 추가
#labs(x = "group", y = "anxiety"): 그래프에 적절한 이름표를 추가



##----------t-test--------------------------------------------

##--------long dataframe-----------
ind.t.test<-t.test(Anxiety ~ Group, data = spiderLong, paired=FALSE)
ind.t.test
#두 그룹의 자료가 하나의 열에 들어있을 때는 lm()함수처럼 사용
#예시(new모형 <-t.test(결과변수~예측변수, data=데이터프레임, paired=FALSE 또는 TRUE))
#Group 변수로 구분되는 두 그룹의 Anxiety 점수들의 평균의 비교에 대한 검정 결과를 담은 객체를 생성
#paired=TRUE이면 종속t검정, FALSE를 지정하거나 이 옵션을 생략하면 독립t검정

##--------wide dataframe-----------
ind.t.test<-t.test(spiderWide$Real, spiderWide$Picture, paired=FALSE)
ind.t.test
#두 그룹의 자료가 다른 두 열에 들어 있을 때 사용
#예시(new모형 <-t.test(점수그룹1, 점수그룹2, paired=FALSE 또는 TRUE))



##---------효과크기 계산-----------------
ind.t.test$statistic
ind.t.test$parameter
t<-ind.t.test$statistic[[1]]
t
df<-ind.t.test$parameter[[1]]
df
#거미공포증 자료에 대한 독립t검정의 t값을 담은 t와 자유도를 담은 df라는 변수를 생성하는 명령어
#t.test 검정결과를 담은 객체의 statistic에 t값이 있고, parameter에 자유도가 있음
#ind.t.test 객체에 t-test 결과가 저장되어 있으므로, 객체 다음에 $기호를 써서 변수이름을 쓰면 됨

r <- sqrt(t^2/(t^2+df))
round(r, 3)
#이들을 이용해서 r값을 계산
#sqrt(t^2/(t^2+df)): r값을 계산하는 공식
#round()함수를 이용하여 수치를 반올림한 결과를 출력(3이라고 적었으므로, 소수점 이하 세자리까지)

#독립 t-test: 서로 다른 개체들로 이루어진 두 그룹의 평균을 비교(각 실험 조건마다 서로 다른 참가자를 사용했다면 독립t-test 적용)
#종속 t-test: 같은 개체들에서 비롯된 두 평균을 비교(여러 실험 조건에 대해 같은 참가자를 사용했다면 종속t-test 적용)


stat.desc(spiderWide, basic = FALSE, norm = TRUE)
#SpiderWide 데이터는 같은 참가자에게 두 가지 실험을 실시하여 비교
#paired=TRUE이므로 종속t검정 적용


dep.t.test<-t.test(Anxiety ~ Group, data = spiderLong, paired = TRUE)
dep.t.test

dep.t.test<-t.test(spiderWide$Real, spiderWide$Picture, paired = TRUE)
dep.t.test

#Effect sizes:
dep.t.test$statistic
dep.t.test$parameter
t<-dep.t.test$statistic[[1]]
t
df<-dep.t.test$parameter[[1]]
df
r <- sqrt(t^2/(t^2+df))
round(r, 3)



#######################################################
####################  ANOVA  #########################
#######################################################


##----Set the working directory------

getwd()
setwd("C:\\mileg\\graduates")


##----이전작업 객체 지우기--------
rm(list=ls())
#rm(): 더이상 사용하지 않는 객체 제거
#list=: 여러 객체를 보다 쉽게 제거, 
#ls(): 리턴값이 character vector인 특성을 이용하여 현재 저장되어 있는 모든 변수 삭제
#rm(list=ls()): 모든 객체를 리스트에 할당한 후 다 삭제하도록 해줌


##----Install Packages-----
install.packages("car")
install.packages("effects")
install.packages("compute.es")
install.packages("multcomp")
install.packages("granova")
install.packages("pastecs")
#car: 회귀분석 가능, 레빈검정(등분산), VIF,  계산
#effects: 수정평균을 계산
#compute.es: 효과크기 기산
#multcomp: 사후검정 계산
#granova: Graphical Analysis of Variance
#pastecs: 기술통계, stat.desc()함수를 포함, 측정단위 효과를 없애준다. 변동계수가 필요할 때 좋음


library(car)
library(effects)
library(compute.es)
library(multcomp)
library(granova)
library(ggplot2)
library(pastecs)



##--------Viagra dataframe 만들기----------
id<-(1:15) #15개의 관측치
libido<-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose<-gl(3,5, labels = c("Placebo", "Low Dose", "High Dose"))
#gl()함수: 요인(factor)을 쉽게 정의할 수 있는 함수, gl(요인 수, 요인 별 반복 수, 전체 수, 요인 이름)
viagraData<-data.frame(dose, libido)


##--------Graph 그리기-------------
line <- ggplot(viagraData, aes(dose, libido))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000") + 
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + labs(x = "Dose of Viagra", y = "Mean Libido")


##---------기술통계----------------
by(viagraData$libido, viagraData$dose, stat.desc)

#Levene's test
#car의 함수중 하나, 등분산검정 결과 (각집단의 분산이 동일한지?=집단의 분산이 다르다)
leveneTest(viagraData$libido, viagraData$dose, center = median)
#leveneTest(결과변수, 그룹화 변수, center=median/mean)


#ANOVA
viagraModel<-aov(libido~dose, data = viagraData)
viagraModel

summary(viagraModel)
summary.lm(viagraModel)
plot(viagraModel)

