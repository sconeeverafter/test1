## File -> Reopen with encoding -> UTF-8

#현재 작업 디렉토리 확인
getwd()

#작업 폴더 경로 설정
setwd()

#------------------- SIMPLE MEDIATION ----------------------#
#만약 한글 주석이 깨진다면, File-> Reopen with encoding -> UTF-8 

#패키지 설치
install.packages("processR")
install.packages("lavaan")
install.packages("nortest") #정규성 검증위한 'anderson'
library(processR)
library(lavaan)
library(nortest) 

# ---------------- X가 이분형 변수인 매개 모형 --------------------#
#X,M,Y에 명칭 부여
labels = list(X="cond", M = "pmi", Y="reaction")
#통계적 모형 그리기
drawModel(labels=labels,box.col="lightcyan")
#미디어 영향력 연구(pmi)의 기술통계량 요약 ###pmi data는 processR에 포함##
xlabels= c("Interior Page", "Front Page")
meanSummaryTable(data=pmi, labels=labels, xlabels=xlabels)


#######회귀 모형 요약 ########
#단순매개모형에서는 독립변수 X, 종속변수Y로 하는 단순회귀분석 추정 가능
#명칭 = lm(종속~독립, data=데이터명)
fit1=lm(pmi~cond, data=pmi)
fit2=lm(reaction~ cond+pmi, data=pmi) #다중 독립변수
#회귀분석 결과
summary(fit1)
summary(fit2)
#회귀계수 요약  #절편,회귀계수값,p값
modelsSummaryTable(labels=labels, data=pmi)

#총효과(c) 계산 
# 명칭 = lm (종속~독립, data=데이터명)
fit3=lm(reaction~cond, data=pmi)
fit3

####### 간접효과 (a*b) 분석 #######
#모형 분석을 위한 lavaan syntax 생성
model=tripleEquation(labels = labels)
#print()와 동일한 기능, 출력 후 행 바꾸지 X
#여러 인자를 나열해 쓰면 해당 인자들이 계속 연결되어 출력된다는 특징 따라서 프로그램 진행 상황을 보기 좋게 출력
cat(model)
print(model)

#부트스트래핑
set.seed(123) #재현 가능한 결과를 얻기 위해 난수 생성
# 명칭 <- sem(모델, data = 데이터명, se="boot", bootstrap = 횟수)
semfit = sem(model=model, data=pmi, se="boot", bootstrap=1000)
summary(semfit, ci=TRUE)
#매개효과 분석표
medSummaryTable(semfit)


############## 통계적 추론 #######################
#총효과(c)에 대한 추론  ------> 유의미 X
summary(fit3) #c의 추정치와 p값
confint(fit3) #c의 95% 신뢰구간

#직접효과(c')에 대한 추론  ----> 유의미 X
summary(fit2) #c'의 추정치와  cond의 p값 
confint(fit2) #c'의 95% 신뢰구간

#간접효과(a*b)에 대한 추론
##### 1) 소벨 검정- 귀무가설 검정    -------> 유의미 X
labels = list(X="cond", M = "pmi", Y="reaction")
result = mediationBK(labels=labels, data=pmi)
#ab값, seab(표준오차추정량), z값, p값, 신뢰구간 제시
result$normalTheory 
##### 2) 부트 스트랩에 의한 신뢰구간
#부트스트랩 데이터 추출
bootData= getBootData(semfit)
#간접효과
ab = bootData$a * bootData$b
#간접효과 - 정규성 검정 - 시각화
densityPlot(ab) #신뢰구간 0 포함 X -> 간접효과 통계적 유의 O
qqPlot(ab) #좌우 비대칭, 정규분포X 

####참고#### - 객관적인 정규성 검정법
#간접효과
shapiro.test(ab) #샤피로
nortest::ad.test(ab) #앤더슨
#직접효과
x=bootData$c
shapiro.test(x)
nortest::ad.test(x) #정규분포

####참고####부트스트랩 방법에 따른 신뢰구간 요약
medSummaryTable(semfit, boot.ci.type="all")
medSummaryTable(semfit, boot.ci.type="perc")


#--------------- X가 연속형 변수인 매개모형------------------#
#사용 데이터 : 경제적 스트레스 연구 데이터(estress)로 processR에 포함함
labels1 = list(X="estress", M="affect", Y="withdraw")
drawModel(labels=labels1, label.pos = 2)
#매개 모형 - 회귀계수 요약
modelsSummaryTable(labels=labels1, data=estress)

#간접/직접/총효과값 + 통계적 검증
#boot strap 신뢰구간 얻기 위해 lavaan syntax 만들어 분석
model1 =tripleEquation(labels=labels1)
#랜덤시드 설정
set.seed(123)
#간접효과 추정을 위한 부트스트랩
semfit1 = sem(model1, data=estress, se="boot")
#모형의 매개효과 요약
#계수값, 신뢰구간 확인
medSummaryTable(semfit1, boot.ci.type="perc")
#표준화 회귀계수를 출력하고 싶은 경우 standardized = TRUE를 입력
#계수값, p값 등 확인
summary(semfit1,standardized = TRUE )

