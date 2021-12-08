##*********************************상관분석***********************************##

setwd("C:\\Users\\82105\\Desktop\\correlation")

##패키지 설치(없을경우)
install.packages("Hmisc")
install.packages("ggm")
install.packages("ggplot2")
install.packages("polycor")

##패키지 부착
library(boot)
library(ggm)
library(ggplot2)
library(Hmisc)
library(polycor)

## 자료입력 (각 변수는 개별적인 열, 각 행은 한 사람의 자료)
adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
advertData<-data.frame(adverts, packets)
# 첫번째 사람은 광고 5편, 사탕 8봉지

cov(advertData)
# 각각의 행렬의 값은 분산
# 4.25(adverts x packets)는 이 둘의 공분산

var(adverts)
# adverts의 분산이 위와 같이 2.8이 나옴


## 분석 데이터에 결측치가 있을 경우

adverts<-c(5,4,4,6,8)
packetsNA<-c(8,9,10,NA,15)
# 데이터에 결측치 N/A가 존재함

age<-c(5, 12, 16, 9, 14)

advertNA<-data.frame(adverts, packetsNA, age)

cor(advertData)

cor(advertNA, use = "everything",  method = "pearson")
# cor(x, y, use= "결측값 처리 방식", method = "상관계수 종류")
# x는 수치 변수 또는 데이터 프레임 / y는 다른 수치 변수 (x가 데이터프레임이면 y를 지정할 필요가 없음)
# cor함수의 디폴트는 pearson상관분석 / 다른 방식(e.g. spearman)을 쓰려면 직접 기입해줘야 함
# use="everything" 이라는 조건을 부여했기 때문에 결측치가 있는 객체의 경우 상관 계수가 NA로 산출됨


## 피어슨 상관분석(일반적인 상관분석)

# cor(x,y, use = "everything", method = "pearson")
# use="" 조건식은 결측값 처리방식(4가지)을 의미함.
# "everything"(default)은 결측값이 있으면 NA로 계산 / all.obs=모든 관측값을 사용하려 함, 결측값 있으면 오류
# complete.obs= 결측값이 있는 케이스 모두 제거 / pairwise.compete.obs= 상관계수가 계산되는 변수들 가운데 결측값 있는 케이스만(N/A만) 제거

examData = read.delim("Exam Anxiety.dat",  header = TRUE)
examData

cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = 'pearson')
# exam과 anxiety의 상관계수만 나오는 명령

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
# 데이터를 분리해냄 (명목척도 제외)

cor(examData2)

cor(examData[, c("Exam", "Anxiety", "Revise")])
# 위의 내용을 다른 방법으로 코딩


# 상관계수 유의성 검사
cor.test(examData$Exam, examData$Anxiety, use = "complete.obs", method = 'pearson')
# 상관계수 r 이 -0.44가 나왔고, t검정통계량은 -4.938로 왼쪽꼬리부분의 임계점인 t(0.025,101)=-0.584보다 더 작기 때문에 95% 신뢰수준에서 모델은 유의함
# -0.4409934*sqrt(101/(1-(0.4409934^2)) = -4.938


## 스피어만 상관분석-변수의 척도가 서열척도인 경우

liarData = read.delim("The Biggest Liar.dat",  header = TRUE)
liarData

cor(liarData$Position, liarData$Creativity, method = "spearman")
# method를 spearman으로 지정하면 상관계수가 도출됨
# 음수이므로 부적 상관관계가 있을 것으로 추정됨

####상관계수수
cor.test(liarData$Position, liarData$Creativity, method = "spearman") 
# 스피어맨 상관분석의 경우 디폴트값이 양측검정임
# p-value가 작기 때문에 상관분석 모델 자체는 유의하며 rho값(상관계수)는 -0.37로 도출됨
# 그래서 연구가설인 두 변수간에는 상관이 있다라는 가설이 채택됨

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")  
cor.test(liarData$Position, liarData$Creativity, alternative = "greater", method = "spearman") 
# 왼쪽 꼬리검정을 하려면 alternative="less"라는 조건식을, 오른쪽 꼬리검정을 하려면 alternative = "greater"라는 조건식을 추가함

# 상관계수 유의확률
liarMatrix<-as.matrix(liarData[, c("Position", "Creativity")])
rcorr(liarMatrix)
# 데이터프레임을 행렬로 변환한 후 rcorr() 함수 이용


#****************켄달타우 상관계수 - 순위 간 차이를 통해 상관계수를 도출해내는 방법**************

cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

cor(advertData$adverts, advertData$packets, method = "kendall")
cor.test(advertData$adverts, advertData$packets, alternative = "greater", method = "kendall")


#***************편상관분석 - 제3 변수의 영향력을 제거하고 싶을 때***************

examData = read.delim("Exam Anxiety.dat",  header = TRUE)
maleExam<-subset(examData, Gender == "Male", select= c("Exam", "Anxiety"))
femaleExam<-subset(examData, Gender == "Female", select= c("Exam", "Anxiety"))
cor(maleExam)
cor(femaleExam)

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
library(ggm)
pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2)) 
# 편상관계수 pcor() / 편상관계수 유의성 계산 pcor.test()
# pcor(c(var1, var2, contro1, control2), var(dataframe))


# 편상관분석 사용방법

pc
# 상관계수만 나옴

pc^2
# 상관계수의 제곱을 하면 그 변수의 설명력이 됨

pcor.test(pc, 1, 103) 
# pcor.test(pcor로 만든 객체, 제어 변수의 개수, 표본 크기)
# 'Revise'라는 z변수를 제외하고도 exam과 anxiety의 부적인 상관관계는 여전히 유의미하다 라고 이해할 수 있음