
setwd("C:/Users/chlwl/Desktop/Multiple_Regression")
#데이터 저장한 경로를 워킹디렉토리로 설정(오류 시 '\'를 '/'로 수정하여 진행)


install.packages("car")
install.packages("QuantPsyc")

library(QuantPsyc)
library(car)
library(boot)
##패키지 설치 및 부착



#######################################################
######### 다중회귀분석(Multiple Regression) ###########
#######################################################

album2<-read.delim("Album Sales 2.dat", header = TRUE)
                         
albumSales.2<-lm(sales ~ adverts, data = album2)
## 광고 횟수 - 매출 간 단순회귀분석

albumSales.3<-lm(sales ~ adverts + airplay + attract, data = album2)
## 독립변수 추가시 '+'로 진행

summary(albumSales.2)
## adverts가 sales에 미치는 영향에 대한 단순회귀분석: adverts의 설명량은 33%, 회귀모델 유의함.

summary(albumSales.3)
## 세 변수(adverts, airply, attract)가 sales에 에 대한 설명량은 66%, 다중회귀모델 유의함(F통계량, p값 참고).
## 도출된 B값으로 다중회귀식을 구성할 경우 Y' = -26.61 + 0.08*B1 + 3.37*B2 + 11.09*B3


lm.beta(albumSales.3)
## Standardized parameter: 독립변수의 베타계수 간 상대적인 비교를 위해서는 lm.beta()를 통해 표준화베타계수를 구할 수 있음.
## 비표준화계수(B)는 회귀식 구성 시 사용, 표준화계수(Beta)는 회귀계수 간의 비교 시 활용 
## 각 독립변수별로 단위가 다를 수 있기 때문에 표준화 과정을 거치는 것

## 비표준화계수는 attract의 영향이 큰 것으로 나타내지만, 표준화계수는 attract가 가장 영향력이 낮고, 오히려 adverts와 airplay의 영향력이 큰 것을 확인할 수 있음.
## 따라서 서로 다른 독립변수 간 차이를 확인하기 위해선 표준화된 회귀계수를 사용.


####################################################
###############  기본가정 검정  ####################
####################################################

library(base)
library("car")
## 회귀분석 기본 가정 검정을 위한 패키지 : base (cook's distance, leverage값 등)

par(mfrow=c(1,1))

## 회귀모델의 이상치 검정 및 확인
outlierTest(albumSales.3)
## 회귀모델 내에서의 이상치 검정: Bonferroni 검정
## Bonferroni 검정의 귀무가설: "회귀모델 내 이상치가 없다", 대립가설: "회귀모델 내 이상치가 있다."
## Bonferroni p값이 0.05보다 크기 때문에 대립가설이 기각, 귀무가설이 채택됨에 따라 이상치가 없다는 결론을 내릴 수 있음
## 회귀모델 내 이상치가 존재할 경우 데이터 수준에서 이상치 처리가 필요하며, 일반적으로 제거(결측치 처리 후 드랍), 대체(평균, 최빈값, 중앙값, 예측값 등으로 대체)의 방법을 사용함.

## 회귀모델의 이상치 검정 및 확인(2)
influencePlot(albumSales.3, main="Influence Plot")
## 이상치(studentized residual로 진단)와 영향치(leverage로 진단)에 대한 진단 수행
## 이상치: 설명변수 관측치 내 존재하지만 적합 회귀선에서 벗어난 관측치, 회귀모형의 적합도, 결정계수를 감소시킴.
## 영향치: 설명변수의 관측값 범위를 벗어나지만 적합 회귀선 상에 있는 관측값, 회귀모형의 결정계수를 높이고 유의성을 과도하게 높힐 수 있음(과적합)
## 일반적으로 이상치와 영향치를 삭제함으로써 회귀모형의 결정계수, 적합도를 증가시킬 수 있음. 

album2[169,]; album2[181,]; album2[138,]

album2_reshape <- album2[-c(169,138,181),]
## 이상치 및 영향치 드랍

influencePlot(ablumSales.3_reshape, main="Influence Plot Reshaped")
## 드랍된 것 확인

ablumSales.3_reshape <- lm(sales ~ adverts + airplay + attract, data = album2_reshape)

summary(albumSales.3)
summary(ablumSales.3_reshape)


## 잔차의 정상성, 선형성, 동변량성(그래픽적 방법)
par(mfrow=c(2,2))
plot(albumSales.3)
## 그림(1.1) 잔차가 추정된 y값과 무관하게 정상분포함을 알 수 있음(기울기가 0인 직선)
## 그림(1.2) 잔차가 정규분포함을 알 수 있음(기울기가 45도)
## 그림(2.1) 표준화 잔차가 추정된 y값과 무관하게 정상분포함(기울기가 0인 직선)
## 그림(2.2) 표준화잔차와 레버리지 값 간의 관계, 레버리지 값이 비정상적으로 크면 이상치로 의심할 수 있음.


#### 잔차의 독립성(D-W통계치)
durbinWatsonTest(albumSales.3)
## 더빈왓슨 통계량 - 잔차간의 자기상관이 있는지 없는지를 판단
## 2에 근접할 경우 잔차 간 자기상관이 없는 것으로 판단
## D-W값이 1.948로 잔차 간 독립성을 확보한 것으로 판단 가능. 

## 동일한 함수: dwt(albumSales.3)


#### 다중공선성 검정(공차, 분산팽창계수)
vif(albumSales.3)
# 각 독립변수의 분산팽창계수(VIF)값 확인
# 값이 10 이상이면 다중공선성을 의심할 수 있으나 해당 케이스는 그렇지 않은 것으로 판단할 수 있음/

1/vif(albumSales.3)
## 공차(Tolerance)확인
## 공차는 분산팽창계수의 역수이며, 1에 근접할수록 다중공선성이 없는 것으로 판단, 10 이상인 경우 다중공선성 존재 가능 


########################################################
#########   더미변수를 활용한 다중회귀분석   ###########
########################################################

album2$adverts_d1<-ifelse(album2$adverts<500,1,0)
## adverts의 값 중 500보단 작은 것을 1, 나머지를 0으로 코딩

album2$adverts_d2<-ifelse(album2$adverts>=500&album2$adverts<1000,1,0)
## 500 이상 1000 미만의 값을 1, 나머지는 0으로 코딩


## 이는 곧 광고비용이 1000이상인 집단을 기준집단(reference group)으로 삼았음을 의미함.
## 이렇게 더미변수를 코딩한 d1, d2로 회귀식을 구성
## 광고비용에 대한 세 집단(adverts_d1,d2 / 500 미만, 500이상 1000미만, 1000이상)이 판매량(sales)에 주는 영향에 대한 다중회귀분석

albumSales_dummy<-lm(sales ~ adverts_d1 + adverts_d2, data = album2)
## 더미변수를 활용한 회귀분석 실시

summary(albumSales_dummy)
## 회귀식은 y'=271.75 - 118.67*d1 - 71.17*d2, 더미변수를 활용한 회귀모형은 유의함(F통계량,p값 참고)
## 광고비용이 1000 이상인 경우 추정되는 판매량은 271.75개.
## 광고비용이 500이상 1000미만인 경우의 추정 판매량은 1000이상인 경우보다 71.17이 낮은 200.58개.
## 광고비용이 500미만인 경우의 추정 판매량은 1000이상인 경우보다 118.67이 낮은 153.08개임을 추정할 수 있음.








