##### 단순회귀분석
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("car")
library(car)

install.packages("nycflights13")
library(nycflights13)

flightData<-data.frame(flights)
#### 그래프 보기가 너무 느려서 day=2, dep_delay=10, distance 3000 이하만 플링
flightData %>%
  filter(day==2, dep_delay==10, distance<=3000)->sample

#### 변수 확인
str(sample)
View(sample)

#### distance - arr_time의 관계 확인 - 산점도를 통해 확인
Scatterplot<-ggplot(sample, aes(distance, air_time))+geom_point()

#### 단순회귀모형 도출
flight1 <- lm(air_time ~ distance, data = sample, na.action=na.omit) 
summary(flight1)

#### Intercept(상수항): 21.171273
#### Distance Coefficients Estimate(기울기) : 0.122982 = 기울기
#### Multiple R-squared가 0.9838이므로 distance가 air_time을 98.69% 설명할 수 있음 
#### F통계치 : 6556가 나왔으므로 임계치 F(1,87)보다 큰 값이므로 유의미함
#### p-value: 0.05보다 작으므로 신뢰수준 95%에서 회귀모델은 유의미함

####잔차 검증을 위한 확인 가능
par(mfrow=c(2,2))
plot(flight1)

# 잔차의 독립성 검정 durbin-watson
durbinWatsonTest(flight1)

# 잔차의 정규성 검정
shapiro.test(flight1$residuals)

