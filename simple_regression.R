##### �ܼ�ȸ�ͺм�
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("car")
library(car)

install.packages("nycflights13")
library(nycflights13)

flightData<-data.frame(flights)
#### �׷��� ���Ⱑ �ʹ� ������ day=2, dep_delay=10, distance 3000 ���ϸ� �ø�
flightData %>%
  filter(day==2, dep_delay==10, distance<=3000)->sample

#### ���� Ȯ��
str(sample)
View(sample)

#### distance - arr_time�� ���� Ȯ�� - �������� ���� Ȯ��
Scatterplot<-ggplot(sample, aes(distance, air_time))+geom_point()

#### �ܼ�ȸ�͸��� ����
flight1 <- lm(air_time ~ distance, data = sample, na.action=na.omit) 
summary(flight1)

#### Intercept(�����): 21.171273
#### Distance Coefficients Estimate(����) : 0.122982 = ����
#### Multiple R-squared�� 0.9838�̹Ƿ� distance�� air_time�� 98.69% ������ �� ���� 
#### F���ġ : 6556�� �������Ƿ� �Ӱ�ġ F(1,87)���� ū ���̹Ƿ� ���ǹ���
#### p-value: 0.05���� �����Ƿ� �ŷڼ��� 95%���� ȸ�͸��� ���ǹ���

####���� ������ ���� Ȯ�� ����
par(mfrow=c(2,2))
plot(flight1)

# ������ ������ ���� durbin-watson
durbinWatsonTest(flight1)

# ������ ���Լ� ����
shapiro.test(flight1$residuals)
