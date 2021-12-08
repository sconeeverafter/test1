#---------------------------------------------------------------------------------------------------------
#R Code for 요인분석
#---------------------------------------------------------------------------------------------------------

#----사용할 폴더 루트 설정하기------
setwd("F:/Users/song he/Desktop/R")

# 어떤 데이터인지 설명
factorData<-read.delim("raq.dat", header = TRUE)

#해당 데이터는 "R불안 설문지"에 관련된 응답 데이터임.
#개인이 R 사용법 학습에 관해 얼마나 불안해하는가?를 예측하기 위함임
#R에 관한 불안이 어떤 구체적인 형태의 불안으로 분해가 되는지 알고자 함

#------설치할 패키지와 라이브러리 실행하기-----
install.packages("corpcor")
install.packages("GPArotation") # 회전을 위한 패키지
install.packages("psych") # 요인분석을 위한 패키지
install.packages("pastecs")

library(corpcor)
library(GPArotation)
library(psych)

#상관행렬 구하기 / 요인들을 묶기 위해서는 요인 간에 어떤 상관관계가 있는지 파악해야 함
#변인 간 상관이 0.3보다 큰지, 그리고 변인간 상관이 0.9보다 큰 것은 없는지 파악해야 함
factorMatrix<-cor(factorData)

#소수점 셋째자리까지 반올림하기
round(factorMatrix, 3)

#한눈에 보기 편하게 편집하기
round(factorMatrix[,1:8], 3)
round(factorMatrix[,9:16], 3)
round(factorMatrix[,17:23], 3)

#------요인분석에 적합한지 확인하기-----
#Bartlett's test / 모집단으로부터 추출한 표본의 상관계수행렬의 행렬식 값을 계산하여 상관계수행렬이 단위행렬인지 아닌지를 카이제곱분포를 이용하여 검증
#이 검정의 결과가 유의하다는 것은 R 행렬이 단위행렬이 아니라는 것이고 변수들 간의 일정한 관계가 존재함.
cortest.bartlett(factorData)

# R was not square, finding R from data

cortest.bartlett(factorMatrix, n = 2571)

# KMO = 변수들 사이의 상관계수의 제곱과 변수들 사이의 편상관계수의 제곱의 비
# KMO가 1에 가까우면 상관계수들이 비교적 한 곳에 몰려있으며, 이는 요인분석에서 서로 구별되고 신뢰성이 있는 요인들이 나올 확률이 큼
# 최소 0.5이상이어야 함
#KMO test : 대문자임에 유의!

KMO(factorData)
#1에 가까울 수록 좋다. 

#판별식/ 변수들 간의 상관관계가 너무 높아 다중공선성이 존재할 경우 문제가 됨.
#판별식 값이 0.00001보다는 커야 함
det(factorMatrix)
#0.00005니까 요인분석해도 되겠다.

#------요인분석 실시하기-----

#------요인추출 실시하기-----
#Principal Component Analysis를 하는 이유 (버려지는 값들이 없다)

#pcModel<-principal(dataframe/R-matrix, nfactors = number of factors, rotate = "method of rotation", scores = TRUE)

pc1 <-  principal(factorData, nfactors = 23, rotate = "none")
#23개의 열인데 귀찮으면 length 써도 된다. 
pc1 <-  principal(factorData, nfactors = length(factorData), rotate = "none")
pc1
#23개 볼 필요가 없다. h2가 1, u2는 1에서 빼주는?

##커뮤넬리티(h2)가 모두 1인 것 확인할 수 있음
#com은 호프만 지수로 1이면 해당 문항이 요인 하나에, 2이면 해당 문항이 요인 두 개에 공통적으로 부하되어 있다는 뜻임
# SS loadings는 각 요인의 고유값(아이겐값)임
# Proporation Var는 인자의 개수를 나눈 것임 (7.29/)
# 출력된 고윳값들을 보았을 때 1이 넘는 것은 네 개임(PC1~4)

#------요인 개수 도출하기(Determining the Number of Factors to Extract)-----
## 스크리 도표 - values는 아이겐값을 말함
plot(pc1$values, type = "b") 

# 스크리 검사와 평행분석이 있음
scree(factorData)
fa.parallel(factorData)
#평행분석에 대한 간단한 설명
#의미있는 요인이 고유값은 최소한 무작위적인 자료에서 얻어진 요인의 고유값의 평균보다는 커야한다는것
## 도출한 요인 수에 따라 요인을 추출하기
#커뮤넬리티 값이 변하고 요인적재값은 변하지 않음
pc2 <-  principal(factorData, nfactors = 4, rotate = "none")
pc2
#고윳값은 변하지 않지만 커뮤넬리티는 더 이상 1이 아님을 알 수 있음
# 커뮤넬리티(h2)는 달라짐. 주어진 변수 안에 있는 공통분산의 비율이기 때문(23 -> 4 로 줄었음: 추출된 요인들이 설명하는 비율이 당연히 감소)

#------잔차를 통해 요인추출 확인하기-----
#요인 추출이 잘 되었는가? 잔차를 통해 확인해보자
#Explore residuals
#여기서 loadings는 요인적재값을 말함 / 재생상관행렬을 구할 수 있음

factor.model(pc2$loadings)
reproduced<-round(factor.model(pc2$loadings), 3) #보기 편하게 바꾸기
reproduced[,1:9]  #확인하기 이게 y^ 값

factor.residuals(factorMatrix, pc2$loadings) 
resids<-round(factor.residuals(factorMatrix, pc2$loadings), 3) #보기 편하게 바꾸기
resids[,1:9] #확인하기

pc2$fit.off
#fit 1에 가까우면 좋은건데, 상관계수를 분모로 갖고 그 잔차값을 분자로 갖아서 1에서 빼줘서

#잔차가 0.05 ㅇ ㅣ상이면 안된다.
residuals<-factor.residuals(factorMatrix, pc2$loadings)
residuals<-as.matrix(residuals[upper.tri(residuals)]) # 대각산 위 삼각형만 남김
large.resid<-abs(residuals) > 0.05 # TRUE라면 값이 1로 책정됨
sum(large.resid)
sum(large.resid)/nrow(residuals) ##??

hist(residuals) # 정규분포를 하고 있어야 함


#------요인회전 하기---------
# 요인들을 회전하게 되면 해석이 개선이 되고 요인 하나에 대해 각 변수의 요인부하량이 최대화 되고 다른 요인에 대해서는 최소화됨. 이를 통해서 어떤 변수가 어떤 요인과 상관되어 있는지 파악할 수 있음.
# 회전은 변수들의 상대적 차이를 유지하면서 변수들의 절댓값을 변경하는 식임
# 직교회전 - 배리맥스
pc3 <-  principal(factorData, nfactors = 4, rotate = "varimax")
# 행렬의 성분(요인부하량)은 pc2와 달라졌지만 커뮤넬리티는 변하지 않음
# 고윳값 모두 변하지만 총합은 변하지 않음(회전을 통해 고윳값이 고르게 분포하게 되지만 총량은 안변함)

# 0.3 미만의 요인부하량은 보이지 않게 함. sort 높은거로 정렬해줘
print.psych(pc3, cut = 0.3, sort = TRUE)

# 사교회전 - 오블리민
# 직교회전에서는 요인들끼리의 상관관계가 없다고 하였으나 서로 상관되어 있을 가능성이 큼.
pc4 <- principal(factorData, nfactors = 4, rotate = "oblimin")
print.psych(pc4, cut = 0.3, sort = TRUE)
#상관을 활용해서 구조행렬 구할 수 있다. 

#사교회전 시 r에서 도출되는 행렬은 패턴행렬임. 구조행렬을 구하기 위해서는 새로운 함수를 도입해야 함.
#구조행렬을 구하는 식은 아래와 같음.
factor.structure <- function(fa, cut = 0.2, decimals = 2){
  structure.matrix <- fa.sort(fa$loadings %*% fa$Phi)
  structure.matrix <- data.frame(ifelse(abs(structure.matrix) < cut, "", round(structure.matrix, decimals)))
  return(structure.matrix)
}

factor.structure(pc4, cut = 0.3)
#구조행렬은 패턴행렬과 상관행렬을 곱한 것임. 요인들 간의 관계를 고려해서 수정한 성분을 담고 있음.


#------요인점수 구하기---------

pc5 <- principal(factorData, nfactors = 4, rotate = "oblimin", scores = TRUE)
pc5$scores
head(pc5$scores, 10)
factorData <- cbind(factorData, pc5$scores)

#요인 점수 간 행렬은? 직교회전을 했을 때와 사교회전을 했을 때의 차이점은?

#사교회전 시에는? 위의 값과 무슨 차이가 있는가?
cor(pc5$scores)
round(cor(pc5$scores), 2)

#직교 회전 시에는?
round(cor(pc3$scores), 2)


#self test
round(cor(pc5$scores, factorData$Q01),2)
round(cor(pc5$scores, factorData$Q06),2)
round(cor(pc5$scores, factorData$Q18),2)


#------r을 활용하여 요인분석에 대한 시각화해보기-----

install.packages("factoextra")
library("factoextra")

#주성성분 한 것에 대한 그림을 본 것. 회전을 한 게 아님.
res.pca <- prcomp(factorData, scale = TRUE)

#요인별로 설명하는 아이겐값 (Propertion Explained 값과 같음)
fviz_eig(res.pca)



#개인별 데이터 시각화
#x,y축 두었을 때 얼마나 잘 설명할 수 있는지
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#변인에 대한 데이터 시각화
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# 데이터 값만으로 PCA 시각화해보기
library(FactoMineR)
result <- PCA(factorData)

##개인 데이터와 변인 벡터값 한 그래프에 표현하기

#단, 이 데이터에서는 값들이 너무 많아서 잘 보이지 않음
fviz_pca_biplot(res.pca)

#다른 방법은 다음과 같으나 역시 데이터가 너무 많음
install.packages("ggfortify")
library(ggfortify)
autoplot(res.pca, data = factorData, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



## 적은 데이터로 그래프 다시 활용해보기
library(haven) 
graphData<-read_spss("factor.sav") # 다변량 데이터 요인분석 책에 있는 데이터값 (나중에 SPSS 활용 가능)
graphData<-graphData[,-c(1,2)]

res.pca <- prcomp(graphData, scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca)

autoplot(res.pca, data = graphData, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


#요인 모델을 가꾸기 위해 변인들 제거하기기
install.packages("gdata")
library(gdata)

#요인을 제거할 때는 많은 것을 고려해야 함함
factorData.2<-remove.vars(factorData, c("Q19"))



##원하는 값(아이겐값, 커뮤넬리티 등)을 따로 보기
#커뮤넬리티
pc1$communality
pc3$communality

print(pc1$communality, cutoff=0, digits=3)
print(pc3$communality, cutoff=0, digits=3)
print(pc4$communality, cutoff=0, digits=3)


#아이겐값값

pc3$values
print(pc3$values, cutoff=0, digits=3)


#Percentage of Variance Accounted For
100*pc3$values[1:23]/length(pc3$values)

print(pc3$loadings, cutoff=0, digits=3, sort = TRUE)
print(pc3$Structure, cutoff=0, digits=3)

print(pc4$loadings, cutoff=0, digits=3, sort = TRUE)
print(pc4$Structure, cut=0.3, digits=3, sort = TRUE)







