#---------------------------------------------------------------------------------------------------------
#R Code for ���κм�
#---------------------------------------------------------------------------------------------------------

#----����� ���� ��Ʈ �����ϱ�------
setwd("F:/Users/song he/Desktop/R")

# � ���������� ����
factorData<-read.delim("raq.dat", header = TRUE)

#�ش� �����ʹ� "R�Ҿ� ������"�� ���õ� ���� ��������.
#������ R ���� �н��� ���� �󸶳� �Ҿ����ϴ°�?�� �����ϱ� ������
#R�� ���� �Ҿ��� � ��ü���� ������ �Ҿ����� ���ذ� �Ǵ��� �˰��� ��

#------��ġ�� ��Ű���� ���̺귯�� �����ϱ�-----
install.packages("corpcor")
install.packages("GPArotation") # ȸ���� ���� ��Ű��
install.packages("psych") # ���κм��� ���� ��Ű��
install.packages("pastecs")

library(corpcor)
library(GPArotation)
library(psych)

#������ ���ϱ� / ���ε��� ���� ���ؼ��� ���� ���� � ������谡 �ִ��� �ľ��ؾ� ��
#���� �� ����� 0.3���� ū��, �׸��� ���ΰ� ����� 0.9���� ū ���� ������ �ľ��ؾ� ��
factorMatrix<-cor(factorData)

#�Ҽ��� ��°�ڸ����� �ݿø��ϱ�
round(factorMatrix, 3)

#�Ѵ��� ���� ���ϰ� �����ϱ�
round(factorMatrix[,1:8], 3)
round(factorMatrix[,9:16], 3)
round(factorMatrix[,17:23], 3)

#------���κм��� �������� Ȯ���ϱ�-----
#Bartlett's test / ���������κ��� ������ ǥ���� ����������� ��Ľ� ���� ����Ͽ� ����������� ����������� �ƴ����� ī������������ �̿��Ͽ� ����
#�� ������ ����� �����ϴٴ� ���� R ����� ��������� �ƴ϶�� ���̰� ������ ���� ������ ���谡 ������.
cortest.bartlett(factorData)

# R was not square, finding R from data

cortest.bartlett(factorMatrix, n = 2571)

# KMO = ������ ������ �������� ������ ������ ������ ���������� ������ ��
# KMO�� 1�� ������ ���������� ���� �� ���� ����������, �̴� ���κм����� ���� �����ǰ� �ŷڼ��� �ִ� ���ε��� ���� Ȯ���� ŭ
# �ּ� 0.5�̻��̾�� ��
#KMO test : �빮���ӿ� ����!

KMO(factorData)
#1�� ����� ���� ����. 

#�Ǻ���/ ������ ���� ������谡 �ʹ� ���� ���߰������� ������ ��� ������ ��.
#�Ǻ��� ���� 0.00001���ٴ� Ŀ�� ��
det(factorMatrix)
#0.00005�ϱ� ���κм��ص� �ǰڴ�.

#------���κм� �ǽ��ϱ�-----

#------�������� �ǽ��ϱ�-----
#Principal Component Analysis�� �ϴ� ���� (�������� ������ ����)

#pcModel<-principal(dataframe/R-matrix, nfactors = number of factors, rotate = "method of rotation", scores = TRUE)

pc1 <-  principal(factorData, nfactors = 23, rotate = "none")
#23���� ���ε� �������� length �ᵵ �ȴ�. 
pc1 <-  principal(factorData, nfactors = length(factorData), rotate = "none")
pc1
#23�� �� �ʿ䰡 ����. h2�� 1, u2�� 1���� ���ִ�?

##Ŀ�³ڸ�Ƽ(h2)�� ��� 1�� �� Ȯ���� �� ����
#com�� ȣ���� ������ 1�̸� �ش� ������ ���� �ϳ���, 2�̸� �ش� ������ ���� �� ���� ���������� ���ϵǾ� �ִٴ� ����
# SS loadings�� �� ������ ������(���̰հ�)��
# Proporation Var�� ������ ������ ���� ���� (7.29/)
# ��µ� ���������� ������ �� 1�� �Ѵ� ���� �� ����(PC1~4)

#------���� ���� �����ϱ�(Determining the Number of Factors to Extract)-----
## ��ũ�� ��ǥ - values�� ���̰հ��� ����
plot(pc1$values, type = "b") 

# ��ũ�� �˻�� ����м��� ����
scree(factorData)
fa.parallel(factorData)
#����м��� ���� ������ ����
#�ǹ��ִ� ������ �������� �ּ��� ���������� �ڷῡ�� ����� ������ �������� ��պ��ٴ� Ŀ���Ѵٴ°�
## ������ ���� ���� ���� ������ �����ϱ�
#Ŀ�³ڸ�Ƽ ���� ���ϰ� �������簪�� ������ ����
pc2 <-  principal(factorData, nfactors = 4, rotate = "none")
pc2
#�������� ������ ������ Ŀ�³ڸ�Ƽ�� �� �̻� 1�� �ƴ��� �� �� ����
# Ŀ�³ڸ�Ƽ(h2)�� �޶���. �־��� ���� �ȿ� �ִ� ����л��� �����̱� ����(23 -> 4 �� �پ���: ����� ���ε��� �����ϴ� ������ �翬�� ����)

#------������ ���� �������� Ȯ���ϱ�-----
#���� ������ �� �Ǿ��°�? ������ ���� Ȯ���غ���
#Explore residuals
#���⼭ loadings�� �������簪�� ���� / ����������� ���� �� ����

factor.model(pc2$loadings)
reproduced<-round(factor.model(pc2$loadings), 3) #���� ���ϰ� �ٲٱ�
reproduced[,1:9]  #Ȯ���ϱ� �̰� y^ ��

factor.residuals(factorMatrix, pc2$loadings) 
resids<-round(factor.residuals(factorMatrix, pc2$loadings), 3) #���� ���ϰ� �ٲٱ�
resids[,1:9] #Ȯ���ϱ�

pc2$fit.off
#fit 1�� ������ �����ǵ�, �������� �и�� ���� �� �������� ���ڷ� ���Ƽ� 1���� ���༭

#������ 0.05 �� �ӻ��̸� �ȵȴ�.
residuals<-factor.residuals(factorMatrix, pc2$loadings)
residuals<-as.matrix(residuals[upper.tri(residuals)]) # �밢�� �� �ﰢ���� ����
large.resid<-abs(residuals) > 0.05 # TRUE��� ���� 1�� å����
sum(large.resid)
sum(large.resid)/nrow(residuals) ##??

hist(residuals) # ���Ժ����� �ϰ� �־�� ��


#------����ȸ�� �ϱ�---------
# ���ε��� ȸ���ϰ� �Ǹ� �ؼ��� ������ �ǰ� ���� �ϳ��� ���� �� ������ ���κ��Ϸ��� �ִ�ȭ �ǰ� �ٸ� ���ο� ���ؼ��� �ּ�ȭ��. �̸� ���ؼ� � ������ � ���ΰ� ����Ǿ� �ִ��� �ľ��� �� ����.
# ȸ���� �������� ����� ���̸� �����ϸ鼭 �������� ������ �����ϴ� ����
# ����ȸ�� - �踮�ƽ�
pc3 <-  principal(factorData, nfactors = 4, rotate = "varimax")
# ����� ����(���κ��Ϸ�)�� pc2�� �޶������� Ŀ�³ڸ�Ƽ�� ������ ����
# ������ ��� �������� ������ ������ ����(ȸ���� ���� �������� ������ �����ϰ� ������ �ѷ��� �Ⱥ���)

# 0.3 �̸��� ���κ��Ϸ��� ������ �ʰ� ��. sort �����ŷ� ��������
print.psych(pc3, cut = 0.3, sort = TRUE)

# �米ȸ�� - ��������
# ����ȸ�������� ���ε鳢���� ������谡 ���ٰ� �Ͽ����� ���� ����Ǿ� ���� ���ɼ��� ŭ.
pc4 <- principal(factorData, nfactors = 4, rotate = "oblimin")
print.psych(pc4, cut = 0.3, sort = TRUE)
#����� Ȱ���ؼ� ������� ���� �� �ִ�. 

#�米ȸ�� �� r���� ����Ǵ� ����� ���������. ��������� ���ϱ� ���ؼ��� ���ο� �Լ��� �����ؾ� ��.
#��������� ���ϴ� ���� �Ʒ��� ����.
factor.structure <- function(fa, cut = 0.2, decimals = 2){
  structure.matrix <- fa.sort(fa$loadings %*% fa$Phi)
  structure.matrix <- data.frame(ifelse(abs(structure.matrix) < cut, "", round(structure.matrix, decimals)))
  return(structure.matrix)
}

factor.structure(pc4, cut = 0.3)
#��������� ������İ� �������� ���� ����. ���ε� ���� ���踦 �����ؼ� ������ ������ ��� ����.


#------�������� ���ϱ�---------

pc5 <- principal(factorData, nfactors = 4, rotate = "oblimin", scores = TRUE)
pc5$scores
head(pc5$scores, 10)
factorData <- cbind(factorData, pc5$scores)

#���� ���� �� �����? ����ȸ���� ���� ���� �米ȸ���� ���� ���� ��������?

#�米ȸ�� �ÿ���? ���� ���� ���� ���̰� �ִ°�?
cor(pc5$scores)
round(cor(pc5$scores), 2)

#���� ȸ�� �ÿ���?
round(cor(pc3$scores), 2)


#self test
round(cor(pc5$scores, factorData$Q01),2)
round(cor(pc5$scores, factorData$Q06),2)
round(cor(pc5$scores, factorData$Q18),2)


#------r�� Ȱ���Ͽ� ���κм��� ���� �ð�ȭ�غ���-----

install.packages("factoextra")
library("factoextra")

#�ּ����� �� �Ϳ� ���� �׸��� �� ��. ȸ���� �� �� �ƴ�.
res.pca <- prcomp(factorData, scale = TRUE)

#���κ��� �����ϴ� ���̰հ� (Propertion Explained ���� ����)
fviz_eig(res.pca)



#���κ� ������ �ð�ȭ
#x,y�� �ξ��� �� �󸶳� �� ������ �� �ִ���
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#���ο� ���� ������ �ð�ȭ
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# ������ �������� PCA �ð�ȭ�غ���
library(FactoMineR)
result <- PCA(factorData)

##���� �����Ϳ� ���� ���Ͱ� �� �׷����� ǥ���ϱ�

#��, �� �����Ϳ����� ������ �ʹ� ���Ƽ� �� ������ ����
fviz_pca_biplot(res.pca)

#�ٸ� ����� ������ ������ ���� �����Ͱ� �ʹ� ����
install.packages("ggfortify")
library(ggfortify)
autoplot(res.pca, data = factorData, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



## ���� �����ͷ� �׷��� �ٽ� Ȱ���غ���
library(haven) 
graphData<-read_spss("factor.sav") # �ٺ��� ������ ���κм� å�� �ִ� �����Ͱ� (���߿� SPSS Ȱ�� ����)
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


#���� ���� ���ٱ� ���� ���ε� �����ϱ��
install.packages("gdata")
library(gdata)

#������ ������ ���� ���� ���� �����ؾ� ����
factorData.2<-remove.vars(factorData, c("Q19"))



##���ϴ� ��(���̰հ�, Ŀ�³ڸ�Ƽ ��)�� ���� ����
#Ŀ�³ڸ�Ƽ
pc1$communality
pc3$communality

print(pc1$communality, cutoff=0, digits=3)
print(pc3$communality, cutoff=0, digits=3)
print(pc4$communality, cutoff=0, digits=3)


#���̰հ���

pc3$values
print(pc3$values, cutoff=0, digits=3)


#Percentage of Variance Accounted For
100*pc3$values[1:23]/length(pc3$values)

print(pc3$loadings, cutoff=0, digits=3, sort = TRUE)
print(pc3$Structure, cutoff=0, digits=3)

print(pc4$loadings, cutoff=0, digits=3, sort = TRUE)
print(pc4$Structure, cut=0.3, digits=3, sort = TRUE)






