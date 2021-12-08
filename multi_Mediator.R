#설치 
install.packages("processR")
install.packages("lavaan")
library(processR)
library(lavaan)

#병렬 예제
cor.test(pmi$pmi,pmi$import)
#모델 그리기
labels=list(X="cond",M=c("import","pmi"),Y="reaction")
drawModel(labels=labels,parallel=TRUE,serial=FALSE)
#회귀계수 요약 표
modelsSummaryTable(labels=labels,data=pmi)
#모형 분석, lavaan syntax
model=multipleMediation(labels=labels)
cat(model) #print와 비슷하지만 행을 안바꾸는 프린트

#부트스트래핑
set.seed(1) #규칙적인 난수 생성 (재현 가능성을 위함)
semfit=sem(model=model,data=pmi,se="boot")
summary(semfit,ci=TRUE)
#회귀분석 요약
estimatesTable2(semfit,digits=3)
#회귀계수를 넣어 다시 그리기
drawModel(semfit,whatLabel="est",parallel=TRUE,labels=labels)
#매개효과 요약
medSummaryTable(semfit)


#서로다른 부호를 가진 두 간접효과의 크기 비교/절대값으로 비교 ex. |a1b1|-|a2b2|
labels=list(X="cond",M=c("import","pmi"),Y="reaction")
model=multipleMediation(labels=labels,data=pmi,contrast=2)
cat(model)

set.seed(123)
semfit=sem(model=model, data=pmi, se="boot")
summary(semfit,ci=TRUE)
drawModel(semfit,whatLabel="est",parallel=TRUE,labels=labels)



#------------------------------------------------------------------

#직렬다중 매개모형
labels=list(X="X",M=c("M1","M2"), Y= "Y")
drawModel(labels=labels, serial=TRUE,nodemode=4)
labels=list(X="X",M=c("M1","M2","M3"), Y= "Y")
#nodemode는 x축의 간격, ylim은 y축 간격 조절
drawModel(labels=labels, serial=TRUE,nodemode=4, ylim=c(0.4,1))

labels=list(X="cond",M=c("import","pmi"),Y="reaction")
drawModel(labels=labels, serial=TRUE)
#회귀계수 요약표
modelsSummaryTable(labels=labels, data=pmi, serial=TRUE)
#lavaan syntax
model=multipleMediation(labels=labels, serial=TRUE)
cat(model)

set.seed(1)
semfit=sem(model=model, data=pmi, se="boot")
summary(semfit, ci=TRUE)
drawModel(semfit,whatLabel="est",serial=TRUE,labels=labels)
medSummaryTable(semfit)


#------------------------------------------------------------------
#직렬병렬 다중모형
equation='M1~X
M2~X+M1
M3~X+M1
Y~X+M1+M2+M3'
node.pos=list(X=c(0,0.5),M1=c(0.5,0.45),M2=c(0.75,0.9),M3=c(0.75,0.1),Y=c(1,0.5))
curved.arrow=list(c=-0.1)
drawModel(equation=equation,nodemode=2,node.pos=node.pos,curved.arrow=curved.arrow)


equation='M1~X
M2~X
M3~X
M4~X+M1+M2+M3 
Y~X+M1+M2+M3+M4'
node.pos=list(X=c(0,0.5),M1=c(0.35,0.9),M2=c(0.35,0.5),M3=c(0.35,0.1),M4=c(0.7,0.5),Y=c(1,0.5))
curved.arrow=list(a4=0.15,b2=0.15)
segment.arrow=list(c=0.5)
drawModel(equation=equation,nodemode=2,node.pos=node.pos,radx=0.08,curved.arrow=curved.arrow,
          segment.arrow=segment.arrow)
