install.packages("processR")
##install.packages("flextable")
##install.packages("dplyr")
install.packages("lavaan")
install.packages("tidyverse")

library(processR)
##library(flextable)
##library(dplyr)
library(lavaan)
library(tidyverse)

########################################################
##########2. 다양한 변수유형의 조절분석 2번#############
########################################################
##독립변수: 부정적인 감정(연속형)
##종속변수 : 정책지지도(연속형)
##조절변수 : 연령(연속형)
##통제변수 : 긍정적인 감정, 정치성향, 성별

##개념적 모형 그리기
labels = list(X="negemot", W="age", Y="govact", C1="posemot", C2="ideology", C3="sex")
moderator = list(name="age", site=list("c"))
covar = list(name=c("posemot","ideology","sex"),site=list("Y","Y","Y"))
pmacroModel(1, labels=labels, covar=covar, radx=0.04)


##통계적모형
arrowslabels = paste0("b",1:6)
statisticalDiagram(1, labels=labels, covar=covar, radx=0.11, arrowslabels=arrowslabels, whatLabel = "label")

##회귀식
fit = lm(govact ~ negemot*age + posemot + ideology + sex, data= glbwarm)
summary(fit)
modelsSummaryTable(fit, vanilla = FALSE)

#상호작용값이 종속변수의 분산 중 어느 정도를 차지하는 지 비교
##상호작용항이 제외된 모형과 포함된 모형을 비교
fit_ex = lm(govact~negemot + age+ posemot +ideology+sex, data=glbwarm)
modelsSummaryTable(list(fit_ex,fit), labels=labels, vanilla = FALSE)

##비교를 위해 F-test실시
anova(fit_ex,fit)
##상호작용은 유의하며, 상호작용항은 정책지지의 분산의 1.25% 설명



##상호작용의 시각화 - Johnson-Neyman 방법
jnPlot(fit, addEq=TRUE)

#연령범위에 한정하여 JNPlot 그리기
jnPlot(fit, addEq = TRUE, mod.range=c(17,87))

#######################################################
######factorial ANOVA & moderation regression analysis
#######################################################
##anova
anova_test <- aov(data=caskets, interest ~ kerry*policy)
summary(anova_test)
candidate <- caskets$kerry
print(model.tables(anova_test, "means"), digits=3)
interaction.plot(caskets$policy, candidate, caskets$interest, col=caskets$kerry,
                main="CASKETS예제",
                 xlab = "policy",
                 ylab = "interest",
                 ylim = c(1.2,2.7)
)

##regression
anova_test_rg = lm(interest ~ policy*kerry, data = caskets)
summary(anova_test_rg)
modelsSummaryTable(anova_test_rg, vanilla = FALSE)
