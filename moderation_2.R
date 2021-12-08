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
##########2. �پ��� ���������� �����м� 2��#############
########################################################
##��������: �������� ����(������)
##���Ӻ��� : ��å������(������)
##�������� : ����(������)
##�������� : �������� ����, ��ġ����, ����

##������ ���� �׸���
labels = list(X="negemot", W="age", Y="govact", C1="posemot", C2="ideology", C3="sex")
moderator = list(name="age", site=list("c"))
covar = list(name=c("posemot","ideology","sex"),site=list("Y","Y","Y"))
pmacroModel(1, labels=labels, covar=covar, radx=0.04)


##���������
arrowslabels = paste0("b",1:6)
statisticalDiagram(1, labels=labels, covar=covar, radx=0.11, arrowslabels=arrowslabels, whatLabel = "label")

##ȸ�ͽ�
fit = lm(govact ~ negemot*age + posemot + ideology + sex, data= glbwarm)
summary(fit)
modelsSummaryTable(fit, vanilla = FALSE)

#��ȣ�ۿ밪�� ���Ӻ����� �л� �� ��� ������ �����ϴ� �� ��
##��ȣ�ۿ����� ���ܵ� ������ ���Ե� ������ ��
fit_ex = lm(govact~negemot + age+ posemot +ideology+sex, data=glbwarm)
modelsSummaryTable(list(fit_ex,fit), labels=labels, vanilla = FALSE)

##�񱳸� ���� F-test�ǽ�
anova(fit_ex,fit)
##��ȣ�ۿ��� �����ϸ�, ��ȣ�ۿ����� ��å������ �л��� 1.25% ����



##��ȣ�ۿ��� �ð�ȭ - Johnson-Neyman ���
jnPlot(fit, addEq=TRUE)

#���ɹ����� �����Ͽ� JNPlot �׸���
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
                main="CASKETS����",
                 xlab = "policy",
                 ylab = "interest",
                 ylim = c(1.2,2.7)
)

##regression
anova_test_rg = lm(interest ~ policy*kerry, data = caskets)
summary(anova_test_rg)
modelsSummaryTable(anova_test_rg, vanilla = FALSE)