rm(list=ls())

##------------조건부 과정모형 실습--------------##

# I. processR 설치 -------------------------------------------------------------
# 참고문헌 : 문건웅. (2019). R을 이용한 조건부과정분석. 학지사.

# 1. 필요한 패키지 설치 --------------------------------------------------------
install.packages("jsonlite", type = "source")
# devtools : 개발자용 패키지, 여기에서는 깃헙에 있는 패키지를 설치 하는데 사용 | https://devtools.r-lib.org
install.packages("devtools")
# tidyverse : 데이터분석에 필요한 패키지, stringr : 문자열을 쉽게 처리하도록 도와주는 함수세트 제공
install.packages(c("tidyverse","stringr"))
# rlang : R 및 Tidyverse 패키지 활용을 위한 기반 패키지
install.packages("rlang", dependencies = TRUE)
# lavaan : 확인적 요인분석, 구조방정식, 잠재성장모형 등 고급통계를 쉬운 문법으로 구동시킬 수 있는 패키지
# https://cran.r-project.org/web/packages/lavaan/index.html
install.packages("lavaan", dependencies = TRUE)
# ggplot2 : 시각화 패키지
install.packages("ggplot2")

# predict3d, processR : 저자가 개발한 매개, 조절, 조건부과정분석 패키지
devtools::install_github("cardiomoon/predict3d", force = TRUE)
devtools::install_github("cardiomoon/processR", force = TRUE)

# 2. 패키지 실행 ---------------------------------------------------------------

# 저자는 library 보다 require를 사용하여 자체개발 패키지를 불러올 것을 권장하고 있음
# library 함수는 설치되어있지 않은 패키지를 불러오는 경우에 오류값을 반환
# require 함수는 설치되어있지 않은 패키지를 불러오는 경우에 경고메시지를 보여줌

options(rgl.useNULL=TRUE) # processR, predict3D 실행 시 error: OpenGL is not available in this build 메시지가 나오는 것을 막기 위함
library(rgl)
require(processR)
require(predict3d)
library(ggplot2)
library(lavaan)

# II. PROCESS Macro for R 설치 -------------------------------------------------
# 필요한 파일 다운로드 및 PROCESS 활성화 ---------------------------------------
    # http://processmacro.org/download.html
    # Download PROCESS v4.0 클릭
    # 파일 압축해제
    # processv40 폴더
    # PROCESS v4.0 for R 폴더
    # process.R 파일을 Rstudio 파일에서 열기
    # 전체 코드실행 (5,369행, 1~2분 소요)
    # PROCESS 가동됨 (SPSS에서 syntax를 사용할 때 PROCESS.sps 파일을 여는것과 유사한 개념)

# III. 실습사례 : 직장 내 성차별 연구 ------------------------------------------

# 1. 데이터 불러오기 -----------------------------------------------------------
getwd()
setwd("/Users/sweethome/Onedrive/OneDrive - SNU/R/R/processv40/PROCESS v4.0 for R")
Data <- read.csv("liking.csv", header = TRUE)


# 2.  데이터 들여다보기 --------------------------------------------------------
str(Data) # 구조확인 : 5개 변수, 129개 관측치


# 3. 모형 시각화, 회귀계수 요약 ------------------------------------------------
labels = list (X="protest", M="respappr", W="sexism", Y="liking") #변수지정
pmacroModel(8,labels=labels) #8번 모델 개념적모형 시각화
statisticalDiagram(8,labels=labels) #8번 모델 통계적모형 시각화

#회귀계수 요약표
moderator=list(name="sexism", site=list(c("a","c")))
equation = tripleEquation(labels=labels,moderator=moderator,mode=1)
cat(equation)
fit = eq2fit(equation, data=Data)
modelsSummaryTable(fit,labels=labels)

#회귀계수를 이용한 통계적 모형
drawModel(fit,labels=labels,whatLabel="est")

# 4. PROCESS Macro Syntax 입력 ----------------------------------------------
process (data = Data,
         y="liking",
         x="protest",
         m="respappr",
         w="sexism",
         model = 8,
         boot = 10000,
         seed = 191217)

# 5. PROCESS Macro 결과 해석 --------------------------------------------------

# 6.  조건부 직접효과 및 간접효과의 시각화 ------------------------------------------------
model = tripleEquation(labels=labels,moderator=moderator,rangemode=2,data=Data)
cat(model)
semfit = sem(model=model,data=Data,se="boot")

conditionalEffectPlot(semfit,data=Data) + labs(x="Perceived Pervasiveness of Sex Discrimination(w)",y="Effect of Lawyer's Behavior on Liking")
