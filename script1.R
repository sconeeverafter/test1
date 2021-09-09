1+1

install.packages("dplyr")
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

View(exam)
summary(exam)
summary(exam$id)
str(exam)

exam %>% filter(class ==1)
exam %>% filter(class == 2)
exam %>% filter(class != 1)

exam %>% filter(class ==1 & math >= 50)
exam %>% filter(math >=90 | english >=90)
exam %>% filter(class==1 | class==3 | class==5)
exam %>% filter(class %in% c(1.3, 5))

class1 <- exam %>% filter(class==1)
mean(class1$math)


x <- c(5, 12, 13, 12)
xf <- factor(x)
xf

str(xf)
unclass(xf)

a<- c("ha", "kim", "park")
b <-c('f')


a <- 1
a
b <- "hello!"
b

class(a)
class(b)

x1 <- data.frame(var1 =c(1,2,3),
                var2 = c("a","b","c"))
x1

class(x1)

exam %>% select(-math) %>% head
exam %>% filter(class==1) %>% select(english)

exam %>% 
  filter(class==1
         ) %>% 
  select(english)


exam %>%  filter(class==2)

exam %>% summarise(mean_math = mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))


exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())

mpg
qplot(mpg)

mpg %>% 
  group_by(manufacturer, drv)

install.packages("ggplot2")
library(ggplot2)

mpg %>%
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate( tot= (cty+hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

mpg_new <- mpg
mpg_new %>% mutate(chtotal = cty + hwy)
mpg_new %>% mutate %>% summarise(mean_chtotal = mean(chtotal))


test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60, 80, 70, 90, 85))
test1

test2 <- data.frame(id =c(1,2,3,4,5),
                    final= c(70,83,65,95,80))
test2

total <- left_join(test1, test2, by= "id")
tatal
total

name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))
name
str(exam)
exam_new <- left_join(exam, name, by ="class")
exam_new %>% head(4)

group_a <-data.frame(id =c(1,2,3,4,5),
                     test = c(60,80,70,90,85))
group_b <-data.frame(id=c(6,7,8,9,10),
                     test = c(70,83,65,95,80))
group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all


fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl=c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
str(mpg)
mpg_new <- left_join(mpg, fuel, by="fl")
mpg_new

mpg_new %>% select(model, fl, price_fl) %>% 
  head(5)

mpg <-as.data.frame(ggplot2::mpg)

mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty))

midwest_new <- as.data.frame(ggplot2::midwest)

midwest_new <- midwest_new %>% 
  mutate(ratio = (poptotal-popadults)/poptotal*100)

summary(midwest_new$ratio)
str(midwest_new$ratio)

midwest_new %>%
  arrange(desc(ratio)) %>% 
  select(county, ratio) %>% 
  head(5)

View(midwest_new)


midwest_new<- midwest_new %>% 
  mutate(grade= ifelse(ratio>=40, "large", ifelse(ratio>=30, "middle", "small")))

table(midwest$popasian)


midwest%>% 
  mutate(ratio_asian = popasian/poptotal*100) %>% 
  select(state, county, ratio_asian) %>% 
  arrange(ratio_asian) %>% 
  head(10)




