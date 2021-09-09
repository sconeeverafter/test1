install.packages("dplyr")
library(dplyr)

x <- c(30, 20, 10,0)
x %>% 
  abs() %>% 
  mean() %>% 
  sqrt()
help(abs)

#filter

install.packages("nycflights13")
library(nycflights13)
head(flights)
flight_df <-data.frame(flights)
str(flights)


flight_df %>% 
  filter(month ==2) %>% 
  count(month)   #get this habit 중간확인절차

table(flight_df$month)

flight_df %>% 
  filter(month==2|day==1) %>% 
  head(5)

flight_df %>% 
  filter(month==2, day==1 ) %>% 
  head(5)

flight_df %>% 
  filter(month!=2) 

flight_df %>% 
  filter(month>=5) %>% 
  head(5)

#복수의 조건
flight_df %>% 
  filter(month %in% c(5,7,10))

#na값 표시 또는 제거해서 subset, 미싱값 제외하고 불러와줘

flight_df %>% 
  filter(is.na(month))

flight_df %>% 
  filter(!is.na(month))

filter_flight_df <-
  flight_df %>% 
  filter(month %in% c(5,7,10))


#select
select_flight_df <-
  flight_df %>% 
  select(month, day) 
str(select_flight_df)

select_flight_df<-
  flight_df %>% 
  select(!year:day)
str(select_flight_df)

select_flight_df <-
  flight_df %>% 
  select(-c(year,month))

#arrange

arrange_flight_df <-
  flight_df %>% 
  arrange(month,day)

arrange_flight_df <-
  flight_df %>% 
  arrange(-month, -day)

#mutate

flight_df %>% 
  mutate(mean_distance = distance/hour,
         ratio_delay = arr_delay/(hour*60+minute))
#이거는 저장이 안되는 형태

mutate(meanq = q1+q2+q3+q4+q5/5)

flight_df %>% 
  mutate(arr_delay = ifelse(arr_delay>0, "delay", "no delay"))

mutate_flight_df <-
  flight_df %>% 
  mutate(arr_delay_group = ifelse(arr_delay>0, "delay", "no delay"))










