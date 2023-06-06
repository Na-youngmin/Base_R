setwd("/Users/nayoungmin/Desktop/Basic_of_Datascience/5weeks")
getwd()


###데이터 정제 예제 1: 결측값 처리

View(airquality)


##is.na 함수 사용하기
table(is.na(airquality))
table(is.na(airquality$Temp)) #Temp에는 NA가 없음을 확인함
table(is.na(airquality$Ozone)) #Ozone에는 NA가 37개 발견됨
table(is.na(airquality$Solar.R))
mean(airquality$Temp) #Temp는 평균이 구해짐
mean(airquality$Ozone) #Ozone에는 평균이 안구해짐


#Ozone 속성에서 NA가 없는 값만 추출함
air_narm = airquality[!is.na(airquality$Ozone),]
air_narm
mean(air_narm$Ozone)

##na.omit 함수 사용하기
air_narm1 <- na.omit(airquality)
mean(air_narm1$Ozone)

##na.rm을 TRUE로 설정
mean(airquality$Ozone, na.rm = T)
mean(airquality$Temp, na.rm = T)

###데이터 정제 예제 2: 이상값 처리
patients <- data.frame(name = c("환자1", "환자2", "환자3", "환자4", "환자5"),
                       age = c(22, 20, 25, 30, 27),
                       gender = c("M", "F", "M", "K", "F"),
                       blood.type = c("A", "O", "B", "AB", "C"))
patients

#성별에서 이상값 제거
patients_outrm <- patients[patients$gender == "M" | patients$gender == "F", ]
patients_outrm

#성별과 혈액형에서 이상값 제거
patients_outrm1 <- patients[(patients$gender == "F" | patients$gender == "M") & 
                              (patients$blood.type == "A" | patients$blood.type == "B" | patients$blood.type == "O" | patients$blood.type == "AB"),]
patients_outrm1

##이상값을 모두 NA로 표현한다면 NA관련 함수들을 사용할 수 있게 된다.
#성별은 남자는 1, 여자는 2로 표시, 혈액형은 A,B,O,AB형을 각각 1,2,3,4로 표현

patients <- data.frame(name = c("환자1", "환자2", "환자3", "환자4", "환자5"),
                       age = c(22, 20, 25, 30, 27),
                       gender = c(1, 2, 1, 3, 2),
                       blood.type = c(1, 3, 2, 4, 5))
patients

#성별에 있는 이상값을 결측값으로 변경
patients$gender <- ifelse((patients$gender < 1 | patients$gender > 2), NA, patients$gender)
patients

#혈액형에 있는 이상값도 결측값으로 변경
patients$blood.type <- ifelse((patients$blood.type < 1 | patients$blood.type > 4), NA, patients$blood.type)
patients

#결측값으로 표현된 이상값을 모두 제거
patients[!is.na(patients$gender) & !is.na(patients$blood.type),]

##boxplot 그리기
#airquality의 Ozone, Solar.R, Wind, Temp에 대한 boxplot을 그려보자
boxplot(airquality[,c(1:4)]) 
#Ozone의 boxplot을 그리고 최소값, 1사분위수, 중앙값, 3사분위수, 최대값도 출력해보자
boxplot(airquality[,1])$stats


#install.packages("data.table")
#install.packages("tidyverse")
library(data.table)
library(tidyverse)

## 실습데이터를 데이터프레임으로 로드!!!!!
## 실습에 사용되는 데이터 설명 https://dacon.io/competitions/official/62540/data/

DF <- read.table(file = "/Users/nayoungmin/Desktop/Basic_of_Datascience/5weeks/Pre_Season_Batter.csv", sep = ",", header = TRUE,
                 fileEncoding = "CP949", encoding = "UTF-8")

nrow(DF) #DF의 행의 개수를 구하고 싶을땐?
head(DF, 10) #DF의 처음 10개의 자료를 보고 싶을땐?

table(is.na(airquality)) ## airquality는 R의 내장 데이터

# 우리가 받은 실습데이터(Pre_Seson_Battter) 확인해보자!
table(is.na(DF))  

#DF$SLG ----> 장타율
print(table(is.na(DF$SLG)))

#DF$OBP ----> 출루율
# 한번 채워보세요

#DF$E ----> 실책
# 한번 채워보세요



# mean(평균)
mean(DF$SLG)  ## NA가 존재, 결과도 NA!
mean(DF$OBP)  ##         ""
mean(DF$E)    ##         ""


# mean에는 na.rm argument가 존재!!
mean(DF$SLG, na.rm = T)  ## NA가 존재, 결과도 NA! SLG의 NA값을 제거하고 평균을 구해보자
# 한번 채워보세요        ##         ""      OBP의 NA값을 제거하고 평균을 구해보자
# 한번 채워보세요        ##         ""                 E의 NA값을 제거하고 평균을 구해보자


# sum(합계)
sum(DF$SLG)  ## NA가 존재, 결과도 NA!
sum(DF$OBP)  ##         ""
sum(DF$E)    ##         ""
sum(DF$`height/weight`)  ## chr 형태

# sum에도 na.rm argument가 존재!!
sum(DF$SLG, na.rm = T)  ## NA가 존재, 결과도 NA! SLG의 NA값을 제거하고 합을 구해보자
# 한번 채워보세요       ##         ""  OBP의 NA값을 제거하고 합을 구해보자
# 한번 채워보세요       ##         ""        E의 NA값을 제거하고 합을 구해보자


is.na(DF$SLG)
!is.na(DF$SLG)

#DF_not_NA_SLG 변수에 SLG의 NA값이 없는것만 저장해보자
# nrow() -> 행의 수를 계산해주는 함수 
# NA값을 제거한 다음에 제거하기 전 데이터와 한번 행의 수를 비교해보세요!
DF_not_NA_SLG <- DF[!is.na(DF$SLG), ]
nrow(DF_not_NA_SLG) 

DF_not_NA_SLG <- DF[is.na(DF$SLG) == F, ]
nrow(DF_not_NA_SLG)

# 조건이 여러개라면???
# SLG가 NA가 아니고 OBP가 NA가 아닌것! (AND)의 행의수는?
nrow(DF[!is.na(DF$SLG) & !is.na(DF$OBP), ])

# SLG가 NA 또는 OBP가 NA가 아닌 것!(OR)의 행의수는?
# 한번 채워보세요!


#na.omit() -----> NA인 데이터를 지운다. 주의 NA가 포함된 행 전체를 지운다...
DF_na_omit <- na.omit(DF) #DF_na_omit변수에 DF의 NA가 있는 행을 모두 지우자 
nrow(DF_na_omit) #DF_na_omit 행의수를 구해보자

#DF_na_omit의 OBP의 평균을 구해보자
# 한번 채워보세요!

#DF의 OBP의 NA값을 제거한 뒤 평균을 구해보자
mean(DF$OBP, na.rm = T)


# 결측값을 0으로 바꾸기.
DF$SLG <- ifelse(is.na(DF$SLG), 0, DF$SLG) #DF의 SLG의 결측값을 0으로 바꿔보자

head(DF, 10)

#이상값 처리

# 모든 열의 4분위수 구하기!!!!
summary(airquality)

quar <- summary(airquality)
str(quar)

quar[,1] # 첫 번째 열, Ozone

View(airquality)
# 또한, boxplot으로 그래프와 수치까지 확인할 수 있다
## 1개만 그린다면 Ozone의 상자그림을 그리자, 스탯까지 나오게
boxplot(airquality$Ozone)$stats 

#1~4열까지의 정보를 담은 상자그림을 그리자, 스탯까지 나오게
#한번 해보세요!


# min, 1사분위수, 중앙값, 3사분위수, 최대값(초과시 이상값 분류 가능)을 구해보자!!

#Ozone의 IQR을 구해보자, NA값은 삭제
IQR(airquality$Ozone, na.rm = T) 

#Day의 IQR을 구해보자
# 한번 채워보세요!

##실습 데이터를 가지고 확인해보자!
DF <- read.table(file = "Pre_Season_Batter.csv", sep = ",", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
summary(DF)

#SLG의 상자그림을 그려보자, 스탯까지 나오게
boxplot(DF$SLG)$stats

#OBP의 상자그림을 그려보자, 스탯까지 나오게
# 한번 채워보세요!

#DF의 6열부터 14열까지 상자그림을 그려보자 스탯까지 나오게
# 한번 채워보세요!

