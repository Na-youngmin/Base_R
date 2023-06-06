install.packages("data.table")
library(tidyverse)
library(data.table)
getwd()

DF <- read.csv("Regular_Season_Batter.csv", header = T,sep = ",",fileEncoding = "CP949", encoding = "UTF-8") %>% as_tibble()
View(DF)
DF %>% str()

# https://dacon.io/competitions/official/235546/data/
# G(출전게임수), avg(타율), R(득점), SO(삼진아웃)

# DF를 활용해 x는 G, y는 avg인 그래프 그리기
plot(x = DF$G, y = DF$avg)

# R(득점)에 대한 avg(평균)의 그래프 그리기 (포뮬러 사용)
plot(DF$avg ~ DF$R)

# AB(타수), H(안타) 
#5~12열의 그래프 그리기
DF[,5:12] %>% plot()

# 숫자형 열 전체..
#5~23열의 그래프 그리기
DF[,5:23] %>% plot()

# cov() 
#G와 avg의 공분산 구하기
cov(DF$G, DF$avg)

# 결측값 처리, na.omit을 이용해 행 전체를 날릴 것인가... 아니면 NA를 0으로 바꿀 것인가!! 선택의 자유~
# na값을 0으로 바꾸고 DF_NA 변수에 저장
DF_NA <- DF %>% replace(is.na(.), 0) # tidyr replace 이용

#DF_NA의 공분산 구하기
cov(DF_NA$G, DF_NA$avg) # G(경기수), avg(타율)
cov(DF_NA$avg, DF_NA$HR) # avg(타율), HR(홈런)
cov(DF_NA$G, DF_NA$AB) # G(경기수), AB(타수)

# DF_NA의 5~12열의 공분산 구하기
cov(DF_NA[,5:12])
DF_NA[,5:12] %>% cov()


#DF_NA의 상관계수 구하기
cor(DF_NA$G, DF_NA$avg) # G(경기수), avg(타율)
cor(DF_NA$avg, DF_NA$HR) # avg(타율), HR(홈런)
cor(DF_NA$G, DF_NA$AB) # G(경기수), AB(타수)
#avg_HR_cor_M에 DF_NA변수의 5~12열의 상관계수를 구한 뒤 저장
avg_HR_cor_M <- DF_NA[,5:12] %>% cor()
View(avg_HR_cor_M)

# 상관계수 plotting 패키지 (https://rpubs.com/cardiomoon/27080)
install.packages("corrplot")
library(corrplot)
?corrplot

#avg_HR_cor_M의 산점도 그리기
# method = circle
corrplot(avg_HR_cor_M, method = "circle")

# method 변경
# method = square
corrplot(avg_HR_cor_M, method = "square")

# method 변경
# method = number
corrplot(avg_HR_cor_M, method = "number")

# method 변경
# method = pie
corrplot(avg_HR_cor_M, method = "pie")

# type 변경
# type = upper
corrplot(avg_HR_cor_M, type = "upper")

# type 변경
# type = lower
corrplot(avg_HR_cor_M, type = "lower")

# 섞어서 보여주기
corrplot.mixed(avg_HR_cor_M)

# 섞어서 보여주기 (아래는 사각형, 위에는 원)
corrplot.mixed(avg_HR_cor_M, lower = "square", upper = "circle")

#상관관계가 높은 값들은 묶어주기
corrplot(avg_HR_cor_M, order = "hclust", addrect = 3)

# 연도별 팀 평균 타율
# 1. DF를 팀,연도별로 묶은뒤 
# 2. avg변수의 na값을 제거한뒤 avg_AVG에 저장한 것을 요약한 뒤
# 3. KIA,SK,삼성 팀만 추출한 것을 team_year_avg에 저장하자

team_year_avg <- DF %>% group_by(team, year) %>% 
  summarise(avg_AVG = mean(avg, na.rm = T)) %>% 
  filter(team == "KIA" | team == "SK" | team == "삼성")

View(team_year_avg)

# 기본 plot
# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
plot(x = team_year_avg$year, y = team_year_avg$avg_AVG)

# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
plot(x = team_year_avg$year, y = team_year_avg$avg_AVG, type = "n")

# main, xlab, ylab
# x 축은 YEAR, y축은 AVG로 이름 설정
# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# 이름은 Team batting average by Year
# type "n"의 상태에서 point함수로 채움
# x 축은 YEAR, y축은 AVG로 이름 설정
plot(x = team_year_avg$year,
     y = team_year_avg$avg_AVG,
     main = "Team batting average by Year",
     type = "n",
     xlab = "YEAR",
     ylab = "AVG")


# point를 이용한 그래프 추가
# team_year_avg에서 각 팀 추출하기
samsung <- team_year_avg %>% filter(team == "삼성") # samsung에 삼성만 추출해 저장
sk <- team_year_avg %>% filter(team == "SK") #sk에 SK만 추출해 저장
kia <- team_year_avg %>% filter(team == "KIA") #kia에 KIA만 추출해 저장

# x는 연도, y는 avg_AVG로 설정 해 각 팀 점 찍기
points(x = samsung$year, y = samsung$avg_AVG) #samsung
points(x = sk$year, y = sk$avg_AVG) #sk
points(x = kia$avg_AVG, y = kia$avg_AVG) #kia

# col
# team 별 색상 변경
# 단, 그룹화 변수로 주어야함(Factor)
# main, xlab, ylab
# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
# 이름은 Team batting average by Year
# x 축은 YEAR, y축은 AVG로 이름 설정
plot(x = team_year_avg$year,
     y = team_year_avg$avg_AVG,
     type = "n",
     main = "Team batting average by Year",
     xlab = "YEAR",
     ylab = "AVG")

# x는 연도, y는 avg_AVG로 설정 해 각 팀 점 찍기
points(x = samsung$year, y = samsung$avg_AVG, col = "maroon2") #samsung (maroon2)
points(x = sk$year, sk$avg_AVG, col = "gold4") #sk (gold4)
points(x = kia$year, kia$avg_AVG, col = "grey32") #kia (grey32)

# pch
# 점의 모양 변경
# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
# 이름은 Team batting average by Year
# x 축은 YEAR, y축은 AVG로 이름 설정

plot(x = team_year_avg$year,
     y = team_year_avg$avg_AVG,
     type = "n",
     main = "Team batting average by Year",
     xlab = "YEAR",
     ylab = "AVG")

# x는 연도, y는 avg_AVG로 설정 해 각 팀 점 찍기
points(x = samsung$year, y = samsung$avg_AVG, col = "maroon2", pch = 8) #samsung, 색은 maroon2, 점 모양 8
points(x = sk$year, y = sk$avg_AVG, col = "gold4", pch = 11) #sk, 색은 gold4, 점 모양 11
points(x = kia$year, kia$avg_AVG, col = "grey32", pch = 4) #kia, 색은 grey32, 점 모양 4

# cex
# 점의 크기 변경
# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
# 이름은 Team batting average by Year
# x 축은 YEAR, y축은 AVG로 이름 설정
plot(x = team_year_avg$year,
     y = team_year_avg$avg_AVG,
     type = "n",
     main = "Team batting average by Year",
     xlab = "YEAR",
     ylab = "AVG")

points(x = samsung$year, y = samsung$avg_AVG, col = "maroon2", pch = 8, cex = 2) #samsung, 색은 maroon2, 점 모양 8, 점크기 2
points(x = sk$year, sk$avg_AVG, col = "gold4", pch = 11, cex = 2) #sk, 색은 gold4, 점 모양 11, 점크기 2
points(x = kia$year, y = kia$avg_AVG, col = "grey32", pch = 4, cex = 2) #kia, 색은 grey32, 점 모양 4, 점크기 2

# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
# 이름은 Team batting average by Year
# x 축은 YEAR, y축은 AVG로 이름 설정
plot(x = team_year_avg$year, y = team_year_avg$avg_AVG, type = "n", main = "Team batting average by Year", xlab = "YEAR", ylab = "AVG")
points(x = samsung$year, y = samsung$avg_AVG, col = "maroon2", pch = 8, cex = 2) #samsung, 색은 maroon2, 점 모양 8, 점크기 2
points(x = sk$year, y = sk$avg_AVG, col = "gold4", pch = 11, cex = 2) #sk, 색은 gold4, 점 모양 11, 점크기 2
points(x = kia$year, y = kia$avg_AVG, col = "grey32", pch = 4, cex = 2) #kia, 색은 grey32, 점 모양 4, 점크기 2
legend("topleft", legend = c("삼성", "SK", "KIA")) #위치: 좌측상단, 내용: 삼성, SK, KIA 범례 만들기

# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
# 이름은 Team batting average by Year
# x 축은 YEAR, y축은 AVG로 이름 설정
plot(x = team_year_avg$year, y = team_year_avg$avg_AVG, type = "n", main = "Team batting average by Year", xlab = "YEAR", ylab = "AVG")
points(x = samsung$year, y = samsung$avg_AVG, col = "maroon2", pch = 8, cex = 2)#samsung, 색은 maroon2, 점 모양 8, 점크기 2
points(x = sk$year, y = sk$avg_AVG, col = "gold4", pch = 11, cex = 2)#sk, 색은 gold4, 점 모양 11, 점크기 2
points(x = kia$year, y = kia$avg_AVG, col = "grey32", pch = 4, cex = 2)#kia, 색은 grey32, 점 모양 4, 점크기 2
#위치: 좌측상단, 내용: 삼성, SK, KIA 범례 만들기 , 점모양 8,11,4, 색: maroon2, gold4, grey32, 점 크기 0.5
legend("topleft", legend = c("삼성", "SK", "KIA"), pch = c(8,11,4), col = c("maroon2", "gold4", "grey32"), cex = 0.5)

# team_year_avg데이터에서 x는 year, y는 avg_AVG 값을 설정한 뒤 그래프를 그리자
# type "n"의 상태에서 point함수로 채움
# 이름은 Team batting average by Year
# x 축은 YEAR, y축은 AVG로 이름 설정
plot(x = team_year_avg$year, y = team_year_avg$avg_AVG, type = "n", main = "Team batting average by Year", xlab = "YEAR", ylab = "AVG")

# 삼성
points(x = samsung$year, y = samsung$avg_AVG, col = "maroon2", pch = 8, cex = 1) #samsung, 색은 maroon2, 점 모양 8, 점크기 1
lines(x = samsung$year, y = samsung$avg_AVG, col = "maroon2") # samsung, 색은 maroon2 선 그리기

# SK
points(x = sk$year, y = sk$avg_AVG, col = "gold4", pch = 11, cex = 1) #sk, 색은 gold4, 점 모양 11, 점크기 1
lines(x = sk$year, y = sk$avg_AVG, col = "gold4") #sk, 색은 gold4 선 그리기

# KIA
points(x = kia$year, y = kia$avg_AVG,col = "grey32", pch = 4, cex = 1) #kia, 색은 grey32, 점 모양 4, 점크기 1
lines(x = kia$year, y = kia$avg_AVG,col = "grey32") #kia, 색은 grey32 선 그리기

#위치: 우측하단, 내용: 삼성, SK, KIA 범례 만들기 , 점모양 8,11,4, 색: maroon2, gold4, grey32, 점 크기 0.5
legend("bottomright", legend = c("삼성", "SK", "KIA"), pch = c(8,11,4), col = c("maroon2", "gold4", "grey32"), cex = 0.5)
