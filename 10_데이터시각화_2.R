install.packages("data.table")
library(tidyverse)
library(data.table)
library(ggplot2)

DF <- read.csv("Regular_Season_Batter.csv", sep = ",", header = T, fileEncoding = "CP949", encoding = "UTF-8") %>% as_tibble()
View(DF)
str(DF)

# 1. ggplot 빈배경 만들기
ggplot()
# 2. 데이터와 x값 y값을 선택 
# ggplot x 값을 G, y  값을 R로 만들기
ggplot(DF, aes(x = G, y = R)) 

# 3. 그릴 그래프의 종류를 선택해주자
# https://ggplot2.tidyverse.org/reference/geom_point.html
# x = AB(타수), y = R(득점수) 에 대한 산점도 그리기 (ggplot 사용)
ggplot(DF, aes(x = AB, y = R)) +
  geom_point()

# geom_point()
# aes(color, alpha)
#x = AB(타수), y = R(득점수), 색은 G의 정도에 따라 설정 한뒤 산점도 그리기 (ggplot 사용) 투명도는 0.5
ggplot(DF, aes(x = AB, y = R, color = G)) + # 데이터에 관한 옵션은 ggplot()에
  geom_point(alpha = 0.5) # 그래프에 관한 옵션은 그래프에

# geom_point()
# aes(color)
#x = AB(타수), y = R(득점수), 색은 팀별로 설정 한뒤 산점도 그리기 (ggplot 사용) 투명도는 2/10, 점 크기는 5
ggplot(DF, aes(x = AB, y = R, color = team)) +
  geom_point(alpha = 2/10, size = 2)

# geom_point()
# aes(size)
#x = AB(타수), y = R(득점수), 색, 크기는 G의 정도에 따라 설정 한뒤 산점도 그리기 (ggplot 사용) 투명도는 0.1
ggplot(DF, aes(x = AB, y = R, color = G, size = G)) +
  geom_point(alpha = 1/10)

# geom_point()
# 두개이상의 변수를 나타내고 싶을때? 산점도에 산점도를 더하자!

#x에 G, y에 avg값을 넣고 색은 tan3으로 설정, 투명도는 0.5로 설정한 산점도와
#x에 G, y에 OPS값을 넣고 색은 marron2로 설정, 투명도는 0.5로 설정한 산점도를 동시에 그리자
ggplot() +
  geom_point(data = DF, aes(x = G, y = avg), colour = "tan3", alpha = 0.5) +
  geom_point(data = DF, aes(x = G, y = OPS), colour = "maroon2", alpha = 0.5)


## 막대그래프 그리기

# team별 R(득점수)와 H(안타수)의 합

# DF를 team별로 그룹지은 뒤에 R_SUM에 결측값을 제외환 R의 합을 저장,
#H_SUM에 결측값을 제외한 H의 합을 저장 한 뒤 team_R_H_sum에 저장
team_R_H_sum <- DF %>% group_by(team) %>% 
  summarise(R_SUM = sum(R, na.rm = T), H_SUM = sum(H, na.rm = T))
View(team_R_H_sum)
#g 에 team_R_H_sum 데이터와 x는 team, y는 R_SUM으로 설정
g <- ggplot(data = team_R_H_sum, aes(x = team, y =R_SUM))
g + geom_bar(stat = "identity") # 막대그래프 그리기 y값이 반영되게 그리기

#g 에 team_R_H_sum 데이터와 x는 team으로 설정, 막대그래프 그리기 y값이 반영안되게 그리기
ggplot(team_R_H_sum, aes(x = team)) + geom_bar()

# color
# 막대그래프 그리기 y값이 반영되게 그리기, 테두리를 팀별 색으로 칠하기
g + geom_bar(stat = "identity", aes(color = team))

# aes에서 data를 따로 정의하지 않는 이상, 
# 이전에 설정된 data를 그대로 사용한다.

# fill
## 막대그래프 그리기 y값이 반영되게 그리기, 막대를 팀별 색으로 채우기
g + geom_bar(stat = "identity", aes(fill = team))

# reorder
## 막대그래프 그리기 y값이 반영되게 그리기, team, R_SUM을 오름차순으로 정렬, 막대를 팀별 색으로 채우기
g + geom_bar(stat = "identity", aes(reorder(team, R_SUM), fill = team))

# reorder
## 막대그래프 그리기 y값이 반영되게 그리기, team, R_SUM을 내림차순으로 정렬, 막대를 팀별 색으로 채우기
g + geom_bar(stat = "identity", aes(reorder(team, -R_SUM), fill = team))

# 막대그래프를 가로로 그리고 싶을땐? coord_flip() 
## 막대그래프 그리기 y값이 반영되게 그리기, team, R_SUM을 내림차순으로 정렬, 막대를 팀별 색으로 채우기, 가로로 그리기
g + geom_bar(stat = "identity", aes(reorder(team, -R_SUM), fill = team)) + coord_flip()

# geom_histgram
#DF에서 H의 히스토그램 그리기 (team별로 테두리 색칠하기)
ggplot(DF,aes(H,color = team)) + geom_histogram()

# geom_histgram
#DF에서 H의 히스토그램 그리기 (team별로 막대 색 채우기)
DF %>% ggplot(aes(H, fill = team)) + geom_histogram()

# geom_histgram
#DF에서 H의 히스토그램 그리기 (team별로 막대 색 채우기) 구간 12개
DF %>% ggplot(aes(H, fill = team)) + geom_histogram(bins = 12)

# geom_histgram
#DF에서 H의 히스토그램 그리기 (team별로 막대 색 채우기) 구간 1개
DF %>% ggplot(aes(H, fill = team)) + geom_histogram(bins = 1)

# 단위를 일정한 수로 나누고 싶을땐? scale_xy_continuous
#DF에서 H의 히스토그램 그리기 (team별로 막대 색 채우기) 구간 12개
# x값 범위 0 ~ 220, 10씩 끊기
# y값 범위 0~800, 20씩 끊기
DF %>% ggplot(aes(H, fill = team)) + geom_histogram(bins = 12) +
  scale_x_continuous(breaks = seq(0,220,10)) +
  scale_y_continuous(breaks = seq(0,800,20))

# position = "dodge"
#DF에서 H의 히스토그램 그리기 (team별로 막대 색 채우기), 팀별로 그룹지어서 표기하기
# x값 범위 0 ~ 220, 10씩 끊기
# y값 범위 0~800, 20씩 끊기
DF %>% ggplot(aes(H, fill = team)) + geom_histogram(position = "dodge") +
  scale_x_continuous(breaks = seq(0,220,10)) +
  scale_y_continuous(breaks = seq(0,800,20))

# geom_boxplot
DF %>% ggplot(aes(x = team, y = R)) +
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot")

# geom_boxplot
# http://blog.naver.com/nife0719/221000580841
DF %>% ggplot(aes(x = team, y = R)) +
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 15,face = "bold"),
        axis.title.y = element_text(family = fonts()[1], size = 15,face = "bold"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))

# geom_boxplot
# http://blog.naver.com/nife0719/221000580841
DF %>% ggplot(aes(x = team, y = R, fill = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 15,face = "bold", color = "red"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold", color = "blue"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))

# geom_boxplot
# http://blog.naver.com/nife0719/221000580841
DF %>% ggplot(aes(x = team, y = R, color = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 15,face = "bold", color = "red"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold", color = "blue"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))

## 어떤 색이 있는지 궁금할땐??
library(RColorBrewer)
RColorBrewer::display.brewer.all()

RColorBrewer::brewer.pal.info %>% show()

# geom_boxplot
DF %>% ggplot(aes(x = team, y = R, color = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))

# geom_boxplot
DF %>% ggplot(aes(x = team, y = R, color = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))+
  scale_color_brewer(palette = "PuOr")

# geom_boxplot
DF %>% ggplot(aes(x = team, y = R, fill = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))

# geom_boxplot
DF %>% ggplot(aes(x = team, y = R, fill = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))+
  scale_fill_brewer(palette = "PuOr")

# geom_boxplot
DF %>% ggplot(aes(x = team, y = R, fill = team, color = team)) + # 또는 color
  geom_boxplot() +
  ggtitle("Team 별 R(득점) boxplot") +
  theme(plot.title = element_text(family = fonts()[1], size = 25, face = "bold", color = "maroon2", hjust = 0.5), 
        axis.title.x = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.title.y = element_text(family = fonts()[1], size = 20,face = "bold"),
        axis.text.x = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"),
        axis.text.y = element_text(family = fonts()[1], size = 12, angle = 90, face = "bold"))+
  scale_fill_brewer(palette = "PuOr")
