### 데이터 가공(전반부)

#install.packages("gapminder")
library(gapminder)
library(dplyr)
glimpse(gapminder) #데이터 구조를 확인하는 함수로 str()과 비슷하다

gapminder[,c("country", "lifeExp", "year")]
gapminder[1:15,]

# 국가이름이 Croatia 인 샘플을 조건식을 사용해 추출
gapminder[gapminder$country == "Croatia",]

# 국가이름이 Croatia인것만 추출하고 인구(pop)를 출력하자
gapminder[gapminder$country == "Croatia", "pop"]

# 국가이름이 Croatia인것만 추출하고 인구(pop)와 기대수명을 출력하자
gapminder[gapminder$country == "Croatia", c("pop", "lifeExp")]

# Croatia의 1990년도 이후의 기대 수명과 인구 추출
gapminder[gapminder$country == "Croatia" & gapminder$year > 1990, c("lifeExp","pop")]

# Croatia의 기대수명과 인구의 평균을 구해보자
# apply(x, MARGIN, FUN, optional arguments to FUN): 행 또는 열별로 함수 적용. MARGIN이 1이면 행별로 2면 열별로 적용
apply(gapminder[gapminder$country == "Croatia", c("lifeExp","pop")], 2, mean)

### 샘플과 속성 추출

##select 함수 이용하기 - 열을 지정할때 "" 없이 이름을 그대로 사용할 수 있다!

# gapminder, country, year, lifeExp를 추출해보자
select(gapminder,country,year,lifeExp)

## filter 함수 이용하기 - 조건에 따라 행을 추출한다

# Croatia의 정보만 추출해보자
filter(gapminder, country == "Croatia")

## 행/열 단위의 연산

# group_by 함수를 이용해 factor형 속성을 활용해 전체 데이터를 그룹으로 분류가능
# 보통 summarise 함수를 연이어 사용해 그룹별 통계 지표를 한번에 산출

# pop_avg에 pop의 평균을 저장하고 출력하자
summarise(gapminder, avg_pop = mean(pop))

# 대륙별로 구룹짓고 pop_avg에 인구의 평균을 저장하고 출력하자
summarise(group_by(gapminder, continent), avg_pop = mean(pop))

# 대륙별로 1차그룹화하고 국가별로 2차 그룹화 한뒤 pop_avg에 pop평균을 저장하고 출력하자
summarise(group_by(gapminder,continent,country), avg_pop = mean(pop)) 

## %>% 연사자를 이용한 연속 처리

# summarise(group_by(gapminder,continent,country), avg_pop = mean(pop))를 %>% 사용해서 나타내보자
gapminder %>% group_by(continent, country) %>% summarise(pop_avg = mean(pop))

# 국가가 Croatia 이고 국가,연도,기대수명을 선택한 다음에, lifeExp_avg 변수에 기대수명 평균을 저장하고 출력하자 파이프 함수를 사용
gapminder %>% filter(country == "Croatia") %>% select(country,year,lifeExp) %>% summarise(lifeExp_avg = mean(lifeExp))


# In this chapter we’ll explore the tibble package, part of the core tidyverse.
#install.package("tidyverse")
library(tidyverse)

# Most other R packages use regular data frames, so you might want to coerce a data frame to a tibble. You can do that with as_tibble():
class(as_tibble(iris))
class(iris)

#x는 1부터 5, y는 1, z는 x의 제곱 더하기 y 값을 티블 형식으로 나타내라
tibble(
  x = 1:5, 
  y = 1 , 
  z = x^2 + y 
)

#x는 a,b y는 2,1 z는 3.6, 8.5를 트리블 형식으로 나타내라
tribble(
  ~x, ~y, ~z,
  "a", "2", "3.6",
  "b", "1", "8.5"
)

#df라는 변수에 x는 0~1 까지의 균일분포 5개, y는 정규분포 5개를 넣고 티블 형식으로 저장해라
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df # google colab or jupyter notebook을 사용하는 경우에는 %>% show() 붙여줘야 
              # 요약된 정보가 추출됨

# Extract by name, return vector
df$x #df의 균일분포를 벡터형식으로 추출하자
df[["x"]] #df의 균일분포를 벡터형식으로 추출하자

# Extract by position, return vecotr
df[[1]] #df의 균일분포를 벡터형식으로 추출하자

# column, return tibble
df["x"] #df의 열을 티블 형식으로 추출하자
df[1] #df의 열을 티블형식으로 추출하자

df %>% .$x #df를 부르고 파이핑해서 균일분포를 벡터형식으로 추출하자
df %>% .[["x"]] #df를 부르고 파이핑해서 균일분포를 벡터형식으로 추출하자

install.packages("data.table")
library(dplyr) # or library(tidyverse)
library(data.table)

# 실습 데이터 다운로드!!
 DF <- read.table(file = "/Users/nayoungmin/Desktop/Basic_of_Datascience/6weeks/Pre_Season_Batter.csv",
                  fileEncoding = "CP949",
                  encoding = "UTF-8",
                  sep = ",",
                  header = T) %>% as_tibble()

# DF <- as_tibble(DF)를 %>%으로 간소화

DF %>% show() # Rstudio에서는 tibble 변수(DF)만 입력해도 됨.

# select
# base R
DF[,c("batter_name","team")] %>% show() #DF에서 batter_name, team을 선택한뒤 보여줘라

# dplyr
DF %>% select(.,c(batter_name,team)) %>% show() # DF에서 batter_name, team을 선택하고 보여줘라
DF %>% select(.,-batter_name, -team) %>% show() # DF에서 batter_name, team을 제외하고 보여줘라
DF %>% select(., "이름" = batter_name, "팀" = team) %>% show() # DF에서 batter_name을 이름으로 team을팀으로 설정하고 보여줘라
DF %>% select(., "batter_name1" = batter_name, "batter_name2" = batter_name, "batter_name3" = batter_name) %>% show() #DF에서 batter_name을 세번 선택하고 각각이름을 batter_name1,batter_name2,batter_name3으로 설정한 뒤 보여줘라

DF %>% select(.,"이름" = batter_name, "팀" = team, 'R/G' = R/G) %>% show() #DF에서 batter_name을 이름으로 설정, team을 팀으로, R/G는 'R/G'로 설정하고 보여줘라
View(DF) #DF를 보여줘라 

# %>%의 마지막 단계가 지정된 변수에 저장된다.
DF %>% show()
DF_2 <- DF %>% select(., 이름 = batter_name, 팀 = team, `R/G` = R/G) 
DF_2 %>% show()

# filter
# base R
DF[DF$team == "한화", c("team","batter_name")] %>% show() #DF의 팀이 한화인 team, batter_name만 보여줘라
# DF의 팀이 한화이고 G가 5이상 또는 AB가 15이상인 team, batter_name, G, AB만 추출해서 중복을 제거하고 보여주라
DF[DF$team == "환화" & (DF$G >=5 | DF$AB >= 15), c("team", "batter_name","G","AB")] %>% unique() %>% show() 


# dplyr
DF %>% filter(.,team == "한화" & (DF$G >= 5 | DF$AB >= 15)) %>%  show() #filter를 사용해서 팀이 한화이고 G가 5이상 또는 AB가 15이상인 것을 추출해 보여주라
##filter를 사용해서 팀이 한화이고 G가 5이상또는 AB가 15이상인 것을 추출한 후에 team, batter_name,G,AB를 선택하고 중복을 제거한 뒤 보여주라
DF %>% filter(.,team == "한화" & (DF$G >= 5 | DF$AB >= 15)) %>% select(.,team, batter_name,G,AB) %>% unique() %>% show()

# groupby, summarise
DF %>% group_by(team) %>% show() #DF를 팀별로 묶고 보여줘라
#DF를 팀별로 묶고 G,AB의 평균을 각각 G_AVG,AB_A로 저장하고 R의 합을 R_SUM으로 저장한 뒤 요약한 후, 보여줘라
DF %>% group_by(team) %>% summarise(.,G_AVG = mean(G), AB_A = mean(AB), R_SUM = sum(R)) %>% show()
#DF를 팀,이름별로 묶고 G,AB,R의 평균을 각각 G_AVG,AB_A,R_AVG로 저장하고 요약한 후, 보여줘라
DF %>% group_by(team, batter_name) %>% summarise(.,G_AVG = mean(G), AB_A = mean(AB), R_AVG = mean(R)) %>% show()
#DF_3에 DF를 팀,이름별로 묶고 G,AB,R의 평균을 각각 G_AVG,AB_A,R_AVG로 저장하고 요약한 후 저장해라
DF_3 <- DF %>% group_by(team, batter_name) %>% summarise(., G_AVG = mean(G) ,AB_A = mean(AB) ,R_AVG = mean(R))
View(DF_3) #DF_3 보기

