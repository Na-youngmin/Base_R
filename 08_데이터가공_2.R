setwd("/Users/nayoungmin/Desktop/Basic_of_Datascience/7weeks")
getwd()
library(dplyr)


#### 데이터가공 후반부

avocado <- read.csv("avocado.csv",header = T)
str(avocado)

# 경향 도출을 위해 총 판매량과 평균 가격 속성을 지역에 따라 구분하여 각각 요약
(x_avg = avocado %>% group_by(region) %>% summarise(V_avg = mean(Total.Volume),
                                                    P_avg = mean(AveragePrice)))
# 지역별 특징을 연도별로 다시 세분화
(x_avg = avocado %>% group_by(region, year) %>% summarise(V_avg = mean(Total.Volume),
                                                          P_avg = mean(AveragePrice)))

# 유기농 여부를 기준으로 한번 더 세분화
(x_avg = avocado %>% group_by(region,year,type) %>% summarise(V_avg = mean(Total.Volume),
                                                              P_avg = mean(AveragePrice))) %>% View()
## 정렬 arrange함수 사용하기

# x_avg의 V_avg를 내림차순으로 정렬하자
arrange(x_avg, desc(V_avg))
x_avg %>% arrange(desc(V_avg))

## 데이터 정렬과 검색 - 데이터 셋에 중간 통계 값이 포함된 경우도 있으므로 주의가 필요

# 지역 열에서 TotalUS를 없앤 값들을 x_avg1에 저장하자
x_avg1 <- x_avg %>% filter(region != "TotalUS")

#TotlaUS를 제외하고 나면 통계 함수를 직접 사용하여 처리할 수 있음
x_avg[x_avg$V_avg == max(x_avg1$V_avg),]

## Date형 데이터의 활용
# install.packages("lubridate")
library(lubridate)

# avocado 판매 정보를이번에는 연도별 평균 대신 월별 평균으로 요약
(x_avg = avocado %>% group_by(region, year, month(Date), type) %>% summarise(V_avg = mean(Total.Volume),
                                                                             P_avg = mean(AveragePrice)))

## 와인 데이터 활용하기 

# wind.data를 wine 변수에 저장하기 -첫번째 행도 값으로
wine <- read.table("wine.data.txt",header = FALSE, sep = ",")
head(wine)

# wine.name.txt 파일을 wine 데이터의 열 이름으로 저장한다
n = readLines("wine.names.txt") #
n
#문자열 일부를 추출하기 위해 substr 함수 사용
names(wine)[2:14] <- substr(n, 4, nchar(n)) 
names(wine)

# readLines(): 텍스트 파일로부터 각 줄을 문자열로 읽어들여 문자 형식의 벡터 생성
# names(): 데이터 프레임의 변수명을 가져오거나 변경하는 함수
# substr(x, start, stop): 문자형 벡터 x의 start 에서 stop까지 잘라오기 (부분선택)
# nchar(x): 문자형 벡터 x의 구성 요소 개수를 구하는 함수

### 데이터 셋 분할하기

## 데이터 셋을 일정 비율로 분할해보자
# dplyr에서 제공하는 sample_frac나 sample_n 함수를 사용하면 간편

# sample_frac() : 특정 비율만큼 표본을 무작위로 추출하는 함수
train_set = sample_frac(wine, 0.6)
str(train_set)

test_set = setdiff(wine, train_set)
str(test_set)

### 텍스트 인코딩 문제 해결해보기

# "electricity_generation_per_person.csv"의 연도 앞에 붙어 있는 X를 정리해보자
elec_gen = read.csv("electricity_generation_per_person.csv", header = T, sep = ",") %>% show()
names(elec_gen)

names(elec_gen) = substr(names(elec_gen), 2, nchar(names(elec_gen)))
names(elec_gen)

elec_use = read.csv("electricity_use_per_person.csv", header = T, sep = ",") %>% show()
names(elec_use)[2:56]
names(elec_use) <- substr(names(elec_use), 2, nchar(names(elec_use)[2:56]))
names(elec_use)[2:56]

### 데이터 구조 변경

## 각 측정 연도 값이 열의 이름이 되어 한 행에 여러 해의 데이터가 기록됨
## 국가 명, 연도, 전기 생산량과 소비량을 각각 하나의 열에 대응시키는 변형이 필요
## gather 함수 이용

#install.packages("tidyr")
library(tidyr)

elec_gen_df = gather(elec_gen, -ountry, key = "year", value = "ElectricityGeneration")
elec_gen_df = gather(elec_use, -country, key = "year", value = "ElectricityUse")

## 두 개의 데이터 프레임을 병합
## merge() 함수를 이용해 하나의 데이터 프레임으로 병합
## merge()는 두 데이터 프레임을 공통된 값을 기준으로 묶는 함수이다. 데이터베이스에서 join과 같은 역할을 한다.

elex_gen_use = merge(elec_gen_df,elec_df)


#install.packages(c("tidyverse", "data.table"))
library(tidyverse);library(data.table)

getwd()
setwd("/Users/nayoungmin/Desktop/Basic_of_Datascience/7weeks")
# 실습 데이터 다운로드!!
DF <- read.table(file = "Pre_Season_Batter.csv", sep = ",", header = T,
                 fileEncoding = "CP949", encoding = "UTF-8") %>%
  as_tibble()

DF %>% show()
DF %>% str()

DF %>% arrange(year) %>% show() #DF을 연도를 기준으로 오름차순으로 정렬하고 바로 보여줘라
DF %>% arrange(year, desc(team)) %>% show() #DF를 연도를 기준으로 오름차순, 팀을 기준으로 내림차순하고 보여줘라
# year는 ascending(오름차순), team은 descending(내림차순)
DF %>% arrange(team, desc(avg)) %>% show() #DF를 팀은 오름차순, 평균은 내림차순으로 정렬하고 보여줘라
#DF를 SLG,OBP행을 선택하고 SLG는 내림차순으로,OBP도 내림차순으로 정렬하고 마지막 10개만 보여줘라
DF %>% select(SLG,OBP) %>% arrange(desc(SLG), desc(OBP)) %>% tail(10)
# NA는 ascending or descending 상관없이 맨 아래!!!!!
DF %>% arrange_all() %>% show() #DF를 모두 오름차순으로 정렬해라

DF <- read.table(file = "Pre_Season_Batter.csv", sep = ",", header = F) #DF에 "Pre_Season_Batter.csv"를 저장하고 첫번째 행을 값으로 지정해라 그 뒤에 티블로 바꾸자

DF %>% show() #DF를 보여줘라

col_name <- str_split(DF[1,], ",") %>% unlist() #col_name 변수에 첫번째 행을 ,단위로 구분하고 저장해라 그리고 리스트를 해체시켜라
col_name #col_name 부르기
names(DF) #DF의 행 이름 확인하기
names(DF) <- col_name #DF의 행이름을 col_name으로 값을 넣어주기
DF %>% show() #DF를 보여줘라
DF <- DF[2:nrow(DF),] #DF를 2행부터 DF의 행수만큼을 다시 DF에 넣어라
DF %>% show() #DF를 보여줘라

# dplyr
DF <- read.table(file = "Pre_Season_Batter.csv", sep = ",", header = F, fileEncoding = "CP949", encoding = "UTF-8") %>% as_tibble() #"Pre_Season_Batter.csv"를 DF에 저장하고 첫 행을 제목으로 설정하지 말자 그리고 티블 형식으로 바꾸자

DF %>% rename_all(funs(as.character(DF[1,]))) %>% show() #DF에 첫번째 행을 캐릭터 형으로 바꾸고 이름을 전부 바꾸자 그리고 보여줘라
DF <- DF %>% rename_all(funs(as.character(DF[1,]))) %>% .[2:nrow(DF),] #DF에 DF의 첫번째 행을 캐릭터로 바꾸고 다시 이름을 지은다음에 2행부터 DF의 행수만큼의 데이터를 저장하자
 
DF %>% show() #DF를 보여줘라

# mutate를 이용한 data type 변경 일부분
DF %>% mutate(batter_id = as.numeric(batter_id), G = as.numeric(G), AB = as.numeric(AB)) %>% show() #batter_id, G, AB를 numeric형태로 바꾸고 보여줘라

# mutate_at를 이용한 data type 변경 일부분
DF %>% mutate_at(vars(batter_id,G,AB), as.numeric) %>% show() ##batter_id, G, AB를 한번에 numeric형태로 바꾸고 보여줘라

#Sample_frac, sample_n
#arguments
train_set <- sample_n(DF, 975, replace= F) # DF를 975개를 비복원 추출로 뽑는 것을 train_set에 저장해라
train_set %>% show() #train_set을 보여줘라
test_set <- setdiff(DF, train_set) #DF에서 train_set을 뺀것을 test_set에 저장해라 
test_set %>% show() #test_set을 보여줘라 

#Join
#DF에 "Pre_Season_Batter.csv"를 단위로 구분하고, 첫째행을 제목으로 한것을 저장하고 Pre_DF에 저장한뒤 티블로 바꾸자
Pre_DF <- DF <- read.table(file = "Pre_Season_Batter.csv", sep = ",", header = T, fileEncoding = "CP949", encoding = "UTF=8") %>% as_tibble()
#DF에 "Regular_Season_Batter.csv"를 단위로 구분하고, 첫째행을 제목으로 한것을 저장하고 Reg_DF에 저장한뒤 티블로 바꾸자
Reg_DF <- DF <- read.table(file = "Regular_Season_Batter.csv", sep = ",", header = T, fileEncoding = "CP949", encoding = "UTF-8") %>% as_tibble()

Pre_DF %>% show() #Pre_DF를 보여줘라
Reg_DF %>% show() #Reg_DF를 보여줘라

#inner join
#reff에 Pre_DF와 Reg_DF에서 "batter_id", "batter_name", "year"행에서 둘다 공통으로 있는 값들을 추출한 것을 저장하고 위에 값 40개만 보여주자
reff <- inner_join(x = Pre_DF, y = Reg_DF, by = c("batter_id", "batter_name", "year")) %>% head(40) 

# full join
#ref에 Pre_DF와 Reg_DF에서 "batter_id", "batter_name", "year"행에서 둘다 각각있는 값들 모두 추출한 것을 저장하고 위에 값 40개만 보여주자
ref <- full_join(x = Pre_DF, y = Reg_DF, by = c("batter_id", "batter_name", "year")) %>% head(40)

#Pre_DF와 Reg_DF에서 "batter_id", "batter_name", "year"행에서 둘다 공통으로 있는 값들을 추출한 뒤에 
#"batter_id", "batter_name", "year"끼리 묶은뒤에 Pre_G에 G열의 있는 NA값을 없앤 평균과 Reg_G에 G열의 있는 NA값을 없앤 평균을 요약하고 보여줘라
inner_join(x = Pre_DF, y = Reg_DF, by = c("batter_id", "batter_name", "year")) %>% 
  group_by(batter_id, batter_name, year) %>% summarise(Pre_G = mean(G.x, na.rm = T), Reg_G = mean(G.y, na.rm = T)) %>% show()
  


#Pre_DF와 Reg_DF에서 "batter_id", "batter_name", "year"행에서 둘다 각각있는 값들 모두 추출한 뒤에 
#"batter_id", "batter_name", "year"끼리 묶은뒤에 G열의 있는 NA값을 없앤 평균을 요약하고 보여줘라
full_join(x = Pre_DF, y = Reg_DF, by = c("batter_id","batter_name","year")) %>% 
  group_by(batter_id, batter_name, year) %>% summarise(Pre_G = mean(G.x, na.rm = T), Reg_G = mean(G.y, na.rm = T)) %>% show()


