#################################################################
####################   데이터과학입문 멘토링  ###################
###############      모든 데이터가 모이면..?     ################
#################################################################

# 데이터 프레임 만들기
name <- c("신비","형준","영민") # 문자형 데이터의 벡터
age <- c(21,23,25) # 숫자형 데이터의 벡터
gender <- factor(c("F","M","M")) # 범주형 데이터의 벡터
blood.type <- factor(c("B","A","O")) # 범주형 데이터의 벡터
mentors <- data.frame(name,age,gender,blood.type) 
mentors

# 데이터프레임 인덱싱
# 1. 리스트 방식 접근
# 리스트 연산자를 사용해 데이터프레임에서 열을 추출하는 방법
# 데이터프레임은 리스트이므로 구성요소의 이름 또는 인덱스를 통해 접근할 수 있다.
# df[i]. df[[i]], df$name 와 같은 방식으로 사용

# 2. 행렬 방식 접근
# 행렬과 비슷한 표기법을 사용하는 방법
# 데이터 프레임은 행렬과 같은 성격을 갖고 있기 때문에 행렬처럼 접근 할 수 있다. 
# df[i, j], df[i, ], df[, j] 처럼 사용


# 리스트 방식 접근
mentors[2] # 2 열 추출(데이터프레임으로)
mentors[[2]] #2열 추출 (벡터로)
mentors$age # 2열 추출(벡터로)

class(mentors[2]) #무슨형? data.frame
class(mentors[[2]]) #무슨형? numeric
class(mentors$age) #무슨형? numeric




# 행렬 방식 접근
mentors[c(1:3),]  # 1-3행 추출
mentors[c(1:3),2]  # 1~3행, 2열 추출
mentors[c(1:3),2, drop = F]  # 차원축소 방지
mentors[c(1:3),2,drop=T]  # 차원축소 

class(mentors[c(1:3),2]) #1~3행, 2열은 무슨데이터 형일까?
class(mentors[c(1:3),2,drop=F]) #차원축소 방지를 한 df의 1~3행, 2열은 무슨데이터 형일까? 

# 결측치(NA)란
# 변수는 데이터를 담는 상자에 비유할 수 있다.
# 다음의 두 경우를 생각해보자.

# 1.상자에 데이터가 담겨 있는데 상자가 포장되어 있어서 안에 뭐가 들었는지 모르는 경우
# 2.상자는 있는데 아무 것도 안들어 있는 빈 상자의 경우

# 우리는 1번을 NA, 2번을 NULL이라고 부른다.
# NA는 존재하지만 불확실한 값이고 NULL은 존재하지 않으며 비어있는 값

# 결측치(NA) 제거하기
# 경우에 따라 NA가 있는 관측치는 제거해야 할 필요가 존재함  - NA가 포함된 행 전체

# 결측치를 제거하는 방법에는 두 가지가 있습니다.

# (1) na.omit( )를 사용하는 방법 - NA가 있는 행을 제외하는 함수

# (2) complete.cases( )를 사용하는 방법 -NA가 없는 행을 TURE로 반환

df <- data.frame(x=c(1,2,3,NA,7,8), y=c(5,4,2,2,NA,9))
df

# 첫 번째 방법 na.omit 활용
clean <- na.omit(df)
clean

# 두 번째 방법 complete.cases 사용
clean2 <- df[complete.cases(df),]
clean2

# subset
View(iris)
str(iris)
subset(iris, iris$Petal.Length >= 3) #petal.length가 3이상인 값 선택
subset(iris, iris$Petal.Width <= 3) #petal.width가 3이하인 값 선택
subset(iris, iris$Sepal.Length == 4.9) #sepal.length가 4.9인 값 선택
subset(iris, iris$Species == "setosa") #종이 setosa인 것 선택
subset(iris, iris$Species != "setosa") #종이 setosa가 아닌 것 선택

# 리스트 ----
list_value <- list(iris, cars, airquality) #iris, cars, airquality를 list_value에 list로 저장
str(list_value) #list_value의 구조보기

str(list_value[1]) # list_value의 첫번째 원소보기 (list로 반환)
str(list_value[2]) #list_value의 두번째 원소보기 (list로 반환)

str(list_value[[1]]) #list_value의 첫번째 구조 (data.frame으로 반환)
str(list_value[[2]]) #list_value의 두번째 구조 (data.frame으로 반환)

# 리스트에 요소 추가 
list_value[[4]] <- "Hello" #list_value의 Hello를 4번째 구조에 추가
str(list_value) #<- 살펴보기

# 리스트에 요소 제거 
list_value[[4]] <- NULL #네번째 구조 지우기, 


list_value[[3]] <- NULL #세번째 구조 지우기
str(list_value) #구조 보기

# lapply
str(list_value[[1]]) #첫번째 원소보기 (data.frame으로)
lapply(list_value[[1]], mean) #첫번째 값의 평균 구하기
lapply(list_value[[2]], mean) #두번째 값의 평균 구하기
