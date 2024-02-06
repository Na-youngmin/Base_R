## 조건문

#벡터인 경우
test = c(15, 20, 30, NA, 45)
test[test < 40] #값이 40 미만인 요소 추출
test[test%%3 != 0] #값이 3으로 나누어 떨어지지 않는 요소 추출
test[is.na(test)] #NA인 요소 추출
test[!is.na(test)] #NA가 아닌 요소 추출
test[(test %% 2 ==0) & (!is.na(test))] #2의 배수이면서 NA가 아닌 요소 추출

#데이터 프레임의 경우
students = data.frame(name = c("세현", "형준", "민재","신비","연아","주광","유진","효준","진호","준혁"),
                        age = c(30, 16, 21, 34, -30, 17, 42, 62, 12, 22), 
                        gender = factor(c("M","M","M","F","F","M","F","M","M","M")))
students

#성별이 여성인 행 추출
students[students$gender == "F",]

#30살 미만의 남성행 추출
students[(students$age < 30) & (students$gender == "M"),]


#if문 사용
#두 가지 조건 분기가 필요한 경우
x = 5
if(x %% 2 ==0){
  print("짝수")
} else {
  print("홀수")
}

#세 가지 조건 분기가 필요한 경우
x = -1
if(x>0){
  print("양수")
} else if(x<0){
  print("음수")
} else {
  print(0)
}

#ifelse문 사용
#if/else 문을 합쳐놓은 형태
x = c(-5:5)
options(digits = 3)
sqrt(x)

sqrt(ifelse(x>0, x, NA)) #NaN이 발생하지 않게 음수면 NA로 표시

##반복문

#while 문 - while문을 이용해 1부터 10까지 수 증가시키기
i = 1
while(i <= 200000000){
  print(i)
  i = i + 1
}


#구구단 2단 만들기
i = 1
while(i <= 9){
  print(paste(2, "X", i,"= ", 2 * i))
  i = i + 1
}

#for문 - for문을 이용해 1부터 10까지 수 증가시키기
for(i in 1:10){
  print(i)
}

# 구구단 2단 만들기
for (i in 1:9){
  print(paste(2,'X',i, '=', 2*i))
  i = i + 1
}

#구구단 2~9단 만들기
for(i in 2:9){
  for(j in 1:9){
    print(paste(i," x ",j," = ",i*j))
  }
}

#조건문과 반복문을 활용하여 특정 범위 내에서 조건에 맞는 값 찾기
# 1부터 10까지 짝수만 출력
for(i in 1:10){
  if(i %%2 == 0){
    print(i)
  }
}


#user_define_function - 계승함수
fact = function(x){
  fa = 1
  while(x > 1){
    fa = fa * x
    x = x - 1
  }
  return(fa)
}
fact(6)


### 키보드 입력 변수??
name <- readline(prompt = "이름을 입력하세요: ")
cat("안녕하세요, ", name, "님!", sep="")


