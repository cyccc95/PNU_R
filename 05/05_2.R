#타이타닉 데이터
#https://www.kaggle.com/c/titanic/overview

#데이터불러오기
df <- read.csv("C:/Rwork/05/05_titanic.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 
head(df)

#데이터구조
# PassengerID	승객을 구별하는 고유 ID number	Int
# Survived	승객의 생존 여부를 나타내며 생존은 1, 사망은 0 입니다.	Factor
# Pclass	선실의 등급으로서 1등급(1)부터 3등급(3)까지 3개 범주입니다.	Ord.Factor
# Name	승객의 이름	Factor
# Sex	승객의 성별	Factor
# Age	승객의 나이	Numeric
# SibSp	각 승객과 동반하는 형제 또는 배우자의 수를 설명하는 변수이며 0부터 8까지 존재합니다.	Integer
# Parch	각 승객과 동반하는 부모님 또는 자녀의 수를 설명하는 변수이며 0부터 9까지 존재합니다.	Integer
# Ticket	승객이 탑승한 티켓에 대한 문자열 변수	Factor
# Fare	승객이 지금까지 여행하면서 지불한 금액에 대한 변수	Numeric
# Cabin	각 승객의 선실을 구분하는 변수이며 범주와 결측치가 너무 많습니다.	Factor
# Embarked	승선항, 출항지를 나타내며 C, Q, S 3개 범주이다.	Factor

# 결측치 확인
 
# 결측치 처리
# 평균값으로 대체
 

# 성별에 따른 생존여부
library(ggplot2)

class(df$Survived)
mode(df$Survived)
 

#Pclass	선실의 등급에 따른 생존여부
 

#분류모델
 
#수치데이터로 변환
 

 

#혼돈행렬
 
 
#accuracy
 
