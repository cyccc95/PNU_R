
#R 내장 데이터 가져오기
data(iris)

#iris 데이터 확인
str(iris)

#iris : 꽃받침, 꽃잎 데이터 추출
iris1 <- iris[c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
# iris1 <- iris[, -5]
iris1

#기술통계량
summary(iris1)

#상관계수
cor(iris1, method="pearson")

#색의 농도로 상관계수 
install.packages("corrgram")
library(corrgram)

corrgram(iris1, upper.panel = panel.conf)

#상관계수 챠트
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(iris1)

#학습데이터와 테스트데이터 분리
x <- sample(1:nrow(iris1), 0.7 * nrow(iris1))
x

train <- iris[x, ]
test <- iris[-x, ]

nrow(train)
nrow(test)
#회귀모델 : 꽃받침 길이 예측 
names(iris1)

#학습
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train)
model
 
#예측
pred <- predict(model, test)
pred

#평가
#RMSE : sqrt((실제 - 예측)^2의 평균)

RMSE <-  
RMSE


#분류모델 
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width))  + 
  geom_point(aes(colour = Species))

ggplot(iris, aes(Petal.Length , Petal.Width))  + 
  geom_point(aes(colour = Species))

#학습데이터와 테스트데이터 나누기
 

#모델 학습
#트리 모델 
install.packages("party")
library(party)
names(iris)

model <- ctree(Species ~ Sepal.Length + Sepal.Width + 
                 Petal.Length + Petal.Width,
               data = train)

plot(model)

#예측 
 

#혼돈행렬
 


#iris data 저장
 
