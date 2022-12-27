setwd('c:/Rwork/01')

dfbmi <- read.csv('01_국민건강보험공단500.csv', header = T,  stringsAsFactors = F,fileEncoding = 'euc-kr')
dfbmi <- dfbmi[c("신장",   "체중")]
names(dfbmi)
dfbmi$BMI <- round( dfbmi$체중 / (dfbmi$신장 / 100) ** 2, 2)

dfbmi$비만도 <- ifelse(dfbmi$BMI<20, "저체중", 
                    ifelse(dfbmi$BMI < 25, "정상",
                           ifelse(dfbmi$BMI < 30, "과체중", "비만"
                           )))

summary(dfbmi)
dfbmi$비만도 <- as.factor(dfbmi$비만도)
x <- sample(1:nrow(dfbmi), 0.7*nrow(dfbmi))
train <- dfbmi[x, ]
test <- dfbmi[-x, ]

library(party)
model <- ctree(비만도 ~ 신장 + 체중, data = train)
pred <- predict(model, test)
t <- table(test$비만도, pred)
rate <- (t[1,1] + t[2,2] + t[3,3] + t[4,4]) / sum(t)
# 분류분석도 그냥 accuracy 불러서 뽑을 수 있다.
rate2 = accuracy(test$비만도, pred)
# =========================================================================

model2 <- lm(BMI ~ 신장 + 체중, data = train)
model2 <- randomForest(BMI ~ 신장 + 체중, data = train)
# y = -0.296x1 + 0.3758x2 + 48.1631
pred2 <- predict(model2,test)
rmse <- sqrt(mean(test$BMI - pred2)^2)
