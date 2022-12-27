getwd()
setwd('c:/Rwork/06')
df2 <- read.csv(file = '06_국민건강보험공단500.csv', header = T, fileEncoding = 'cp949')

df2 <- df2[, c("시도코드", "성별코드", "수축기.혈압", "이완기.혈압", "식전혈당.공복혈당.", "트리글리세라이드", "HDL.콜레스테롤", "허리둘레")]

names(df2) <- c("시도코드", "성별코드", "수축기혈압", "이완기혈압", "공복혈당", "트리글리세라이드", "HDL콜레스테롤", "허리둘레")

summary(df2)

df2 <- na.omit(df2)

df2$높은혈압 <- ((df2$수축기혈압>=130) | (df2$이완기혈압 >= 85))
head(df2)
# 높은 혈당(공복 혈당 100mg/dL 이상)
df2$높은혈당 <- df2$공복혈당>=100
# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
df2$높은중성지방 <- df2$트리글리세라이드>=150
# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
df2$낮은콜레스테롤수치 <- ((df2$성별코드==1) & (df2$HDL콜레스테롤<40) | (df2$성별코드==2) & (df2$HDL콜레스테롤<50))
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
df2$복부비만 <- ((df2$성별코드==1) & (df2$허리둘레>=90) | (df2$성별코드==2) & (df2$허리둘레>=85))
# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군
df2$대사증후군수 <- df2$높은혈압 + 
  df2$높은혈당 + 
  df2$높은중성지방 + 
  df2$낮은콜레스테롤수치 + 
  df2$복부비만

df2$판별 <- ifelse(df2$대사증후군수==0, "정상", 
                 ifelse(df2$대사증후군수<=2, "주의군", "위험군"))

# =====================================================
df3  <- df2[, c("대사증후군수", "판별" )]
x <- sample(1:nrow(df3), 0.7* nrow(df3))
train <- df3[x, ]
test <- df3[-x, ]
df3$판별 <- as.factor(df3$판별)
df3$대사증후군수 <- as.factor(df3$대사증후군수)

model <- ctree(판별 ~ 대사증후군수, data = train)
model
pred1 <- predict(model, test)
pred1

t <- table(test$판별, pred1)
# 100%
rate = (t[1,1] + t[2,2] + t[3,3]) / sum(t)

# ============================================================
# Boolean으로 맞추게 하면
x1 <- sample(1:nrow(df2), 0.7* nrow(df2))
train1 <- df2[x1, ]
test1 <- df2[-x1, ]

model1 <- ctree(판별 ~ 높은혈당 + 높은중성지방 + 낮은콜레스테롤수치 + 복부비만, data = train1)
model1
pred2 <- predict(model1, test1)
pred2

t1 <- table(test1$판별, pred2)
t1
# 이렇게 계산을 시키면 75% 정도.
rate1 = (t1[1,1] + t1[2,2] + t1[3,3]) / sum(t1)


# ==========================================================================
# 그냥 수치값으로
df4 <- df2

summary(df4)
df4$성별코드 <- as.factor(df4$성별코드)
df4$판별 <- as.factor(df4$판별)
x2 <- sample(1:nrow(df4), 0.7* nrow(df4))
train2 <- df4[x2, ]
test2 <- df4[-x2, ]

# 72%
model2 <- ctree(판별 ~ 성별코드 + 수축기혈압 + 이완기혈압 + 공복혈당 + 트리글리세라이드 + HDL콜레스테롤, data = train2)
# 평균혈압을 넣으면 좀 오른다.
# 79%
# 사실 그것보다 어떻게 샘플이 뽑히냐에 따라 더 많이 흔들리는 것같다.
model2 <- ctree(판별 ~ 성별코드 + 평균혈압 + 공복혈당 + 트리글리세라이드+ HDL콜레스테롤, data = train2)
model2
# 니가 이 모델을 테스트하려면 그 열에 위에 써둔 x값들이 있어야 될 거다. y는 없어도 되고
pred3 <- predict(model2, test2)
pred3

t2 <- table(test2$판별, pred3)
t2
# 79%
rate2 = (t2[1,1] + t2[2,2] + t2[3,3]) / sum(t2)

df4$평균혈압 <- (df4$수축기혈압 + df4$이완기혈압) / 2
