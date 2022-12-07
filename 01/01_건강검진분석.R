# 해결문제 
# BMI는 몸무게와 키를 이용하여 체지방율을 측정하는 지수이다. 
# 자신의 몸무게와 키를 각각 변수 weight와 height에 저장하고 BMI지수를 계산해 본다. 
# 단, 키는 cm로 입력 받아서 처리한다.
# 
# BMI = 체중(kg) / (키(m) x키(m))

# 키와 몸무게 scala입력, BMI 계산
## 하나 입력 받고 enter치면 하나만 입력받을 수 있음
height <- scan()
weight <- scan()
BMI = weight / (height / 100) ** 2 

print("키와 몸무게 입력")
data <- scan()
BMI = data[2] / (data[1] / 100) ** 2

# 키와 몸무게 vector입력
#문자열 입력
data <- readline()
mode(data) # 데이터 타입 확인
# stringr 패키지 설치
# 문자열 분리
data <- str_split(data, " ")
# 자료형 확인
mode(data)
# 벡터로 형변환
data <- unlist(data) # list -> vector
mode(data)
# 벡터 확인
is.vector(data)
# 숫자벡터로 변경
data <- as.numeric(data)
height <- data[1]
weight <- data[2]
BMI = weight / (height / 100) ** 2


# 데이터프레임 입력
df <- data.frame()
mode(df)
class(df)

df <- edit(df)
df
# 데이터프레임 열명 변경
names(df) <- c("키", "몸무게")
df$BMI <- df$몸무게 / (df$키 / 100) ** 2
df

# 해결문제 
# 국민건강보험공단 자료를 이용하여 BMI와 비만도를 구하시오.
# 비만도 
# 저체중	20 미만
# 정상	20 - 24
# 과체중	25 - 29
# 비만	30 이상
 
# 빈도 테이블
dfbmi <- read.csv("01_국민건강보험공단500.csv", sep = ",", fileEncoding = "euc-kr")
dfbmi
dfbmi$BMI <- dfbmi$체중 / (dfbmi$신장 / 100) ** 2
dfbmi$비만도 <- ifelse(dfbmi$BMI < 20, "저체중",
                    ifelse(dfbmi$BMI < 25, "정상",
                    ifelse(dfbmi$BMI < 30, "과체중", "비만"
                )))
dfbmi
# 빈도 테이블 저장
table(dfbmi$성별)
table(dfbmi$비만도, dfbmi$성별)
write.csv(dfbmi, "dfbmi.csv", row.names = F, quote = F, fileEncoding = "euc-kr")


# 해결문제
# 국민건강보험 관리공단의 건강검진 자료를 이용하여 대사증후군을 판별하시오.
# 높은 혈압(130/85mmHg 이상)
# 높은 혈당(공복 혈당 100mg/dL 이상)
# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군
dfcheck <- read.csv("국민건강보험공단_건강검진정보_20211229.csv",
                    sep = ",", fileEncoding = "euc-kr")
# 열명확인
names(dfcheck)
df2 <- dfcheck[c("성별코드","허리둘레","수축기.혈압","이완기.혈압","식전혈당.공복혈당.",
                 "트리글리세라이드","HDL.콜레스테롤")]
head(df2)
# NA 값 제거
df2 <- na.omit(df2)
df2$높은혈압 <- ifelse(df2$수축기.혈압 >= 130 & df2$이완기.혈압 >= 85, 1, 0)
df2$높은혈당 <- ifelse(df2$식전혈당.공복혈당. >= 100, 1, 0)
df2$높은중성지방 <- ifelse(df2$트리글리세라이드 >= 150, 1, 0)
df2$낮은HDL수치 <- ifelse((df2$성별코드 == 1 & df2$HDL.콜레스테롤 < 40)
                          | (df2$성별코드 == 2 & df2$HDL.콜레스테롤 < 50), 1, 0) 
df2$복부비만 <- ifelse((df2$성별코드 == 1 & df2$허리둘레 >= 90)
                       | (df2$성별코드 == 2 & df2$허리둘레 >= 85), 1, 0)
df2$대사증후군 <- ifelse(df2$높은혈압 + df2$높은혈당 + df2$높은중성지방 + df2$낮은HDL수치 + df2$복부비만 < 1, "정상",
                    ifelse(df2$높은혈압 + df2$높은혈당 + df2$높은중성지방 + df2$낮은HDL수치 + df2$복부비만 < 3, "주의군", "위험군"
                ))
# 열명변경
df2 <- df2[c("성별코드","허리둘레","수축기.혈압","이완기.혈압","식전혈당.공복혈당.",
             "트리글리세라이드","HDL.콜레스테롤","대사증후군")]
write.csv(df2, "df2.csv", row.names = F, quote = F, fileEncoding = "euc-kr")
