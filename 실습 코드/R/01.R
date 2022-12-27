# 해결문제 
# BMI는 몸무게와 키를 이용하여 체지방율을 측정하는 지수이다. 
# 자신의 몸무게와 키를 각각 변수 weight와 height에 저장하고 BMI지수를 계산해 본다. 
# 단, 키는 cm로 입력 받아서 처리한다.
# 
# BMI = 체중(kg) / (키(m) x키(m))


# 현재 작업폴더 확인
getwd()

# 키와 몸무게 scala입력
height <- scan()
weight <- readline("몸무게 입력")

# 몸무게 수치 변환
weight <- as.numeric(weight)

# BMI 계산
BMI <- weight / (height /100) **2
BMI

# 키와 몸무게 vector입력
height <- scan()
weight <- c(60, 70, 80)
BMI <- weight / (height /100) **2
BMI


#문자열 입력
weight <- readline("몸무게 입력")

# stringr 패키지 설치
install.packages("stringr")
library(stringr)

# 문자열 분리
weight <- str_split(weight, " ")

# 자료형 확인
mode(weight)
class(weight)

# 벡터로 형변환
weight <- unlist(weight)
mode(weight) 

# 벡터 확인
is.vector(weight)

# 숫자벡터로 변경
weight <- as.numeric(weight)
weight

BMI <- weight / (height /100) **2
BMI


# 데이터프레임 입력
df <- data.frame()
df <- edit(df)
df

df$var3 <- df$var1 / (df$var2 /100) **2

# 데이터프레임 열명 변경
names(df) <- c("몸무게", "키", "BMI")
df

# 해결문제 
# 국민건강보험공단 자료를 이용하여 BMI와 비만도를 구하시오.
# 비만도 
# 저체중	20 미만
# 정상	20 - 24
# 과체중	25 - 29
# 비만	30 이상


dfbmi <- read.csv("./data/01_국민건강보험공단500.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr')

dfbmi$BMI <- dfbmi$체중 / (dfbmi$신장 / 100) ** 2
dfbmi$비만도 <- ifelse(dfbmi$BMI < 20, "저체중", 
                ifelse(dfbmi$BMI < 25, "정상",
                ifelse(dfbmi$BMI < 30, "과체중","비만")))


# 빈도 테이블
table(dfbmi$성별)
table(dfbmi$성별, dfbmi$비만도)

# 빈도 테이블 저장
write.csv(table(dfbmi$성별, dfbmi$비만도), 
          "./data/01_성별비만도.csv", 
            fileEncoding = 'euc-kr',
            quote = F)

# 해결문제
# 국민건강보험 관리공단의 건강검진 자료를 이용하여 대사증후군을 판별하시오.
# 높은 혈압(130/85mmHg 이상)
# 높은 혈당(공복 혈당 100mg/dL 이상)
# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군


df2 <- read.csv("./data/01_국민건강보험공단_건강검진정보_20211229.CSV", 
                header = T, 
                stringsAsFactors = F, 
                fileEncoding = 'euc-kr')

head(df2)

names(df2)

df2 <- df2[c("성별코드", 
             "수축기.혈압", "이완기.혈압", 
             "식전혈당.공복혈당.",
             "트리글리세라이드", 
             "HDL.콜레스테롤",
             "허리둘레"
             )]
head(df2)
names(df2) <- c("성별코드",
                "수축기혈압", "이완기혈압", 
                "공복혈당",
                "트리글리세라이드", 
                "HDL콜레스테롤",
                "허리둘레"
                )

df2 <- na.omit(df2)
df2$높은혈압 <- ifelse ((df2$수축기혈압 >=130 | df2$이완기혈압 >=85), 1, 0)
df2$높은혈당 <- ifelse ((df2$공복혈당 >= 100), 1, 0)
df2$중성지방 <- ifelse ((df2$트리글리세라이드 >= 150), 1, 0)
df2$콜레스테롤 <- ifelse ((df2$성별코드 ==1 & df2$HDL콜레스테롤 < 40)|
                       (df2$성별코드 ==2 & df2$HDL콜레스테롤 < 50), 1, 0)
df2$복부비만 <- ifelse ((df2$성별코드 ==1 & df2$허리둘레 >=90 )|
                      (df2$성별코드 ==2 & df2$허리둘레 >=85), 1, 0)

df2$대사증후군 <- df2$높은혈압 + df2$높은혈당 + df2$중성지방 + df2$콜레스테롤 + df2$복부비만
df2$판별 <- ifelse(df2$대사증후군>=3, "위험군", ifelse(df2$대사증후군>=1, "주의군", "정상"))

df2$성별 <- ifelse(df2$성별코드 == 1, "남", "여")
table(df2$성별, df2$판별)
