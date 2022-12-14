#암종류별 성별 분석
library(dplyr)

#데이터 불러오기(암발생자수)
df1 <- read.csv("C:/Rwork/03/03_암발생자수.csv")
df1

# 열명 변경
# "암종별", "성별", "연령별", "발생자수", "조발생률"
names(df1)
names(df1) <- c("암종별", "성별", "연령별", "발생자수", "조발생률")
df1

# 데이터셋 조회
# 1) 특정 변수 조회
df1['암종별'] 
t1 <- df1$암종별 # vector
mode(t1)
class(t1)

# 2) 특정 열명을 사용하여 조회
t2 <- df1['암종별'] # dataframe
mode(t2)
class(t2)

# 3) 특정 행 조회 :1행 조회
df1[1,]

# 2, 4행 조회
df1[c(2, 4),]

# 4)특정행 제거 : 1행제거
df2 <- df1[-1, ] 
head(df2)

# 암종류 확인
unique(df2$암종별) # 중복제거

# 모든암 제거하고 연령별이 계인 데이터 
df3 <- df2 %>%
  filter(암종별 != "모든 암(C00-C96)") %>%
  filter(연령별 == "계")
unique(df3$암종별)
df3

# 5) 특정행 열 조회
df3[1,'암종별'] 
df3[1:3, '암종별'] 
df3[1:3, c('암종별','발생자수')] 

# 열 데이터 타입 확인
str(df3)

# 값 변경 : - => 0
df3$발생자수 <- ifelse(df3$발생자수 == '-', 0, df3$발생자수)
df3$조발생률 <- ifelse(df3$조발생률 == '-', 0, df3$조발생률)

# 열 데이터타입 변경
df3$발생자수 <- as.numeric(df3$발생자수)
df3$조발생률 <- as.numeric(df3$조발생률)
str(df3)

# 성별 계
df31 <- df3 %>%
  filter(성별 == "계")
df31

# 성별 남여
df32 <- df3 %>%
  filter(성별 != "계")
df32

df31 <- df31[, c('암종별', "발생자수")]
df31

df32 <- df32[, c('암종별', '성별', "발생자수")]
df32

# 그래프 
library(ggplot2)
help(ggplot)
ggplot(mapping =aes(x=암종별, y=발생자수, fill=발생자수), data=df31) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("암종별 발생자수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'), axis.text.x=element_text(angle=90, hjust=1))

ggplot(mapping =aes(x=암종별, y=발생자수, fill=성별), data=df32) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("암종별 성별 발생자수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'), axis.text.x=element_text(angle=90, hjust=1))
