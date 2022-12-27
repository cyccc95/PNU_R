# 치매환자현황분석 
# 거주지역별 치매환자 빈도표를 구하고 그래프 작성
# 기준일자와 진단일자를 이용하여 진단일수를 계산하고 평균 진단일수 산출
# 연령대별 빈도수 그래프를 그려서 치매환자가 많은 연령대 분석

df1 <- read.csv("./data/03_치매환자현황.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 


# 거주지역별 치매환자 빈도표
거주지역 <- table(df1$거주지역)
거주지역

# 거주지역별 치매환자 그래프
library(ggplot2)
qplot(거주지역, data=df1, fill=거주지역)+
  ggtitle("거주지역별 치매환자")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))


# 데이터 타입확인
class(df1$진단일자)

# 날짜 데이터로 변경 
df1$진단일자 <- as.Date(df1$진단일자)
class(df1$진단일자)

df1$데이터기준일자 <- as.Date(df1$데이터기준일자)
class(df1$데이터기준일자)

# 날짜 함수 
df1$진단일수 <- difftime(df1$데이터기준일자, df1$진단일자)
df1
class(df1$진단일수)
df1$진단일수 <- as.numeric(df1$진단일수)

# 평균진단일수
mean(df1$진단일수)

# 엑셀파일저장
install.packages("writexl")
library(writexl)

write_xlsx(df1, "./data/치매.xlsx")

# 연령대별 빈도수
install.packages("lubridate")
library(lubridate)
df1$연령 <- (year(df1$데이터기준일자) - df1$출생년도) %/% 10 * 10

#100대 찾기
df1$연령 <- ifelse(df1$연령 == 100, 90, df1$연령)
df1[df1$연령 == 100,]

df1$연령 <- paste(df1$연령, "대", sep="")
df1

table(df1$연령)

# 연령대별 그래프
qplot(연령, data=df1, fill=연령)+
  ggtitle("연령대별 치매환자")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))


# 암발생자수
install.packages("dplyr")
library(dplyr)

df2 <- read.csv("./data/03_암발생자수_.csv", 
                header = T, 
                stringsAsFactors = F, 
                fileEncoding = 'euc-kr') 

names(df2)

# 모든 암의 연령별 분석
# df21 <- subset(df2, df2$X24개.암종별 == "모든 암(C00-C96)" & df2$성별 == "계")

df21 <- df2 %>% 
        filter(df2$X24개.암종별 == "모든 암(C00-C96)")
df21


# 행인덱스 초기화
rownames(df21) <- NULL

# df21 <- df21[2:19, ]
df21 <- df21 %>% 
        filter(!(연령별 %in% c("계", "연령미상")))
df21

# 연령대 나누기
df21$연령대 <- ifelse(df21$연령별 %in% c("0-4세", "5-9세", "10-14세", "15-19세",
                          "20-24세","25-29세","30-34세","35-39세"), "30대이하",
                  ifelse(df21$연령별 %in% c("40-44세","45-49세","50-54세","55-59세")
                         ,"40~50대",
                   ifelse(df21$연령별 %in% c("60-64세", "65-69세", "70-74세","75-79세"),
                          "60~70대", "80대이상")))

# 그룹
names(df21)
df21$X2019 <- as.numeric(df21$X2019)

df21g <- df21 %>% group_by(연령대, 성별) %>% summarise(계 = sum(X2019))
df21g

ggplot(mapping =aes(x=연령대, y=계, fill=성별), data=df21g) +
  geom_bar(stat="identity", position=position_dodge())  +
  ggtitle('연령대별 성별 분석')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))



df2 <- df2[-1, ]
unique(df2$X24개.암종별)

df22 <- df2 %>% filter(연령별 == "계")
df22

df22$X2019 <- gsub("-", 0, df22$X2019)
df22$X2019 <- as.numeric(df22$X2019)

df22g <- df22 %>% group_by(X24개.암종별, 성별) %>% summarise(계 = sum(X2019))
df22g <- df22g %>% filter(X24개.암종별 != "모든 암(C00-C96)")

ggplot(mapping =aes(x=X24개.암종별, y=계, fill=성별), data=df22g) +
  geom_bar(stat="identity", position=position_dodge())  +
  ggtitle('성별 분석')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold')) +
  theme(axis.text.x=element_text(angle=90, hjust=1))
