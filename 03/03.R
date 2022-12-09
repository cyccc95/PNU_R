##########################################
# 치매환자현황분석
df = read.csv("03_치매환자현황.csv", sep = ",", fileEncoding = "euc-kr")

# 거주지역별 치매환자 빈도표, 그래프
library(ggplot2)
qplot(거주지역, data = df, fill = 거주지역)+
  ggtitle("거주지역별 치매환자")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# 기준일자와 진단일자를 이용하여 진단일수를 계산하고 평균 진단일수 산출
# 날짜형으로 변환
df$진단일자 <- as.Date(df$진단일자)
df$데이터기준일자 <- as.Date(df$데이터기준일자)
# 날짜사이 간격
df$진단일수 = difftime(df$데이터기준일자, df$진단일자, units = 'days')
df$진단일수 = as.numeric(df$진단일수)
# 평균 진단 일수
mean(df$진단일수)

# 연령대별 빈도수 그래프를 그려서 치매환자가 많은 연령대 분석
install.packages("lubridate")
library(lubridate)
df$연령대 <- ifelse((year(df$데이터기준일자) - df$출생년도 + 1) >= 90, "90대",
                 ifelse(year(df$데이터기준일자) - df$출생년도 + 1 >= 80, "80대",
                  ifelse(year(df$데이터기준일자) - df$출생년도 + 1 >= 70, "70대",
                  ifelse(year(df$데이터기준일자) - df$출생년도 + 1 >= 60, "60대",
                  ifelse(year(df$데이터기준일자) - df$출생년도 + 1 >= 50, "50대", "40대")))))
qplot(연령대, data = df, fill = 연령대)+
  ggtitle("연령대별 치매환자")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

#########################################################
# 연령대별 성별 암발생자 현황분석
install.packages("dplyr")
library(dplyr)
dfc = read.csv("03_암발생자수.csv", fileEncoding = "euc-kr")
dfc
names(dfc)
str(dfc)
dfc = dfc %>% filter(X24개.암종별 == "모든 암(C00-C96)", !(연령별 %in% c("연령미상","계")))
unique(dfc$연령별)
unique(dfc$연령대)
dfc$연령대 = ifelse(dfc$연령별 %in% dfc$연령별[1:8], "30대이하",
                 ifelse(dfc$연령별 %in% dfc$연령별[9:12], "40~50대",
                  ifelse(dfc$연령별 %in% dfc$연령별[13:16], "60~70대", "80대이상")))
names(dfc)
dfc$X2019 <- as.numeric(dfc$X2019)

dfc2 <- dfc %>% group_by(연령대, 성별)%>% summarise(계 = sum(X2019))
dfc2
names(dfc)
qplot(연령대, data = dfc2, fill = 성별)+
  ggtitle("연령대별 암발생자 성별 분석")+
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

ggplot(mapping =aes(x=연령대, y=계, fill=성별), data=dfc2) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("연령대별 암발생자 성별 분석")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
