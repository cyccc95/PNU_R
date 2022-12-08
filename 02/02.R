##################################################
# 고속도로 역주행 교통사고와 일반 교통사고 분석
install.packages(("readxl"))
library(readxl)
df = read_excel("02_역주행사고.xlsx")
names(df)
# 전체
df1 = subset(df, df$구분 == "전체")
# 역주행
df2 = df[df$구분 == "역주행", ]
# 일반
df3 = df1
df3$구분 = "일반"
df3[c("사고", "사망")] = df1[c("사고", "사망")] - df2[c("사고", "사망")]
df3
# 치명률
# 전체
df1$치명률 = round(df1$사망 / df1$사고 * 100, 2)
# 역주행
df2$치명률 = round(df2$사망 / df2$사고 * 100, 2)
# 일반
df3$치명률 = round(df3$사망 / df3$사고 * 100, 2)

# 기초통계값
summary(df2)
summary(df3)

mean(df2$치명률)
mean(df3$치명률)

cat("최근 3년간, 역주행 교통사고의 치명률이 ",
    round(mean(df2$치명률),1),
    "%로 일반 교통사고(",
    round(mean(df3$치명률),1),
    "%)보다 ",
    round(round(mean(df2$치명률),1) / round(mean(df3$치명률),1),2),
    "배 높은 것으로 나타났다.")

# 시각화
install.packages("ggplot2")
library(ggplot2)
ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=df) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

#####################################################
# 부산시 체납현황 분석
dfb = read.csv("02_부산광역시_지방세 체납현황.csv", sep = ",", fileEncoding = "euc-kr")
names(dfb)
dfb = dfb[c("과세년도","세목명","체납액구간","누적체납건수","누적체납금액")]
dfb
# 세목명
cols = unique(dfb$세목명)
cols
# 과세년도 범주형
dfb$과세년도 = as.factor(dfb$과세년도)
# 함수 만들기
makedf = function (item) {
  temp = subset(dfb, dfb$세목명 == item)
  ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill=체납액구간), data=temp) +
    geom_bar(stat="identity", position=position_dodge()) +
    ggtitle(item)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}
makedf("지방소득세")

#########################################################
# 기상개황 자료를 분석하여 월별 불쾌지수와 단계
dfw = read.csv("02_기상개황.csv", sep = ",", fileEncoding = "euc-kr")
names(dfw)
dfw = dfw[c("월별.1.","평균기온....","평균상대습도....")]
names(dfw) = c("월별","평균기온","평균상대습도")
dfw$불쾌지수 = round(0.81 * dfw$평균기온 + 0.01 * dfw$평균상대습도 * (0.99 * dfw$평균기온 - 14.3) + 46.3, 1)
dfw$불쾌지수단계 = ifelse(dfw$불쾌지수 >= 80, "매우높음",
                    ifelse(dfw$불쾌지수 >= 75, "높음",
                      ifelse(dfw$불쾌지수 >= 68, "보통", "낮음")))
dfwt = table(dfw$불쾌지수단계)
dfwt = as.data.frame(dfwt)
names(dfwt) = c("불쾌지수단계","빈도수")
dfwt
ggplot(mapping =aes(x=불쾌지수단계, y=빈도수, fill=불쾌지수단계), data=dfwt) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("불쾌지수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
