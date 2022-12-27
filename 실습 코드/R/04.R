#인구 동향
library(dplyr)

#데이터 불러오기
df <- read.csv("./data/04_인구동향.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr') 

df 

# 열명 확인
names(df)
names(df) = c("행정구역","시점","출생아수","사망자수","혼인건수","이혼건수")


#EDA
#결측치 확인
summary(df)

#데이터 구조 확인
str(df)


#결측치 내용 확인 
df %>%
  filter(is.na(출생아수))

#결측치 행 제거
df1 <- na.omit(df)
df1

#결측치 확인
summary(df1)

#결측치 값 대체
names(df)

# "출생아수" "사망자수" "혼인건수" "이혼건수"에 대해 
# na 값 0으로 변경 : 반복문
col <- names(df)[3:6]
col

for (c in col) {
  temp <- df[c]
  temp <- temp[,1]
  temp <- ifelse(is.na(temp), 0, temp)
  df[c]<- temp
}

summary(df)

# 자연증가수열 추가
df$자연증가수 <- df$출생아수- df$사망자수

#범주형 변경 : 시점 int -> category
df$시점 <- as.factor(df$시점)
class(df$시점)
mode(df$시점)
is.factor(df$시점)

#기술통계분석-범주형자료-빈도분석
summary(df$행정구역)
summary(df$시점)
table(df$행정구역, df$시점) 
unique(df$행정구역)

#기술통계분석-범주형자료-빈도분석-barplot 그래프 
barplot(table(df$행정구역))


#기술통계분석-연속형자료-요약 통계량
summary(df$자연증가수)

#기술통계분석-연속형자료-산점도 그래프
plot(df$출생아수, col="blue", type="o", xlab = '', ylab = '')
par(new=T)
plot(df$혼인건수, col="green", type="o", xlab = '', ylab = '', axes=F)
legend("topright",legend=c("출생아수", "혼인건수"),
       col=c("blue", "green"), lty=1:2, cex=0.8)


#자료 나누기 
dft <- df %>%
  filter(행정구역 == "전국")

dfa <- df %>%
  filter(행정구역 != "전국")

dfg <- dfa %>%
  group_by(시점) %>%
  summarise(출생평균 = mean(출생아수), 
            사망평균 = mean(사망자수),
            자연평균 = mean(자연증가수))


#기술통계분석-연속형자료-산점도 그래프
plot(dft$출생아수, col="blue", type="o", xlab = '', ylab = '')
par(new=T)
plot(dft$혼인건수, col="green", type="o", xlab = '', ylab = '', axes=F)
legend("topright",legend=c("출생아수", "혼인건수"),
       col=c("blue", "green"), lty=1:2, cex=0.8)
axis(side=1,at=c(1:length(dft$혼인건수)),labels=dft$시점)

class(dft$시점)

x = dft[,'시점']
y = dft[,c('자연증가수', '혼인건수', '이혼건수')]
matplot(x, 
        y,
        pch=1,
        type='o'
        )


plot(dft$출생아수, dft$혼인건수)

library(ggplot2)
ggplot(dfa, aes(x=시점, y=자연증가수, group=행정구역, color=행정구역)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x=element_text(angle=90, hjust=1))
 