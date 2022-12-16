#인구 동향
library(dplyr)
setwd("C:/Rwork/04")
#데이터 불러오기(인구동향)
df1 = read.csv("C:/Rwork/04/04_인구동향.csv")
head(df1)

# 열명 확인
names(df1)

# "행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수"
names(df1) <- c("행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수")
names(df1)
head(df1)
 
#결측치 확인
summary(df1)

#결측치 내용 확인 
is.na(df1$출생아수)
df2 <- df1 %>% filter(is.na(df1$출생아수))
df2 
unique(df2$행정구역별)
unique(df2$시점)

#결측치 행 제거
 
#결측치 확인
 
#결측치 값 대체
df3 <- df1
summary(df3)

#반복을 통한 na값 처리
col <- names(df3)
is.vector(col)
col[3:6]
col <- names(df1)[3:6]
col

for(c in col){
  temp <- df3[,c]
  temp <- ifelse(is.na(temp),0,temp)
  df3[,c] <- temp
}
summary(df3)

# 자연증가수 
head(df1)
df1$자연증가수 <- df1$출생아수 - df1$사망자수

df4 <- df1 %>% filter(df1$자연증가수 <= 0 & !is.na(df1$자연증가수))
df4
df4 <- df4[c("행정구역별","시점","자연증가수")]

# 전국 자료와 지역자료 분리
 
#데이터 분석
 

# 전국데이터 자연 증가수 그래프
 

library(ggplot2)
ggplot(mapping =aes(x=암종별, y=발생자수, fill=발생자수), data=df31) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("암종별 발생자수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'), axis.text.x=element_text(angle=90, hjust=1))



