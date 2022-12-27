#데이터 불러오기
df <- read.csv("./data/06_지상관측.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr') 

head(df)
names(df)

# 체감온도 = 13.12 + 0.6215*T – 11.37*V*0.16 + 0.3965*V*0.16*T
# T : 기온(°C), V : 풍속(km/h)

names(df) <- c( '지점', '지점명', '일시', '기온', '풍속', '상대습도')
names(df)

# 풍속 환산 : 1 m/s -> 3.6 km/h
df$풍속환산 <- round(df$풍속 * 3.6,2)
head(df)

# 체감온도
df$체감온도 <- 13.12 + 0.6215*df$기온 - 11.37*df$풍속환산*0.16 + 0.3965*df$풍속환산*0.16*df$기온
df$체감온도 <- round(df$체감온도, 2)

str(df)
unique(df$지점)

# 겨울철 체감온도 기온 10°C 이하, 풍속 1.3 m/s 이상
library(dplyr)
dfb <- df %>% 
        filter(지점명== "부산" & 기온 <= 10 & 풍속 >= 1.3)

1:nrow(dfb)
# 그래프
library(ggplot2)
ggplot(dfb, aes(x=1:nrow(dfb), y=체감온도, fill=체감온도)) +
  scale_x_continuous(breaks = seq(1,nrow(dfb)), labels = unlist(dfb$일시)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('부산지역 겨울철 체감온도')+
  xlab("") + ylab("")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold')) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(dfb, aes(fill=체감온도, y=체감온도, x=일시)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('부산지역 겨울철 체감온도')+
  xlab("") + ylab("")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold')) +
  theme(axis.text.x=element_text(angle=90, hjust=1))


# 지점명이 ’서울‘,’부산‘,’제주‘
df1 <- df %>%
  filter(지점명 %in% c("서울", "부산", "제주"))

# filter(지점명 == "서울" | 지점명 == "부산" | 지점명 == "제주" )  

df1

mean(df$기온)
ggplot(df1, aes(fill=지점명, y=기온, x=일시)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle('기온현황')+
  xlab("") + ylab("")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold')) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_hline(yintercept = mean(df$기온), linetype= "dashed",color="red", size=1)


#열지수
#HI = 0.5 * (T + 61.0 + ((T-68.0)*1.2) + (RH*0.094))
df$열지수 <- 
  0.5 * (df$기온 + 61.0 + ((df$기온-68.0)*1.2) + (df$상대습도*0.094))

#일자별 그룹핑 
dfg <- df %>%
  group_by(일시) %>%
  summarise(열지수평균 = mean(열지수), 
            기온평균 = mean(기온))
dfg 
dfg <- dfg %>% 
  filter(열지수평균 <= 5)
dfg

ggplot(df1, aes(x=일시, y=열지수, group = 지점명)) +
  geom_line(aes(color=지점명))+
  geom_point(aes(color=지점명)) +
  ggtitle('열지수 현황')+
  xlab("") + ylab("")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold')) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_hline(yintercept = 5, color="red", size=1)


