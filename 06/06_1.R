getwd()
setwd('c:/Rwork/06')

feeltemp <- read.csv(file = '06_지상관측.csv', header = T, sep = ',', fileEncoding = 'cp949')

#체감온도 = 13.12 + 0.6215*T - 11.37*V*0.16 + 0.3965*V*0.16*T
library(dplyr)

names(feeltemp)
# "지점" , "지점명","일시" , "평균기온..C.", "평균.풍속.m.s.",   "평균.상대습도..."  

names(feeltemp) <- c("지점" , "지점명","일시" , "기온", "풍속",   "상대습도" )

feeltemp1 <- feeltemp %>% filter(지점명 == '부산')

feeltemp1[, '풍속'] <- feeltemp1[, '풍속'] * 3.6

# 풍속을 시속으로 바꿔서 4.68 밀어넣음
feeltemp1 %>% filter(기온 <= 10) %>% filter(풍속 >= 4.68)

# 겨울철 체감온도만 필터링해서
feeltemp2 <- feeltemp1 %>% filter(기온 <= 10) %>% filter(풍속 >= 4.68)
# 어지간하면 & 써라.
feeltemp2 <- feeltemp1 %>% filter((기온 <= 10) & (풍속 >= 4.68))
# 체감온도 공식대로 뽑고
feeltemp2['체감온도'] <- 13.12 + 0.6215*feeltemp2['기온'] - 11.37*feeltemp2['풍속']*0.16 + 0.3965 * feeltemp2['풍속']*0.16*feeltemp2['기온']

# 그리기: x축이 날짜 y축이 체감온도
ggplot(data = feeltemp2, mapping = aes(x = 일시, y = 체감온도, fill=체감온도)) + geom_bar(stat = "identity") + ggtitle('부산지역 겨울철 체감온도') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))

# =====================================================

# 열지수 = HI = 0.5 * {T + 61.0 + [(T-68.0)*1.2] + (RH*0.094)}
# 열지수 = HI = 0.5 * (T + 61.0 + ((T-68.0)*1.2) + (RH*0.094))

heat <- read.csv(file = '06_지상관측.csv', header = T, sep = ',', fileEncoding = 'cp949')

heat1 <- heat %>% filter(지점명 == c('서울', '부산', '제주'))
heat1 <- heat %>% filter(지점명 %in% c('서울', '부산', '제주'))
names(heat1) <- c("지점" , "지점명","일시" , "기온", "풍속",   "상대습도" )
# T = heat1['기온']
# RH = heat1['상대습도']
heat1['열지수'] <- 0.5 * (heat1['기온'] + 61.0 + ((heat1['기온']-68.0)*1.2) + (heat1['상대습도']*0.094))

heat1$일시 <- as.character(heat1$일시)

# 기온의 평균을 구해서 geom_hline
mean_temp <- mean(heat1$기온)

# 일단 기온현황 그래프를 그리고
ggplot(data = heat1, mapping = aes(x = 일시, y=기온, fill=지점명)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  # scale_x_date(date_breaks = "1 days")
  ggtitle('기온현황') + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) + 
  geom_hline(yintercept = mean_temp, color = "red", linetype = 2)

ggplot(data = heat1, mapping = aes(x = 일시, y=열지수, fill=지점명, color=지점명)) + geom_point(size = 1) + geom_line(mapping = aes(group = 지점명)) + ggtitle('열지수 현황') + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 5, color="red") 

# 열지수 5이하인 자료만 추출
under_five <- heat1 %>% filter(열지수 <=5)

# 열지수 5이하인 자료만 group_by를 쓰고 싶으면 묶고 싶은 기준을 정해서 summarise
group <- heat1 %>% group_by(일시) %>% filter(열지수 <=5)