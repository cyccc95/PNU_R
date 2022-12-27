#한국환경공단_에어코리아_대기오염통계 현황
#시도별 실시간 평균정보 조회 상세기능명세
#최근 한달간 지역별 일평균 대기오염 정보
#기준초과인경우 
#https://www.airkorea.or.kr/web/contents/contentView/?pMENU_NO=132&cntnts_no=6

#json처리
install.packages("jsonlite") 
library(jsonlite)

#자료처리
library(dplyr)


#데이터 가져오기함수
getData <- function(item, gubun) {
  
}

#1. pm10, O3 데이터 추출하여 합치기
 

 
df

#2. 지역명 벡터
area <- c("seoul", "busan", "daegu","incheon","gwangju",
          "daejeon", "ulsan", "gyeonggi", "gangwon",
          "chungbuk", "chungnam", "jeonbuk", "jeonnam",
          "gyeongbuk", "gyeongnam", "jeju", "sejong")

areaname <- c("서울","부산","대구","인천","광주","대전",
              "울산","경기","강원","충북","충남","전북",
              "전남","경북","경남","제주","세종")

 

#3. 통합데이터프레임 만들기
 


#4.주의보
#https://www.airkorea.or.kr/web/dustForecast?pMENU_NO=113
 

#5.일자별 주의보정보
names(dft)
 
names(dfn2) <- c("일자", "기준","기준수")
ggplot(dfn2, aes(x=일자, y =기준수, group=기준, color=기준)) +
  geom_line() +
  geom_point() +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#5.지역별 PM10이 좋음인 날 수 
names(dft)
 
ggplot(dfPM10, aes(x=area, y =n, fill=area)) +
  geom_bar(stat="identity") +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
