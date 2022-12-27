# 고속도로 역주행 교통사고 와 일반 교통사고 분석
# https://www.news1.kr/articles/?4695261
# 최근 3년간 역주행 교통사고의 치명률이 10.2%로 일반 교통사고(4.7%) 보다 2.3배 높은 것으로 나타났다고 한다.  다음 주어진 데이터를 활용하여 이를 분석해 본다.
# 년도별 치명률 구하기
# 3년 평균 사고건수, 사망자수, 치명률(사망자수/사고건수) 구하기


# 엑셀 관련 패키지 추가
install.packages("readxl")
library(readxl)

# 엑셀데이터불러오기
dfxl <- read_excel("./data/02_역주행사고.xlsx")  
dfxl

# 전체와 역주행 데이터 테이블 나누기 : 인덱스로 나누기
df1 <- dfxl[dfxl$구분 == "전체",]
df2 <- dfxl[dfxl$구분 == "역주행",]

# 전체와 역주행 데이터 테이블 나누기 : subset()
df11 <- subset(dfxl, 구분 == "전체")
df12 <- subset(dfxl, 구분 == "역주행")

# 일반 교통사고 데이터 만들기
df3 <- df1
df3$구분 <- "일반"

df3[c("사고", "사망")] <- df1[c("사고", "사망")] - df2[c("사고", "사망")]
df3


# 치명률 구하기
df2$치명률 <- df2$사망 / df2$사고 * 100
df3$치명률 <- df3$사망 / df3$사고 * 100


# 일반 교통사고 
summary(df2)
summary(df3)

round(mean(df2$치명률),1)
round(mean(df3$치명률),1)

#시각화
install.packages("ggplot2")
library(ggplot2)

dfxl
dfxl$년도 <- as.factor(dfxl$년도)
dfxl$구분 <- as.factor(dfxl$구분)

ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=dfxl) +
  geom_bar(stat="identity", position=position_dodge())  +
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))


# 해결문제부산시 체납현황 분석
# https://www.data.go.kr/data/15079162/fileData.do#tab-layer-file
# 3년간 세목별을 키로 누적 체납건수와 누적 체납금액

dftax <- read.csv("./data/02_부산광역시_지방세 체납현황.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 


#세목명 
item <- unique(dftax$세목명)
item

makedf <- function(i) {   
  temp <- subset(dftax, 세목명 == i) 
  temp$과세년도 <- as.factor(temp$과세년도)
  
  ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill=과세년도), data=temp) +
    geom_bar(stat="identity", position=position_dodge())  +
    ggtitle(i)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
  
  #r <- c(mean(temp$누적체납건수) , mean(temp$누적체납금액))
  #return(r)
}


makedf("자동차세")

# 기상개황 자료를 분석하여 월별 불쾌지수와 단계
# 
# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE
# 
# 불쾌지수 공식
# 
# DI = 0.81 * Ta + 0.01 * RH(0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)
# 불쾌지수 단계
# 
# 매우높음: 80이상
# 높음: 75이상 80미만
# 보통: 68이상 75미만
# 낮음: 68미만
dfdi <- read.csv("./data/02_기상개황.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 
names(dfdi)
di <- dfdi[c("월별.1.", "평균기온....","평균상대습도...." )]
di
names(di) <- c("월", "평균기온","평균상대습도" )
di$DI <- 0.81 * di$평균기온 + 0.01 * di$평균상대습도 *(0.99 * di$평균기온 - 14.3) + 46.3
di$불쾌지수 <- ifelse(di$DI >= 80, "매우높음",
                  ifelse (di$DI >=75, "높음",
                      ifelse (di$DI >= 68, "보통", "낮음")))
di
