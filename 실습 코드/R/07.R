#json처리하는데 많이 사용하는 패키지
install.packages("jsonlite") 
library(jsonlite)

#일일박스 오피스 자료 가져오기
#url <- "https://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=f5eef3421c602c6cb7ea224104795888&targetDt=20221220"

box <- function(dt) {
  apikey <- "f5eef3421c602c6cb7ea224104795888"
  url <- paste("http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
               "key=", apikey ,
               "&targetDt=" , dt, sep="")  
  mv <- fromJSON(url) 
  
  #박스오피스 목록 추출
  BoxOfficeList <- mv$boxOfficeResult$dailyBoxOfficeList
  return (BoxOfficeList)
}

box("20221221")


#데이터형변환 : 수치데이터 변환
names(BoxOfficeList)
str(BoxOfficeList)

col <- c("rnum", "rank", "rankInten",
         "salesAmt", "salesShare", "salesInten" ,
         "salesChange","salesAcc",
         "audiCnt" ,"audiInten","audiChange",
         "audiAcc","scrnCnt","showCnt")


for (c in col ){
  BoxOfficeList[c] <- as.numeric(unlist(BoxOfficeList[c]))
}

# 매출평균보다 매출이 높은 영화
library(dplyr)
BoxOfficeList %>%
  filter(salesAmt > mean(salesAmt) ) %>%
  select(rank, movieNm, salesAmt)

mean(BoxOfficeList$salesAmt)


plot(BoxOfficeList$salesAmt,
     type='o',
     col ='blue',
     main = '박스오피스 매출액',
     xlab='', ylab='')

barplot(BoxOfficeList$salesAmt)


library(ggplot2)
ggplot(data=BoxOfficeList, aes(x=reorder(movieNm,rank) , y=audiAcc, fill=movieNm)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "none")


#요일별 음식물 쓰레기량
getData <- function(y, m) {
  apikey = '8qw7g%2FC%2BMGd2iRqEvb%2FEx0Sg3ZwAAsnS%2FQ7rRaU3l4UUYfNWgyAbYpNw541yy9pueEvoCcNwmCww8ss32BBWEA%3D%3D'
  url <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?" ,
          "ServiceKey=" , apikey ,
          "&type=json&page=1" ,
          "&disYear=" , y,
          "&disMonth=" , m, sep="" )
  
  rdata <- fromJSON(url)
  return (rdata$data$list) 
}

df2020 <- getData("2020", "08")
df2021 <- getData("2021", "08")
df2022 <- getData("2022", "08")

#데이터 합치기
df <- bind_rows(df2020, df2021, df2022)

dft <- data.frame() ;

for(i in 1:12) {
  if (i < 10) { m = paste("0", i, sep="")}
  else m = as.character(i) ;
  
  temp <- getData("2021", m)
  dft <- bind_rows(dft, temp)
}

dft

#데이터 열 생성
df <- df %>%
  mutate(disWeek = case_when(disDay == 1 ~ "일",
                             disDay == 2 ~ "월",
                             disDay == 3 ~ "화",
                             disDay == 4 ~ "수",
                             disDay == 5 ~ "목",
                             disDay == 6 ~ "금",
                             disDay == 7 ~ "토"
  ))



#요일별 쓰레기 배출량
unique(df$disYear)
df$disYear <- as.factor(df$disYear)
ggplot(df, aes(x=reorder(disWeek, disDay), y =disQuantity, fill=disYear)) +
geom_bar(stat="identity", position = "dodge") +
labs(x="", y="")





