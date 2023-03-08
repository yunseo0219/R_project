library(dplyr)
library(ggplot2)

## 데이터 불러오기
df = read.csv('data/소상공인_서울_전체.csv', encoding='UTF-8')
df %>% head()
# 컬럼 시군구코드 -> id로 변경
names(df) <- c("상호명","상권업종중분류명","시도명","id","시군구명","도로명주소","경도","위도")

# 상권업종중분류명이 커피점/카페인 것만 추출하기
df_cafe <- subset(df, 상권업종중분류명 == "커피점/카페")
df_cafe %>% head()

# 스타벅스가 포함된 문자 추출하기
df_starbucks = df_cafe %>% filter(grepl('스타벅스',상호명))
df_starbucks %>% head()

# 상호명을 모두 '스타벅스'로 통일하기
df_starbucks$상호명 <- "스타벅스" 
df_starbucks %>% head()

# 이디야, 이디아 포함된 문자 추출하기
df_ediya = df_cafe %>% filter(grepl('이디야',상호명) | grepl('이디아',상호명))
df_ediya %>% head()

# 상호명을 모두 '이디야'로 통일하기
df_ediya$상호명 <- "이디야" 
df_ediya %>% head()

# 메가가 포함된 문자 추출하기
df_mega = df_cafe %>% filter(grepl('메가',상호명))
df_mega %>% head()

# 상호명을 모두 '메가'로 통일하기
df_mega$상호명 <- "메가엠지씨커피" 
df_mega %>% head()

# 투썸플레이스가 포함된 문자 추출하기
df_twosome = df_cafe %>% filter(grepl('투썸플레이스',상호명))
df_twosome %>% head()

# 상호명을 모두 '투썸플레이스'로 통일하기
df_twosome$상호명 <- "투썸플레이스" 
df_twosome %>% head()

# 컴포즈가 포함된 문자 추출하기
df_compose = df_cafe %>% filter(grepl('컴포즈',상호명))
df_compose %>% head()

# 상호명을 모두 '컴포즈'로 통일하기
df_compose$상호명 <- "컴포즈커피" 
df_compose %>% head()

# 빽다방 포함된 문자 추출하기
df_paikdabang = df_cafe %>% filter(grepl('빽다방',상호명))
df_paikdabang %>% head()

# 상호명을 모두 '빽다방'로 통일하기
df_paikdabang$상호명 <- "빽다방" 
df_paikdabang %>% head()

## 서울지역중 카페가 가장 많은 지역 시각화
df_t <- table(df_cafe$시군구명)
df_cf <- data.frame(df_t)
df_cf
names(df_cf) <- c("시군구명",'count')

df_s %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_카페")
## 강남구가 가장 많다는 것을 알 수 있다.

# 지역별 스타벅스
df_t <- table(df_starbucks$시군구명)
df_s <- data.frame(df_t)
df_s # df_s = 지역별 스타벅스
names(df_s) <- c("시군구명",'count')

df_s %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_스타벅스")
## 강남구가 가장 많다는것을 알 수 있다.

# 지역별 이디야
df_t <- table(df_ediya$시군구명)
df_e <- data.frame(df_t)
df_e # df_e = 지역별 이디야
names(df_e) <- c("시군구명",'count')

df_e %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_이디야커피")
## 강서구가 가장 많으며, 그 다음으로 강남구와 송파구가 많다는것을 알 수 있다.

# 지역별 투썸
df_t <- table(df_twosome$시군구명)
df_ts <- data.frame(df_t)
df_ts # df_ts = 투썸
names(df_ts) <- c("시군구명",'count')

df_ts %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_투썸플레이스")
## 

# 지역별 메가
df_t <- table(df_mega$시군구명)
df_m <- data.frame(df_t)
df_m # df_m = 메가
names(df_m) <- c("시군구명",'count')

df_m %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_메가커피")
### 강서구가 가장 많으며, 지역별 비슷하게 분포되어 있다.

# 지역별 컴포즈커피
df_t <- table(df_compose$시군구명)
df_c <- data.frame(df_t)
df_c # df_c = 컴포즈커피
names(df_c) <- c("시군구명",'count')

df_c %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_컴포즈커피")
## 강서구가 가장 많으며, 강북구, 성북구, 용산구가 가장 적다는 것을 알 수 있다.

# 지역별 빽다방
df_t <- table(df_paikdabang$시군구명)
df_b <- data.frame(df_t)
df_b # df_b = 빽다방
names(df_b) <- c("시군구명",'count')

df_b %>% 
  ggplot(aes(시군구명,count,fill=시군구명)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("지역별_빽다방커피")
## 성북구가 가장 많으며, 양천구가 가장 적다는 것을 알 수 있다.

## 지도 시각화
library(ggmap)
library(raster)
library(tidyverse)
library(sf)
library(rgeos)
library(rgdal)
library(ggplot2)

ggmap_key <- "AIzaSyBGqIiCDsyypUnfbqdCpEZZZXpAXRBEIhQ"
register_google(ggmap_key)

seoul_map <- get_map("seoul", zoom=11, maptype="roadmap")
# 스타벅스 위치 점으로 출력해보기
ggmap(seoul_map) + 
  geom_point(data=df_starbucks, aes(x=경도,y=위도), 
             size=4, alpha=0.8, col="blue") 

map <- readOGR("data/TL_SCCO_SIG.shp") #지리 정보
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) # 좌표계 변환
map@polygons[[1]]@Polygons[[1]]@coords %>% head(n=10L)
new_map <- fortify(map, region = 'SIG_CD') # dataframe형태로 변경
seoul_map <- new_map[new_map$id <= 11740,] # 서울지역
seoul_map %>% head()

# 지역별로 빈도수 구하기
starbucks_sum <- df_starbucks %>% 
  group_by(id,시군구명) %>% 
  summarise(count = n())

# 서울지역과 스타벅스 데이터 합치기
starbucks_merge <- merge(seoul_map, starbucks_sum, by='id')
starbucks_merge %>% head()

## 지도에 텍스트 넣기 위해 구별로 경도,위도 구하기
# df_s = 지역별 스타벅스 매장 개수 들어있는 테이블
df_s %>% head()
df_s$시군구명 <- as.character(df_s$시군구명)
si <- geocode(df_s$시군구명) # 시군구명 따로 추출
si$lon <- as.numeric(si$lon) # 위도경도 변환
si$lat <- as.numeric(si$lat)
gu <- cbind(df_s,si) 
gu %>% head()

# 지역별 스타벅스 시각화1
ggplot() + geom_polygon(data = starbucks_merge, aes(x=long, y=lat, group=group, fill = count),color = "white")+
  ggtitle("서울시 스타벅스 분포도") + 
  geom_text(data = gu, 
            aes(x = lon, 
                y = lat, label=paste(시군구명, count, sep = "\n")),color="white")

# 지역별 스타벅스 시각화2
ggplot() + geom_polygon(data = starbucks_merge, aes(x=long, y=lat, group=group, fill = count), color = "grey") + 
  scale_fill_gradient(low = "white", high = "purple", space = "Lab", guide = F) + 
  theme_void() +
  ggtitle('서울 지역별 스타벅스') +
  geom_label(data=gu, mapping=aes(x = lon,y = lat,label=paste(시군구명,count)),alpha=0.5) +
  theme(plot.title = element_text(hjust = 0.9,size=15,face='bold'))



