# 라이브러리 불러오기
library(dplyr)
library(ggplot2)

## 데이터 불러오기
df = read.csv('data/ex)2022_01.csv', fileEncoding='UTF-8')
df %>% head()

## 전처리하기

# 남,여 테이블 나누기
df1 = subset(df, select=c("행정구역","총인구수","남_연령구간인구수","남_10대미만","남_10대","남_20대","남_30대","남_40대","남_50대", "남_60대","남_70대","남_80대","남_90대","남_100세이상"))

df2 = subset(df, select=c("행정구역","총인구수","여_연령구간인구수","여_10대미만","여_10대","여_20대","여_30대","여_40대","여_50대", "여_60대","여_70대","여_80대","여_90대","여_100세이상"))

head(df2)

# 성별컬럼 추가하기
df1[,"성별"] = "남"
df2[,"성별"] = "여"


names(df1) # 컬럼명 출력/변경
names(df1) <- c("행정구역","총인구수","연령구간인구수","10대미만","10대","20대","30대","40대","50대","60대","70대","80대","90대","100세이상","성별")
names(df2)
names(df2) <- c("행정구역","총인구수","연령구간인구수","10대미만","10대","20대","30대","40대","50대","60대","70대","80대","90대","100세이상","성별")

head(df1)

# 테이블 병합
df1 <- merge(df1, df2,all = TRUE)

# 연령별 컬럼으로 옮기기
df_1 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","10대미만","성별"))
df_2 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","10대","성별"))
df_3 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","20대","성별"))
df_4 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","30대","성별"))
df_5 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","40대","성별"))
df_6 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","50대","성별"))
df_7 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","60대","성별"))
df_8 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","70대","성별"))
df_9 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","80대","성별"))
df_10 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","90대","성별"))
df_11 = subset(df1, select=c("행정구역","총인구수","연령구간인구수","100세이상","성별"))

# 연령구간에 나이대 넣어주기
df_1[,"연령구간"] = "10대미만"
df_2[,"연령구간"] = "10대"
df_3[,"연령구간"] = "20대"
df_4[,"연령구간"] = "30대"
df_5[,"연령구간"] = "40대"
df_6[,"연령구간"] = "50대"
df_7[,"연령구간"] = "60대"
df_8[,"연령구간"] = "70대"
df_9[,"연령구간"] = "80대"
df_10[,"연령구간"] = "90대"
df_11[,"연령구간"] = "100세이상"

# 확인하기
head(df_1)
head(df_2)
head(df_3)
head(df_4)
head(df_5)

# 구분하기 쉽게 해당 컬럼명 변경
names(df_1) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_2) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_3) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_4) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_5) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_6) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_7) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_8) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_9) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_10) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')
names(df_11) <- c("행정구역","총인구수","연령구간인구수","연령구간인구수2","성별",'연령구간')

# 병합하기
population2 <- merge(df_1,df_2,all=TRUE) %>%
  merge(df_3,all=TRUE) %>%
  merge(df_4,all=TRUE) %>% 
  merge(df_5,all=TRUE) %>% 
  merge(df_6,all=TRUE) %>%
  merge(df_7,all=TRUE) %>%
  merge(df_8,all=TRUE) %>%
  merge(df_9,all=TRUE) %>%
  merge(df_10,all=TRUE) %>%
  merge(df_11,all=TRUE)

# 확인하기
head(population2)
summarise(population2)
population2
population2[2]

population2[,2] <- as.integer(gsub(",", "", population2[,2])) # 총인구수
population2[,3] <- as.integer(gsub(",", "", population2[,3])) # 연령구간인구수
population2[,4] <- as.integer(gsub(",", "", population2[,4])) # 연령구간인구수2

typeof(population2) # 타입확인하기
mode(population2['연령구간인구수2'])
x = is.integer(population2['연령구간인구수'])

## 시각화하기

# 남여 인구수
population2 %>%
  filter() %>% 
  group_by(성별) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수))

# 남, 여 비율
population2 %>% 
  group_by(성별) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수)) %>% 
  ggplot(aes(성별, m, fill=성별)) +
  geom_col() + 
  geom_tile(color = "black")


# 여성의 비율이 조금 더 높은 것을 알 수 있다.
# 남성 = 281295344 / 여성 = 283136117

# 행정구역별 인구수
population2 %>% 
  group_by(행정구역) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수)) %>%
  ggplot(aes(행정구역, m, fill=행정구역)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  ggtitle("행정구역_인구수")

population2 %>% 
  group_by(연령구간) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2))


population2 %>% 
  group_by(연령구간) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2)) %>% 
  ggplot(aes(연령구간, m, fill=연령구간)) +
  geom_col() + 
  theme(axis.text.x=element_text(angle=45,colour="black")) + 
  geom_text(aes(y=m,label=m))

population2 %>% 
  group_by(연령구간) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2)) %>% 
  ggplot(aes(연령구간, m, group=1)) +
  geom_line() + 
  geom_point() +
  theme(axis.text.x=element_text(angle=45,colour="black")) 
# geom_text(aes(y=m,label=m))

# 성별이 남이고 연령구간별 인구수
population2 %>% 
  filter(성별=="남") %>% 
  group_by(성별,연령구간) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2)) %>% 
  ggplot(aes(연령구간, m, fill=연령구간)) +
  geom_col() +
  ggtitle("남_연령구간인구수")


population2 %>% 
  group_by(연령구간) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2))
# 막대그래프 
population2 %>% 
  filter(성별=="남") %>% 
  group_by(행정구역) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2)) %>%
  ggplot(aes(행정구역, m, fill=행정구역)) +
  geom_col() +
  ggtitle("남_행정구역_연령구간인구수") + 
  theme(axis.text.x=element_text(angle=45,colour="black"))

# 선그래프
population2 %>% 
  filter(성별=="여") %>% 
  group_by(행정구역) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2)) %>%
  ggplot(aes(행정구역, m, group=1)) +
  geom_line() +
  geom_point() + 
  ggtitle("여_행정구역_연령구간인구수") + 
  theme(axis.text.x=element_text(angle=45,colour="black"))

# 경기도 나이대별
population2 %>% 
  filter(행정구역=="경상남도" , 성별=="여") %>% 
  group_by(행정구역,연령구간) %>% 
  summarise(n = n(),
            m = sum(연령구간인구수2)) %>%
  ggplot(aes(연령구간, m, fill=연령구간)) +
  geom_col() +
  ggtitle("경상남도_여_연령구간인구수") + 
  theme(axis.text.x=element_text(angle=45,colour="black")) +
  geom_text(aes(y=m,label=m))


