
# 기본 데이터 전처리 -> 널값 제거 (4/12)

#install.packages("tidyr")
library(dplyr)
library(tidyr)

# lFinal 가져오는 코드

# NA값 확인
summary(lFinal.df)
colSums(is.na(lFinal.df))

# 1. salary 데이터 NA값 처리
lFinal_2016.df = lFinal.df %>% filter(yearID < 2017)   # 2016이전 데이터만 확인
  # salary 채울 수 있는 NA값 채우기
lFinal_2016_NA.df = lFinal_2016.df %>% arrange(playerID, yearID) %>% group_by(playerID) %>%  fill(salary, .direction = "down")
lFinal.df = lFinal_2016_NA.df %>% filter(!is.na(salary))   # 채울 수 있는 값 채우기

# 2. 리그 ERA Inf값 처리
lFinal.df[lFinal.df$l_ERA == Inf, c("playerID", "yearID", "l_ER", "l_IPouts")]
lFinal.df[is.na(lFinal.df$l_ERA), c("playerID", "yearID", "l_ER", "l_IPouts")]  
third = quantile(lFinal.df$l_ERA, 0.75, na.rm=T)
lFinal.df[is.na(lFinal.df$l_ERA), "l_ERA"] = third   # NA값 3분위수로 채우기
lFinal.df[lFinal.df$l_IPouts == 0, "l_ERA"] = 99.99    # 무한대값 -> 200으로 채우기

# 3. 나머지 데이터 -> 0으로 채우기
col_names = colnames(aPitchingPost)
col_names = col_names[6:length(col_names)]
col_names = c(col_names, c("round", "GP", "n_award") ); col_names
for( col in col_names ){
  lFinal.df[[col]][is.na(lFinal.df[[col]])] = 0    # 열별 확인
}
colSums(is.na(lFinal.df))

# 4. po_에서 Inf값인 선수 처리
lFinal.df[lFinal.df$po_ERA == Inf, c("playerID", "yearID", "po_ER", "po_IPouts")]
lFinal.df[lFinal.df$po_ERA == Inf, "po_ERA"] = 99.99

# 5. po_ 에서 NA값 채우기
lFinal.df[(lFinal.df$po_ER == 0) & (lFinal.df$po_IPouts == 0), c("playerID", "yearID", "po_ER", "po_IPouts")]
third = quantile(lFinal.df$po_ERA, 0.75, na.rm=T)
lFinal.df[is.na(lFinal.df$po_ERA), "po_ERA"] = third   # NA값 3분위수로 채우기

# 5. 라벨인코딩
col_names = c("teamID", "lgID", "birthCountry", "throws", "divID")
for( col in col_names ){
  # 라벨인코딩
  datas = unique(lFinal.df[[col]])
  lFinal.df[[col]] = as.numeric(factor(lFinal.df[[col]], levels = datas))
}

# (4) debut 날짜만 저장
years = sapply(lFinal.df$debut, function(x) format(as.Date(x), "%Y"))
lFinal.df$debut = as.numeric(years)

summary(lFinal.df)
colSums(is.na(lFinal.df))

# 최종 데이터 프레임 저장
write.csv(lFinal.df, 'D:/R/데이터마이닝/baseball_project/lFinal_df.csv')
write.csv(lFinal.df, 'lFinal_df.csv')

# salary 분포 확인
library(tidyverse)

attach(lFinal_df)
summary(salary)
hist(salary)    # 극단값 처리 문제..
boxplot(salary ~ teamID)
boxplot(salary ~ yearID)
boxplot(salary ~ lgID)
boxplot(salary ~ round)
boxplot(salary ~ stint)
boxplot(salary ~ n_award)
boxplot(salary ~ l_W)
boxplot(salary ~ l_L)
