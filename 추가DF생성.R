# 수정(5/8)


# 분석할 데이터 프레임 생성
#install.packages("vctrs")
#install.packages("ggplot2")
#install.packages("caret", dependencies = TRUE)
library(ggplot2)
library(caret)
library(tidyr)

setwd("D:/R/datamining/baseball_project/R")

df = lFinal_df_last_[-1]        # 기존 데이터
removed_name_df = df[, -1]      # 이름 제거한 기존 데이터
head(df)

# DF1 (논문 추가 지표) -> [6개 지표 추가]
df1 = df
head(df1)
  # 포스트 시즌 데이터의 경우 NA값이 너무 많아 리그값만 추가 
attach(df1)
df1$WHIP = (l_H + l_BB)/(l_IPouts/3)
C = 3.2    # ** 리그별 값을 적용하기 어려움(대략 적인 값으로 계산)
df1$FIP = (l_HR*13 + ((l_BB + l_HBP - l_IBB)*3) - l_SO*2)/(l_IPouts/3) + C
PTB = (((l_H - l_HR)*1.255) + l_HR*4)*0.89 + (l_BB + l_HBP - l_IBB)*0.56
df1$ERC = (((l_H + l_BB + l_HBP)*PTB)/(l_BFP*l_IPouts/3)*9) - 0.56
ERA_l = mean(l_ERA)   # ** 리그별 값을 적용하기 어려움..
df1$ERAP = ((ERA_l/l_ERA)*PPF)*100
df1$kwERA = ((l_SO - (l_BB + l_HBP - l_IBB))*12 / l_BFP) + 5.4
A = (l_H + l_BB + l_HR)
B = ((l_H*1.12 + l_HR*4)*1.4 - l_H*0.6 - l_HR*3 + l_BB*0.1)*1.1
C = l_IPouts
D = l_HR
df1$BSR = (A*B)/(B+C) + D
  #df1$BABIP = (l_H - l_HR)/(l_BFP-l_SO-l_HR+l_SF)   # BABIP는 투수와 직접적인 연관이 없다는 의견이 대부분이므로 제거
# 투수 WAR는 계산식을 모르겠음..
detach(df1)
summary(df1)

# ***새로 대입한 값 이상치 처리***
# Inf 값은 3분위수로 채움 -> (최대값으로 넣어도 괜찮음)
t1 = quantile(df1$WHIP, 0.75, na.rm=T)
t2 = quantile(df1$FIP, 0.75, na.rm=T)
t3 = quantile(df1$ERC, 0.75, na.rm=T)
# WHIP, FIP, ERC는 모두 IPouts = 0 인 사람으로 인해 발생한 문제!! -> t1, t2, t3로 대체
df1[df1$WHIP == Inf, c("yearID", "l_IPouts")]
df1[df1$WHIP == Inf, "WHIP"] = t1
df1[df1$FIP == Inf, c("yearID", "l_IPouts")]
#df1[is.na(df1$FIP), c("l_HR","l_HBP","l_IBB", "l_SO","l_IPouts")]
df1[is.na(df1$FIP), "FIP"] = t2
df1[df1$FIP == Inf, "FIP"] = t2
df1[df1$ERC == Inf, c("yearID", "l_IPouts")]
df1[df1$ERC == Inf, "ERC"] = t3
# ERAP는 Inf값이 많아서 추가 조작 필요
df1[df1$ERAP == Inf, c("yearID", "l_ERA", "l_G")]   # 10경기 이하, 이상을 구분
df1[df1$ERAP == Inf & df1$l_G < 10, "ERAP"] = quantile(df1$ERAP, 0.75, na.rm=T)    # 10경기 미만 -> 3분위수
df1[df1$ERAP == Inf & df1$l_G >= 10, "ERAP"] = quantile(df1$ERAP, 0.25, na.rm=T)   # 10경기 이상 -> 1분위수
summary(df1)
df1
str(df1)


# DF2 [모든 범주형 변수 원핫 인코딩]
col_names = c("teamID", "lgID", "birthCountry", "throws", "divID")
df2 = df
for(col in col_names) {
  df2[[col]] = as.factor(df2[[col]])
}
dummy  = dummyVars("~.", data = df2[-1])
data2 = data.frame(predict(dummy, newdata = df2[-1]))
df2 = data2

# DF5 필요 없는 변수 제거 + 변수별 전처리
df5 = removed_name_df
attach(df5)
  # 중간계투로 던진 경기 추가 -> 총 경기수 제거
df5$l_GM = l_G - l_GS   

  # 관중수 범주화 -> 4분위수 기준 순서화
boxplot(attendance)
hist(attendance)
Q = quantile(df5$attendance)
df5$attendance.f[attendance < Q[2]] = 0
df5$attendance.f[(Q[2] <= attendance) & (attendance < Q[3])] = 1
df5$attendance.f[(Q[3] <= attendance) & (attendance < Q[4])] = 2
df5$attendance.f[Q[4] <= attendance] = 3
summary(df5$attendance.f)
  
  # 연도 데이터 변경
df5$career = yearID - debut         # 선수의 연차 정보
df5$years_old = yearID - birthYear  # 당시 선수 나이

  # stint 이진화
df5$stint.f[stint > 1] = 2    # 시즌 중 이적이 있는 경우
df5$stint.f[stint == 1] = 1   # 시즌 중 이적이 없는 경우

  # 쓸모 없음 or 수정한 열 제거
df5 = subset(df5, select = -c(lgID, divID, G, l_G, attendance, debut, birthYear, birthCountry, teamID, throws, stint)) 

  # 원핫 인코딩 진행  -> 팀ID, 던지는 방향
df5$teamID = as.factor(teamID)
df5$throws = as.factor(throws)
df5 = as.data.frame(dummyVars(~., data = df5) %>% predict(newdata= df5))




hist(df$stint)
hist(df$teamID)
hist(df$lgID)
hist(df$divID)
hist(df$birthCountry)
summary(df$l_ERA)
plot(df$birthCountry, df$salary)
plot(df$throws, df$salary)
plot(df$stint, df$salary)

# 최종 DF 확인
write.csv(df1, 'Data_pre/df1.csv')
write.csv(df2, 'Data_pre/df2.csv')
write.csv(df5, 'Data_pre/df5.csv')

summary(df$salary)


