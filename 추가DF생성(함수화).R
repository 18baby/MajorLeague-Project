# 추가DF생성 [함수화]


# 수정(5/8)


# 라이브러리
library(ggplot2)
library(caret)
library(tidyr)


df = lFinal_df_last_[-1]        # 기존 데이터(이름 포함)
removed_name_df = df[, -1]      # 이름 제거한 기존 데이터
head(df)
head(removed_name_df)


# [함수1] 논문 추가지표 생성 함수
make_DF1.fun = function(DF){
  df = DF
  # *** 추가지표 계산 ***
  attach(df)
  df$WHIP = (l_H + l_BB)/(l_IPouts/3)
  C = 3.2    # ** 리그별 값을 적용하기 어려움(대략 적인 값으로 계산)
  df$FIP = (l_HR*13 + ((l_BB + l_HBP - l_IBB)*3) - l_SO*2)/(l_IPouts/3) + C
  PTB = (((l_H - l_HR)*1.255) + l_HR*4)*0.89 + (l_BB + l_HBP - l_IBB)*0.56
  df$ERC = (((l_H + l_BB + l_HBP)*PTB)/(l_BFP*l_IPouts/3)*9) - 0.56
  ERA_l = mean(l_ERA)   # ** 리그별 값을 적용하기 어려움..
  df$ERAP = ((ERA_l/l_ERA)*PPF)*100
  df$kwERA = ((l_SO - (l_BB + l_HBP - l_IBB))*12 / l_BFP) + 5.4
  A = (l_H + l_BB + l_HR)
  B = ((l_H*1.12 + l_HR*4)*1.4 - l_H*0.6 - l_HR*3 + l_BB*0.1)*1.1
  C = l_IPouts
  D = l_HR
  df$BSR = (A*B)/(B+C) + D
  detach(df)
  
  # ***새로 대입한 값 이상치 처리***
  # Inf 값은 3분위수로 채움 -> (최대값으로 넣어도 괜찮음)
  t1 = quantile(df$WHIP, 0.75, na.rm=T)
  t2 = quantile(df$FIP, 0.75, na.rm=T)
  t3 = quantile(df$ERC, 0.75, na.rm=T)
  # WHIP, FIP, ERC는 모두 IPouts = 0 인 사람으로 인해 발생한 문제!! -> t1, t2, t3로 대체
  df[df$WHIP == Inf, c("yearID", "l_IPouts")]
  df[df$WHIP == Inf, "WHIP"] = t1
  df[df$FIP == Inf, c("yearID", "l_IPouts")]
  #df1[is.na(df1$FIP), c("l_HR","l_HBP","l_IBB", "l_SO","l_IPouts")]
  df[is.na(df$FIP), "FIP"] = t2
  df[df$FIP == Inf, "FIP"] = t2
  df[df$ERC == Inf, c("yearID", "l_IPouts")]
  df[df$ERC == Inf, "ERC"] = t3
  # ERAP는 Inf값이 많아서 추가 조작 필요
  df[df$ERAP == Inf, c("yearID", "l_ERA", "l_G")]   # 10경기 이하, 이상을 구분
  df[df$ERAP == Inf & df$l_G < 10, "ERAP"] = quantile(df$ERAP, 0.75, na.rm=T)    # 10경기 미만 -> 3분위수
  df[df$ERAP == Inf & df$l_G >= 10, "ERAP"] = quantile(df$ERAP, 0.25, na.rm=T)   # 10경기 이상 -> 1분위수
  
  print(summary(df))  # 결과 확인
  return(df)
}



# [함수2] 모든 범주형 변수 원핫 인코딩 함수 (이름 없는 DF 대입)
make_DF2.fun = function(unamed_DF){
  df = unamed_DF
  col_names = c("teamID", "lgID", "birthCountry", "throws", "divID")
  for(col in col_names) {
    df[[col]] = as.factor(df[[col]])
  }
  dummy  = dummyVars("~.", data = df)
  data2 = data.frame(predict(dummy, newdata = df))
  return(data2)
}

# [함수3] 필요 없는 변수 제거 + 변수별 전처리 (이름 없는 DF 대입)
make_DF5.fun = function(DF){
  df = subset(DF, select = -playerID)
  playerID = subset(DF, select = playerID)
  
  attach(df)
  # (1) 중간계투로 던진 경기 추가 -> 총 경기수 제거
  df$l_GM = l_G - l_GS   
  
  # (2) 관중수 범주화 -> 4분위수 기준 순서화
  Q = quantile(df$attendance)
  df$attendance.f[attendance < Q[2]] = 0
  df$attendance.f[(Q[2] <= attendance) & (attendance < Q[3])] = 1
  df$attendance.f[(Q[3] <= attendance) & (attendance < Q[4])] = 2
  df$attendance.f[Q[4] <= attendance] = 3
  summary(df$attendance.f)
  
  # (3) 연도 데이터 변경 (연차 + 나이 추가)
  df$career = yearID - debut         # 선수의 연차 정보
  df$years_old = yearID - birthYear  # 당시 선수 나이
  
  # (4) 쓸모 없음 or 수정한 열 제거
  df = subset(df, select = -c(lgID, divID, G, l_G, attendance, debut, birthYear, birthCountry, teamID, throws, stint)) 
  
  # (5) 원핫 인코딩 진행  -> teamID, throws
  df$teamID = as.factor(teamID)
  df$throws = as.factor(throws)
  df = as.data.frame(dummyVars(~., data = df) %>% predict(newdata= df))
  detach(df)
  
  # 선수 이름 다시 합치기
  df = cbind(df, playerID)
  
  return(df)
}


# 시계열 DF 생성 함수 추가 필요
library(plyr)
library(data.table)

make_DF6.fun = function(df_basic){
  attach(df_basic)
  AAA.df = df_basic
  nm1 = c('WHIP', 'FIP', 'ERC', 'ERAP', 'kwERA', 'BSR', 'round', 'Rank', 'attendance.f','l_GM')
  nm2 = paste("b1", nm1, sep = ".")                       
  setDT(AAA.df)  
  siga.df = AAA.df[, (nm2) := shift(.SD), by=playerID, .SDcols=nm1]           # 1년전 데이터 추가
  
  nm3 = paste("b2", nm1, sep = ".")
  siga.df = siga.df[, (nm3) := shift(.SD, 2), by=playerID, .SDcols=nm1]       # 2년전 데이터 추가
  siga.df_pre = na.omit(siga.df)                          # NA값 제거
  detach(df_basic)
  
  return(siga.df_pre)
}

# DFT (teamID : one-hot encoding -> mean target Encoding) 
make_DFT.fun = function(lFinal_df_last_) {
  lFinal_ex = lFinal_df_last_
  lFinal_e = lFinal_ex %>% group_by(yearID, teamID) %>%  summarise(salary.n = mean(salary))
  lFinal.df = left_join( lFinal_ex, lFinal_e, by=c('yearID', 'teamID'), multiple = "all")    # 1998년~2022년 모든 투수 데이터
  lFinal.df$salary.c = lFinal.df$salary-lFinal.df$salary.n
  boxplot(lFinal.df$salary.c)
  lFinal.df = subset(lFinal.df, select = -c(salary.c))
  lFinal.df = subset(lFinal.df, select = -c(teamID))
  lFinal.df = subset(lFinal.df, select = -c(stint))
  
  mydata = lFinal.df
  str(mydata)
  names(mydata)
  df=mydata[,-1:-2]
  df = mydata
  names(df)
  names(df5)
  
}



# 상관계수 0.24 이상 변수 모두 제거
make_DF7.fun = function(df){
  # 'playerID' 열을 제외한 데이터프레임 생성
  original_df = subset(df, select = -playerID)
  # 상관계수 계산
  cor_df = as.data.frame(cor(original_df))
  hcor_df = subset(cor_df, select = salary, abs(salary) >= 0.2)
  hcor_colnames = rownames(hcor_df)
  df7 = subset(df, select = hcor_colnames)
  df7$playerID = df$playerID
  return(df7)
}


df1 = make_DF1.fun(df)              # 추가지표 추가   ===(선수 이름 포함)===
df5 = make_DF5.fun(df)              # 기본 처리 DF
df_basic = make_DF5.fun(df1)        # basic (DF1 + DF5)
df6 = make_DF6.fun(df_basic)        # 시계열 처리 DF
df7 = make_DF7.fun(df6)

df5 = subset(df5, select = -playerID)
df_basic = subset(df_basic, select = -playerID)
df6 = subset(df6, select = -playerID)
df7 = subset(df7, select = -playerID)

setwd("D:/R/데이터마이닝/baseball_project/R")
# 최종 DF 확인
write.csv(df1, 'Data_pre/df1.csv')
write.csv(df5, 'Data_pre/df5.csv')
write.csv(df_basic, 'Data_pre/df_basic.csv')
write.csv(df6, 'Data_pre/df6.csv')
write.csv(df7, 'Data_pre/df7.csv')
summary(df$salary)


