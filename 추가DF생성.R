# 분석할 데이터 프레임 생성
install.packages("ggplot2")
install.packages("caret")
install.packages("vctrs")
library(ggplot2)
library(caret)

df = lFinal.df    # 기존 데이터


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
# Inf 값은 3분위수로 채움 -> (최대값으로 넣어도 괜찮음)
t1 = quantile(df1$WHIP, 0.75, na.rm=T)
t2 = quantile(df1$FIP, 0.75, na.rm=T)
t3 = quantile(df1$ERC, 0.75, na.rm=T)
# WHIP, FIP, ERC는 모두 IPouts = 0 인 사람으로 인해 발생한 문제!! -> t1, t2, t3로 대체
df1[df1$WHIP == Inf, c("playerID", "yearID", "l_IPouts")]
df1[df1$WHIP == Inf, "WHIP"] = t1
df1[df1$FIP == Inf, c("playerID", "yearID", "l_IPouts")]
#df1[is.na(df1$FIP), c("l_HR","l_HBP","l_IBB", "l_SO","l_IPouts")]
df1[is.na(df1$FIP), "FIP"] = t2
df1[df1$FIP == Inf, "FIP"] = t2
df1[df1$ERC == Inf, c("playerID", "yearID", "l_IPouts")]
df1[df1$ERC == Inf, "ERC"] = t3
# ERAP는 Inf값이 많아서 추가 조작 필요
df1[df1$ERAP == Inf, c("playerID", "yearID", "l_ERA", "l_G")]   # 10경기 이하, 이상을 구분
df1[df1$ERAP == Inf & df1$l_G < 10, "ERAP"] = quantile(df1$ERAP, 0.75, na.rm=T)    # 10경기 미만 -> 3분위수
df1[df1$ERAP == Inf & df1$l_G >= 10, "ERAP"] = quantile(df1$ERAP, 0.25, na.rm=T)   # 10경기 이상 -> 1분위수

summary(df1)
df1 = df1[, -1]


# DF2
col_names = c("teamID", "lgID", "birthCountry", "throws", "divID")
df2 = df
for(col in col_names) {
  df2[[col]] = as.factor(df2[[col]])
}
dummy  = dummyVars("~.", data = df2[-1])
data2 = data.frame(predict(dummy, newdata = df2[-1]))


# DF3 (모든 열 표준화)
df3 = df[, -1:-2]
df3 = scale(df3)
df3 = as.data.frame(df3)


# DF4 (PCA 적용) -> 표준화 디폴트로 적용
df4 = df[, -1:-2]
str(df4)
salary = df4$salary    # target값 추출
df4 = subset(df4, select = -salary)   # feature들만 pca 진행
str(df4)
df_pca = prcomp(df4, scale = TRUE)    # PCA 진행

df_pca_eigen = df_pca$sdev^2          # 주성분들의 고유값을 확인
selected_pca = df_pca_eigen[df_pca_eigen > 1]  # 고유값이 1보다 큰 주성분들만 추출
n = length(selected_pca); n
df4 = predict(df_pca, newdata = df4)
df4 = as.data.frame(df4[, 1:n])    # 14개의 변수만 가짐
df4$salary = salary

colSums(is.na(df1))
colSums(is.na(df3))
colSums(is.na(df4))

# 최종 DF 확인
write.csv(df1, 'df1.csv')
write.csv(df2, 'df2.csv')
write.csv(df3, 'df3.csv')
write.csv(df4, 'df4.csv')

