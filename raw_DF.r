# 기본 DF -> lFinal 생성(4/12)

rm(list=ls()) # clearing environment
cat("\014") # clearing Console

library(dplyr)

setwd('D:/R/데이터마이닝/데이터')
getwd()


# 1. 데이터 읽기
aAllstarFull = read.csv('AllstarFull.csv')
aAwardsPlayers = read.csv('AwardsPlayers.csv')           
aPeople = read.csv('People.csv')
aPitching = read.csv('Pitching.csv')          
aPitchingPost = read.csv('PitchingPost.csv')  
aSalaries = read.csv('Salaries.csv')
aTeams = read.csv('Teams.csv')



# 2. 데이터 확인 및 필요한 열만 추출 (앞에 my_ 붙이기)
  # (1) target 데이터
my_salaries = subset(aSalaries, select = c(playerID, yearID, teamID, salary), subset = (aSalaries$yearID >= 1999))   # target -> 2016년까지만 있어서 아직 미결합
my_salaries$yearID = my_salaries$yearID-1
summary(my_salaries)
my_salaries = subset(my_salaries, subset = (my_salaries$salary != 0))   # 연봉 0원인 사람 삭제
subset(aPitching, subset = (aPitching$playerID == 'martija02'))         # 연봉 0원인 사람 데이터

  # (2) feature들
    # pitching 데이터
Pitching = subset(aPitching, select = -c(l_ERA, l_BAOpp), subset = aPitching$yearID >= 1998)
summary(Pitching)
    # 년도별 선수를 기준으로 통합 -> 최종 my_pitching
my_Pitching = Pitching %>% group_by(playerID, yearID) %>% reframe(stint=max(stint), teamID = teamID, lgID = lgID, l_W = sum(l_W), l_L = sum(l_L), l_G = sum(l_G), l_GS = sum(l_GS),
                                                                  l_CG = sum(l_CG), l_SHO = sum(l_SHO), l_SV = sum(l_SV), l_IPouts = sum(l_IPouts), l_H = sum(l_H), l_ER = sum(l_ER), l_HR = sum(l_HR),
                                                                  l_BB = sum(l_BB), l_SO = sum(l_SO), l_IBB = sum(l_IBB), l_WP = sum(l_WP), l_HBP = sum(l_HBP), l_BK = sum(l_BK), l_BFP = sum(l_BFP), l_GF = sum(l_GF),
                                                                  l_R = sum(l_R), l_SH = sum(l_SH), l_SF = sum(l_SF), l_GIDP = sum(l_GIDP) )   
my_Pitching = my_Pitching %>% group_by(playerID, yearID) %>% slice(1)   # 첫번째 행으로 병합
my_Pitching$l_ERA = (my_Pitching$l_ER*9)/(my_Pitching$l_IPouts/3)       # ERA 추가
my_Pitching$l_BAOpp = (my_Pitching$l_H)/(my_Pitching$l_BFP)             # BAOpp 추가
summary(my_Pitching)  # 데이터 확인


    # post pitching 데이터
PitchingPost = subset(aPitchingPost, select = -c(lgID, po_ERA, po_BAOpp), subset = aPitchingPost$yearID >= 1998)
distinct(PitchingPost, round, .keep_all = TRUE)['round']    # 라운드 종류 확인
      # round별 통합 -> grouping (round=1)
my_PitchingPost = PitchingPost %>% group_by(playerID, yearID) %>% reframe(round = 1, teamID = teamID, po_W = sum(po_W), po_L = sum(po_L), po_G = sum(po_G), po_GS = sum(po_GS),
                                                                          po_CG = sum(po_CG), po_SHO = sum(po_SHO), po_SV = sum(po_SV), po_IPouts = sum(po_IPouts), po_H = sum(po_H), po_ER = sum(po_ER), po_HR = sum(po_HR),
                                                                          po_BB = sum(po_BB), po_SO = sum(po_SO), po_IBB = sum(po_IBB), po_WP = sum(po_WP), po_HBP = sum(po_HBP), po_BK = sum(po_BK), po_BFP = sum(po_BFP), po_GF = sum(po_GF),
                                                                          po_R = sum(po_R), po_SH = sum(po_SH), po_SF = sum(po_SF), po_GIDP = sum(po_GIDP))
my_PitchingPost = my_PitchingPost %>% group_by(playerID, yearID) %>% slice(1)          # 첫번째 행 가져오기
my_PitchingPost$po_ERA = (my_PitchingPost$po_ER*9)/(my_PitchingPost$po_IPouts/3)       # ERA 추가
my_PitchingPost$po_BAOpp = (my_PitchingPost$po_H)/(my_PitchingPost$po_BFP)             # BAOpp 추가
summary(my_PitchingPost)


    # 선수 기본 데이터
my_People = subset( aPeople, select = c(playerID, birthYear, birthCountry, throws, debut) )
summary(my_People)


    # 올스타전 출전 여부 데이터
my_AllstarFull = subset(aAllstarFull, select = -c(gameID, lgID, startingPos, gameNum), subset = (aAllstarFull$yearID >= 1998))
summary(my_AllstarFull)


    # 수상 데이터
my_AwardsPlayers = subset(aAwardsPlayers, select = -c(tie, lgID, notes), subset = (aAwardsPlayers$yearID >= 1998))
summary(my_AwardsPlayers)
awards = unique(my_AwardsPlayers$get_awardID)        # 수상 가능한 상 종류 -> 순서대로 라벨 인코딩
my_AwardsPlayers$get_awardID = as.numeric(factor(my_AwardsPlayers$get_awardID, levels=awards))
      # 수상 내역 그룹핑
my_AwardsPlayers = my_AwardsPlayers %>% group_by(playerID, yearID) %>% reframe(n_award = n_distinct(get_awardID))
summary(my_AwardsPlayers)


    # 팀 데이터
my_Teams = subset(aTeams, select = c("teamID", "yearID", "divID", "Rank", "G", "W", "attendance", "PPF"), subset = (aTeams$yearID >= 1998))
summary(my_Teams)




# 3. 데이터 병합

  # 1차 데이터 결합 -> (투수 기록), (추가 기록)
playdata.df = left_join( my_Pitching, my_PitchingPost, by=c('playerID', 'yearID', 'teamID'))    # 투수 기록
other.df = full_join( my_AllstarFull, my_AwardsPlayers, by=c("playerID", "yearID"))             # 올스타 + 수상여부

  # 2차 데이터 결합 -> 투수 정보 (성적 + 개인정보)
pitcher.df = inner_join(playdata.df, my_People, by='playerID') 

  # 3차 데이터 결합 -> 투수 정보 (성적 + 개인정보 + 추가 기록)
pfinal_pitcher.df = left_join( pitcher.df, other.df, by=c('playerID', 'yearID', 'teamID'), multiple = "all")  # 데이터 1개 늘어나는 문제
pfinal_pitcher.df[ !(pfinal_pitcher.df$playerID %in% pitcher.df$playerID), ]  # 확인용
pfinal_pitcher.df = unique(pfinal_pitcher.df)

  # 4차 데이터 결합 -> 투수 정보 (성적 + 개인정보 + 추가 기록 + 팀정보)
pfinal_pitcher_Team.df = inner_join( pfinal_pitcher.df, my_Teams, by=c('yearID', 'teamID'), multiple = "all")

  # 최종 데이터 결합 -> 투수 정보 (성적 + 개인정보 + 추가 기록 + 팀정보 + 연봉) 
iFinal.df = inner_join( pfinal_pitcher_Team.df, my_salaries, by=c('playerID', 'yearID', 'teamID'), multiple = "all")   # 2016년 이후 데이터 무시
lFinal.df = left_join( pfinal_pitcher_Team.df, my_salaries, by=c('playerID', 'yearID', 'teamID'), multiple = "all")    # 1998년~2022년 모든 투수 데이터
summary(lFinal.df)
summary(iFinal.df)
length(lFinal.df[is.na(lFinal.df$salary), ])   # salary값이 없는 데이터 69개

# 최종 DF 저장
write.csv(lFinal.df, 'lFinal_df.csv')



# 종합본 저장
setwd('D:/R/데이터마이닝')

# 1차결합
write.csv(playdata.df, 'playdata_df.csv')
write.csv(other.df, 'other_df.csv')
# 2차 결합
write.csv(pitcher.df, 'pitcher_df.csv')
# 3차결합
write.csv(pfinal_pitcher.df, 'final_pitcher_df.csv')
# 4차 결합
write.csv(pfinal_pitcher_Team.df, 'final_pitcher_Team_df.csv')
# 최종 파일
write.csv(iFinal.df, 'iFinal_df_1.csv')
write.csv(lFinal.df, 'lFinal_df.csv')


df = lFinal.df[lFinal.df["yearID"] <= 2016, ]
df2 = lFinal.df[lFinal.df["yearID"] == 2017, ]
summary(df)

