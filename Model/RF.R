rm(list=ls()) # clearing environment
library(dplyr)
library(Metrics)
library(glmnet)
library(randomForest)
library(readr)
library(caret)
install.packages("scatterplot3d")
library(scatterplot3d)
library(clock)
getwd()

# 확인할 DF들

urlfile="https://raw.githubusercontent.com/DATA-MINING23-BaseBall/R/main/Data_pre/df6_sample.csv"
df<-read_csv(url(urlfile))
head(df)
DF = df[-1]    # basic_df_sample.csv, df5_sample.csv, lFinal_df_sample.csv, df6_sample.csv
head(DF)

# train, test 범위
n = nrow(DF); n
train_n = as.integer(n*0.8); train_n   # train 분리 개수


get_rf_result = function(start_seed, iter_num, cv_num, df, n, train_n, ntree_para, mtry_para){
  start_time = sys_time_now()
  # 결과 저장 테이블 생성
  result.mat = matrix(NA, iter_num, 4)
  colnames(result.mat) = c("mse","mae","ntree_opt_para", "mtry_opt_para")
  
  
  # 모델 학습 진행
  for(i in 1:iter_num){
    cat("\niter = ", i)
    # 시드 설정
    set.seed(i + start_seed - 1)
    set = sample(1:n, train_n)   # train idx 구분
    
    # 데이터 분리
    train_data = df[set, ]    # train 데이터
    test_data = df[-set, ]    # test 데이터
    
    train_x = as.matrix(subset(train_data, select = -salary))
    train_y = as.matrix(subset(train_data, select = salary))
    test_x = as.matrix(subset(test_data, select = -salary))
    test_y = as.matrix(subset(test_data, select = salary))
    
    # cv * para 만큼 배열 만듦
    mse.vec = rep(0, length(ntree_para) * length(mtry_para) * cv_num)
    
    for(cv_count in 1:cv_num) {
      
      train_index <- sample(nrow(train_data), size = floor(0.8 * nrow(train_data)), replace = TRUE)
      
      ttrain_data <- train_data[train_index, ]
      tvalid_data <- train_data[-train_index, ]
      
      # 파라미터 최적화 코드 필요!!
      for(j in 1:length(ntree_para)){
        for(k in 1:length(mtry_para)){
          cat("\nntree = ", j, "\nmtry = ", k)
          model <- randomForest(salary ~ ., data = ttrain_data, ntree = ntree_para[j], mtry = mtry_para[k])
          pred = predict(model, tvalid_data)
          model_mse = mse(tvalid_data$salary, pred)
          mse.vec[(cv_count - 1) * length(ntree_para) * length(mtry_para) + (j-1) * length(mtry_para) + k] =  model_mse
        }
      }
    }
    
    #plot 그리기
    opt_para_idx = which.min(mse.vec)
    a = (opt_para_idx %% (length(ntree_para) * length(mtry_para)))
    if(a == 0) a = length(ntree_para) * length(mtry_para)
    ntree_opt_para = ntree_para[ ((a-1) %/% length(mtry_para)) + 1]
    b = a %% length(mtry_para)
    if(b==0) b = length(mtry_para)
    mtry_opt_para = mtry_para[b]
    
    # 최적화 된 파라미터로 학습 결과
    rf_model <- randomForest(salary ~ ., data = train_data, ntree = ntree_opt_para, mtry = mtry_opt_para)
    
    # 예측값 생성
    pred_y = predict(rf_model, test_data); length(pred_y)
    real_y = test_y; length(real_y)
    
    ltest_mse = mse(real_y, pred_y)     # mse 계산
    ltest_mae = mae(real_y, pred_y)     # mae 계산
    
    # 행렬에 값 추가
    result.mat[i,1] = ltest_mse
    result.mat[i,2] = ltest_mae
    result.mat[i,3] = ntree_opt_para
    result.mat[i,4] = mtry_opt_para
  }
  
  fit_time = sys_time_now() - start_time
  cat("\niter num = ", iter_num, "\ncv num = ", cv_num, "\nntree_para = ", as.character(ntree_para), "\nmtry_para = ", as.character(mtry_para), "\ntime = ", as.numeric(fit_time)/1000000000, "sec\n")
  return(result.mat)
}

# split(d, ceiling(seq_along(d)/20))

# 최빈값
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 
step = ncol(DF)%/%6
df.tbl = get_rf_result(
  start_seed = 1,
  iter_num = 100,
  cv_num = 5,
  DF, n, train_n, c(250, 500, 750, 1000), seq(step, step*5, step)); df.tbl

scatterplot3d(df.tbl[,3], df.tbl[,4], df.tbl[,1])
boxplot(df.tbl[,1])
boxplot(df.tbl[,2])
boxplot(df.tbl[,3])
boxplot(df.tbl[,4])
cat("mean_mse = ", mean(df.tbl[,1]), "\nmean_mae = ", mean(df.tbl[,2]), "\nntree_para = ", getmode(df.tbl[,3]), "\nmtry_para = ", getmode(df.tbl[,4]))
cat("median_mse = ", median(df.tbl[,1]), "\nmedian_mae = ", median(df.tbl[,2]))