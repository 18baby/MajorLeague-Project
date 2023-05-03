rm(list=ls()) # clearing environment
cat("\014") # clearing Console
setwd("~/RWorkingDirectory")

# 투수 데이터 프레임 생성
installed.packages("dplyr")
library(dplyr)

# 데이터 만들기(대충)
total_data <- read.csv("iFinal_df_1.csv")
total_data <- total_data[,-1:-2]
head(total_data)

#데이터 나누기
n <- nrow(total_data)
set.seed(1234)
train_idx <- sample(n, 0.8*n, replace = FALSE)
train_data <- total_data[train_idx,]
test_data <- total_data[-train_idx,]

# 패키지 설치
#install.packages("randomForest")
library(randomForest)

# train 데이터 100개 인덱스 생성
train_idx_matrix <- matrix(0,nrow = 100, ncol = 0.8*n)

for(seed in 1:100) {
  set.seed(seed)
  train_idx_matrix[seed,] <- sample(n, 0.8*n, replace = FALSE)
}


# 결과를 저장 할 3중 배열 선언
# 15개의 모델, 4개의 결과(train test의 mse, r^2), 100개의 데이터
result_matrix <- array(0, dim = c(15, 4, 100))

# 모델링
model_idx = 0
for(ntree in seq(100, 500, by = 200)) { # 3개 ntree = 100, 300, 500
  for(mtry in seq(11, 55, by = 11)) { # 5개 mtry = 11, 22, 33 ,44 ,55
    
    model_idx <- model_idx + 1
    
    # 모델링 시간 측정
    start_time = Sys.time()
    rf_model <- randomForest(salary ~ .,data = train_data, ntree = ntree, mtry = mtry)
    fit_time = Sys.time() - start_time
    cat("ntree = ", ntree, ", mtry = ", mtry, "fit time = ", fit_time,"s\n")
    
    #100개 데이터에 대해 평가
    for(seed in 1:100){
      train_data <- total_data[train_idx_matrix[seed,],]
      test_data <- total_data[-train_idx_matrix[seed,],]
      train_pred <- predict(rf_model, newdata = train_data)
      test_pred <- predict(rf_model, newdata = test_data)
      
      result_matrix[model_idx,1,] <- mse(train_data$salary, train_pred)
      result_matrix[model_idx,2,] <- mse(test_data$salary, test_pred)
      result_matrix[model_idx,3,] <- R2(train_data$salary, train_pred)
      result_matrix[model_idx,4,] <- R2(test_data$salary, test_pred)
    }
    
    cat("Average Train MSE:", mean(result_matrix[model_idx,1,]), "\n",
        "Average Test MSE:", mean(result_matrix[model_idx,2,]), "\n",
        "Average Train R^2:", mean(result_matrix[model_idx,3,]), "\n",
        "Average Test R^2:", mean(result_matrix[model_idx,4,]), "\n")
  }
}
