### 2. 各变量平稳性及随机性检验
library(data.table)
library(forecast)
library(TSA)
library(aTSA)
library(tseries)

# 读取数据
train <- fread("../temp/Train.csv")
test_data <- fread("../temp/Text.csv")

# 处理时间列
train$Date_Time <- as.POSIXct(train$Date_Time, format = "%d.%m.%Y %H:%M:%S")
test_data$Date_Time <- as.POSIXct(test_data$Date_Time, format = "%d.%m.%Y %H:%M:%S")

# 自动识别所有特征列
feature_names <- setdiff(names(train), "Date_Time")

# 绘制所有数值型特征的时间序列图
numeric_columns <- sapply(train[, ..feature_names], is.numeric)
numeric_features <- feature_names[numeric_columns]

par(mfrow = c(2, 2))
for (col in numeric_features) {
  plot(train$Date_Time, train[[col]],
       type = "l",
       col = sample(colors(), 1),
       lwd = 1,
       main = col,
       ylab = "Value",
       xlab = "Date_Time")
  grid(col = "#B0B0B0")
}
par(mfrow = c(1,1))

# 对所有数值型变量进行ADF单位根检验
cat("\nADF单位根检验结果：\n")
adf_results <- lapply(numeric_features, function(col) {
  res <- adf.test(train[[col]])
  cat(col, ": p-value =", res$p.value, "\n")
  return(res$p.value)
})
names(adf_results) <- numeric_features

# 纯随机性检验（Ljung-Box）
cat("\nLjung-Box检验结果：\n")
for (col in numeric_features) {
  for (k in 1:2) {
    lb <- Box.test(train[[col]], lag = 6 * k, type = "Ljung-Box")
    cat(col, ", lag=", 6*k, ": p-value =", lb$p.value, "\n")
  }
}

