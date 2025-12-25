### 5. ARIMAX回归分析xxooxxo
library(data.table)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

# 读取数据
train <- fread("../temp/Train.csv")
test_data <- fread("../temp/Text.csv")

# 处理时间列
train$Date_Time <- as.POSIXct(train$Date_Time, format = "%d.%m.%Y %H:%M:%S")
test_data$Date_Time <- as.POSIXct(test_data$Date_Time, format = "%d.%m.%Y %H:%M:%S")

# 自动识别所有特征列（排除时间列）
feature_names <- setdiff(names(train), "Date_Time")

# 选择建模目标变量
target_col <- "T_degC"

# 每天采样一次（取每天的第一个观测）
train_day <- train %>%
  arrange(Date_Time) %>%
  mutate(Date = as.Date(Date_Time)) %>%
  group_by(Date) %>%
  slice(1) %>%
  ungroup()
test_data_day <- test_data %>%
  arrange(Date_Time) %>%
  mutate(Date = as.Date(Date_Time)) %>%
  group_by(Date) %>%
  slice(1) %>%
  ungroup()

cat("降采样后训练集行数：", nrow(train_day), "\n")
cat("降采样后测试集行数：", nrow(test_data_day), "\n")

# 频率设置：每天一次，一年frequency=365
freq <- 365

# 构建目标序列和外生变量
exog_cols <- setdiff(feature_names, target_col)
if (length(exog_cols) < 1) stop("未找到外生变量列！")

ts_train <- ts(train_day[[target_col]], frequency = freq)
ts_test <- ts(test_data_day[[target_col]])
h <- length(ts_test)
# 强制外生变量为数值型矩阵
xreg_train <- as.matrix(sapply(train_day[, exog_cols, drop=FALSE], as.numeric))
xreg_test <- as.matrix(sapply(test_data_day[, exog_cols, drop=FALSE], as.numeric))
# 检查NA
cat("训练集外生变量NA数量：", sum(is.na(xreg_train)), "\n")
cat("测试集外生变量NA数量：", sum(is.na(xreg_test)), "\n")

# 检查目标序列平稳性
cat("目标序列ADF检验：\n")
print(adf.test(ts_train))

diff_ts_train <- diff(ts_train)
cat("一阶差分后ADF检验：\n")
print(adf.test(diff_ts_train))

# ARIMAX建模
arimax_model <- auto.arima(ts_train, xreg = xreg_train, seasonal = TRUE, max.p=2, max.q=2, max.P=1, max.Q=1, stepwise=TRUE, approximation=TRUE)
print(arimax_model)
cat("ARIMAX模型参数显著性：\n")
print(summary(arimax_model))

# 预测
yhat <- forecast(arimax_model, xreg = xreg_test, h = h)

# 计算MSE
mse <- mean((as.numeric(ts_test) - as.numeric(yhat$mean))^2)
cat(paste("ARIMAX测试集MSE:", mse, "\n"))

# 可视化数据准备
times_train <- train_day$Date
times_test <- test_data_day$Date
arimax_forecast_data <- data.frame(Date = times_test, 
                                   Mean = as.numeric(yhat$mean),
                                   Lower = as.numeric(yhat$lower[,2]),
                                   Upper = as.numeric(yhat$upper[,2]),
                                   Observed = as.numeric(ts_test))
train_data <- data.frame(Date = times_train, Temp = as.numeric(ts_train))

custom_colors <- c("Training Data" = "black", 
                   "Test Data" = "#1b9e77", 
                   "ARIMAX Forecast" = "#d95f02", 
                   "ARIMAX CI" = "#d95f02")

ggplot() +
  geom_line(data = train_data, aes(x = Date, y = Temp, color = "Training Data"), size = 1) +
  geom_line(data = arimax_forecast_data, aes(x = Date, y = Observed, color = "Test Data"), size = 1) +
  geom_line(data = arimax_forecast_data, aes(x = Date, y = Mean, color = "ARIMAX Forecast"), size = 1) +
  geom_ribbon(data = arimax_forecast_data, aes(x = Date, ymin = Lower, ymax = Upper, fill = "ARIMAX CI"), alpha = 0.2) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors["ARIMAX CI"]) +
  labs(title = "ARIMAX模型预测", x = "时间", y = target_col) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top") 