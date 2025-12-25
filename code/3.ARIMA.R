# 3_1.ARIMA建模
library(data.table)
library(forecast)
library(TSA)
library(tseries)
library(ggplot2)

# 读取数据
train <- fread("../temp/Train.csv")
test_data_2016 <- fread("../temp/Text.csv")

# 处理时间列
train$Date_Time <- as.POSIXct(train$Date_Time, format = "%d.%m.%Y %H:%M:%S")
test_data_2016$Date_Time <- as.POSIXct(test_data_2016$Date_Time, format = "%d.%m.%Y %H:%M:%S")

# 自动识别所有特征列（排除时间列）
feature_names <- setdiff(names(train), "Date_Time")

#### 选择建模目标变量
####
target_col <- "T_degC"

# 创建时间序列对象（假设数据为等间隔，频率可根据实际调整）
ts_train <- ts(train[[target_col]])

# 平稳性检验与差分
adf_res <- adf.test(ts_train)
cat("原序列ADF检验p值：", adf_res$p.value, "\n")
if (adf_res$p.value > 0.05) {
  ts_train_diff <- diff(ts_train)
  adf_res2 <- adf.test(ts_train_diff)
  cat("一阶差分后ADF检验p值：", adf_res2$p.value, "\n")
} else {
  ts_train_diff <- ts_train
}

# 模型定阶：ACF/PACF图
par(mfrow = c(1,2))
acf(ts_train_diff, main = paste(target_col, "ACF"))
pacf(ts_train_diff, main = paste(target_col, "PACF"))
par(mfrow = c(1,1))

# 自动定阶与拟合
fit_auto <- auto.arima(ts_train, ic = "bic")
cat("auto.arima模型：\n")
print(fit_auto)

# 也可手动指定阶数（如ARIMA(1,1,1)）
fit_manual <- arima(ts_train, order = c(1,1,1))
cat("手动ARIMA(1,1,1)模型：\n")
print(fit_manual)

# 比较AIC/BIC
cat("AIC/BIC比较：\n")
print(data.frame(AIC_auto = AIC(fit_auto), BIC_auto = BIC(fit_auto),
                 AIC_manual = AIC(fit_manual), BIC_manual = BIC(fit_manual)))

# 残差诊断
tsdiag(fit_auto)

# 预测
h <- nrow(test_data_2016)
fc <- forecast:::forecast.Arima(fit_auto, h = h)

# 生成预测时间序列，与测试集时间对齐
start_time <- max(train$Date_Time)
interval <- as.numeric(difftime(train$Date_Time[2], train$Date_Time[1], units = "secs"))
fc_times <- seq(from = start_time + interval, by = interval, length.out = h)

# 可视化
plot(fc, main = paste("ARIMA预测 -", target_col), xlab = "Time", ylab = target_col)
lines(test_data_2016$Date_Time, test_data_2016[[target_col]], col = "blue", lwd = 2)
legend("topright", legend = c("预测", "测试集实测"), col = c("red", "blue"), lty = 1)

# 计算MSE
mse <- mean((test_data_2016[[target_col]][1:h] - fc$mean[1:h])^2, na.rm = TRUE)
cat("测试集MSE：", mse, "\n")

# ggplot可视化
train_df <- data.frame(Date = train$Date_Time, Value = train[[target_col]])
test_df <- data.frame(Date = test_data_2016$Date_Time, Value = test_data_2016[[target_col]])
fc_df <- data.frame(Date = fc_times,
                    Mean = as.numeric(fc$mean),
                    Lower = as.numeric(fc$lower[,2]),
                    Upper = as.numeric(fc$upper[,2]))

ggplot() +
  geom_line(data = train_df, aes(x = Date, y = Value), color = "black") +
  geom_line(data = test_df, aes(x = Date, y = Value), color = "blue") +
  geom_line(data = fc_df, aes(x = Date, y = Mean), color = "red") +
  geom_ribbon(data = fc_df, aes(x = Date, ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
  labs(title = paste(target_col, "ARIMA预测"), x = "时间", y = target_col) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 