# 4. SARIMA与Holt-Winters建模与预测
library(data.table)
library(forecast)
library(tseries)
library(ggplot2)
library(viridis)
library(dplyr)

# 读取数据
train <- fread("../temp/Train.csv")
test_data <- fread("../temp/Text.csv")

# 处理时间列
train$Date_Time <- as.POSIXct(train$Date_Time, format = "%d.%m.%Y %H:%M:%S")
test_data$Date_Time <- as.POSIXct(test_data$Date_Time, format = "%d.%m.%Y %H:%M:%S")

# 自动识别所有特征列（排除时间列）
feature_names <- setdiff(names(train), "Date_Time")

# 选择建模目标变量（以温度T_degC为例）
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

# 创建时间序列对象
n_train <- nrow(train_day)
if (n_train < freq*2) stop("降采样后数据量过少，无法进行季节性分解和建模！")
ts_train <- ts(train_day[[target_col]], frequency = freq)
ts_test <- ts(test_data_day[[target_col]])
h <- length(ts_test)

# 时间序列分解
if (n_train > freq*2) {
  decomposed_series <- decompose(ts_train)
  print(decomposed_series)
  par(mfrow=c(2,2))
  plot(decomposed_series$figure, main="Seasonal Figure")
  plot(decomposed_series$trend, main="Trend")
  plot(decomposed_series$random, main="Random")
  plot(decomposed_series$seasonal, main="Seasonal")
  par(mfrow=c(1,1))
}

# SARIMA建模
sarima_model <- auto.arima(ts_train, seasonal = TRUE, max.p=2, max.q=2, max.P=1, max.Q=1, stepwise=TRUE, approximation=TRUE)
print(sarima_model)

# SARIMA预测
sarima_forecast <- forecast::forecast(sarima_model, h = h)

# Holt-Winters建模
hw_model <- HoltWinters(ts_train)
print(hw_model)
hw_forecast <- forecast::forecast(hw_model, h = h)

# 计算MSE
sarima_predicted_values <- sarima_forecast$mean[1:h]
hw_predicted_values <- hw_forecast$mean[1:h]
sarima_mse <- mean((ts_test - sarima_predicted_values)^2)
hw_mse <- mean((ts_test - hw_predicted_values)^2)
cat(paste("SARIMA MSE:", sarima_mse, "\n"))
cat(paste("Holt-Winters MSE:", hw_mse, "\n"))

# 可视化数据准备
times_train <- train_day$Date
times_test <- test_data_day$Date
sarima_forecast_data <- data.frame(Date = times_test, 
                                   Mean = as.numeric(sarima_forecast$mean),
                                   Lower = as.numeric(sarima_forecast$lower[,2]),
                                   Upper = as.numeric(sarima_forecast$upper[,2]))
hw_forecast_data <- data.frame(Date = times_test, 
                               Mean = as.numeric(hw_forecast$mean),
                               Lower = as.numeric(hw_forecast$lower[,2]),
                               Upper = as.numeric(hw_forecast$upper[,2]))
train_data <- data.frame(Date = times_train, Temp = as.numeric(ts_train))
test_data_plot <- data.frame(Date = times_test, Temp = as.numeric(ts_test))

# 自定义颜色
custom_colors <- c("Training Data" = "black", 
                   "Test Data" = "#1b9e77", 
                   "SARIMA Forecast" = "#d95f02", 
                   "Holt-Winters Forecast" = "#7570b3", 
                   "SARIMA CI" = "#d95f02", 
                   "Holt-Winters CI" = "#7570b3")

# 绘制图形比较SARIMA和Holt-Winters模型的预测结果
ggplot() + 
  geom_line(data = train_data, aes(x = Date, y = Temp, color = "Training Data"), size = 1) + 
  geom_line(data = test_data_plot, aes(x = Date, y = Temp, color = "Test Data"), size = 1) + 
  geom_line(data = sarima_forecast_data, aes(x = Date, y = Mean, color = "SARIMA Forecast"), size = 1) + 
  geom_ribbon(data = sarima_forecast_data, aes(x = Date, ymin = Lower, ymax = Upper, fill = "SARIMA CI"), alpha = 0.2) +
  geom_line(data = hw_forecast_data, aes(x = Date, y = Mean, color = "Holt-Winters Forecast"), size = 1) + 
  geom_ribbon(data = hw_forecast_data, aes(x = Date, ymin = Lower, ymax = Upper, fill = "Holt-Winters CI"), alpha = 0.2) +
  labs(title = paste(target_col, "SARIMA与Holt-Winters预测(每天采样)"), x = "时间", y = target_col) +
  theme_minimal(base_size = 15) + 
  scale_color_manual(values = custom_colors) + 
  scale_fill_manual(values = custom_colors) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  theme(legend.position = "top") + 
  guides(color = guide_legend(title = "Legend", title.position = "top"), 
         fill = guide_legend(title = "Confidence Intervals", title.position = "top")) +
  theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  theme(panel.grid.major = element_line(color = "gray", size = 0.5), 
        panel.grid.minor = element_line(color = "gray", size = 0.25)) 