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

# 自动识别所有特征列
feature_names <- setdiff(names(train), "Date_Time")
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

freq <- 365
exog_cols <- setdiff(feature_names, target_col)
if (length(exog_cols) < 1) stop("未找到外生变量列！")

ts_train <- ts(train_day[[target_col]], frequency = freq)
ts_test <- ts(test_data_day[[target_col]])
h <- length(ts_test)
xreg_train <- as.matrix(sapply(train_day[, exog_cols, drop=FALSE], as.numeric))
xreg_test <- as.matrix(sapply(test_data_day[, exog_cols, drop=FALSE], as.numeric))

# 构造正余弦季节性外生变量
n_train <- nrow(train_day)
n_test <- nrow(test_data_day)
season_cos_train <- cos(2 * pi * (1:n_train) / freq)
season_sin_train <- sin(2 * pi * (1:n_train) / freq)
season_cos_test <- cos(2 * pi * (n_train + 1):(n_train + n_test) / freq)
season_sin_test <- sin(2 * pi * (n_train + 1):(n_train + n_test) / freq)

xreg_train2 <- cbind(xreg_train, season_cos_train, season_sin_train)
xreg_test2 <- cbind(xreg_test, season_cos_test, season_sin_test)

# 建模
sarimax_model <- Arima(
  ts_train,
  order = c(2, 0, 1),
  xreg = xreg_train2
)
print(sarimax_model)
checkresiduals(sarimax_model)

# 预测
yhat <- forecast(sarimax_model, xreg = xreg_test2, h = h)

# 可视化
times_train <- train_day$Date
times_test <- test_data_day$Date
sarimax_forecast_data <- data.frame(Date = times_test, 
                                    Mean = as.numeric(yhat$mean),
                                    Lower = as.numeric(yhat$lower[,2]),
                                    Upper = as.numeric(yhat$upper[,2]))
train_data <- data.frame(Date = times_train, Temp = as.numeric(ts_train))
test_data_plot <- data.frame(Date = times_test, Temp = as.numeric(ts_test))

custom_colors <- c("Training Data" = "black", 
                   "Test Data" = "#1b9e77", 
                   "SARIMAX Forecast" = "#d95f02", 
                   "SARIMAX CI" = "#d95f02")

ggplot() + 
  geom_line(data = train_data, aes(x = Date, y = Temp, color = "Training Data"), size = 1) + 
  geom_line(data = test_data_plot, aes(x = Date, y = Temp, color = "Test Data"), size = 1) + 
  geom_line(data = sarimax_forecast_data, aes(x = Date, y = Mean, color = "SARIMAX Forecast"), size = 1) + 
  geom_ribbon(data = sarimax_forecast_data, aes(x = Date, ymin = Lower, ymax = Upper, fill = "SARIMAX CI"), alpha = 0.2) +
  labs(title = "SARIMAX模型预测", x = "时间", y = target_col) +
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

# 计算MSE
sarimax_predicted_values <- as.numeric(yhat$mean[1:h])
sarimax_mse <- mean((as.numeric(ts_test) - sarimax_predicted_values)^2)
cat(paste("SARIMAX MSE:", sarimax_mse, "\n")) 