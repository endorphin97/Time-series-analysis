# 1. 数据读取与预处理

library(data.table)
library(ggplot2)
library(gridExtra)

# 读取数据
train <- fread("../temp/Train.csv")
test_data <- fread("../temp/Text.csv")

# 处理时间列
train$Date_Time <- as.POSIXct(train$Date_Time, format = "%d.%m.%Y %H:%M:%S")
test_data$Date_Time <- as.POSIXct(test_data$Date_Time, format = "%d.%m.%Y %H:%M:%S")

# 自动识别所有特征列（排除时间列"Date_Time"）
feature_names <- setdiff(names(train), "Date_Time")
cat("自动识别到的特征列：\n")
print(feature_names)

# 查看数据结构和描述性统计
cat("\n训练集结构：\n")
str(train)
cat("\n训练集描述性统计：\n")
print(summary(train))

# 检查缺失值
cat("\n每列缺失值数量：\n")
print(colSums(is.na(train)))

# 数值型特征分布可视化
plot_density <- function(data, column_name) {
  ggplot(data, aes_string(x = column_name)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Density plot of", column_name),
         x = column_name,
         y = "Density") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

# 获取数值型列名
numeric_columns <- sapply(train[, ..feature_names], is.numeric)
numeric_columns <- feature_names[numeric_columns]

# 创建一个列表来存储所有的子图
plots <- list()
for (col in numeric_columns) {
  plots[[col]] <- plot_density(train, col)
}
if (length(plots) > 0) {
  grid.arrange(grobs = plots, ncol = 2)
}

# 箱型图检测异常值
if (length(numeric_columns) > 0) {
  par(mfrow = c(1, min(4, length(numeric_columns))))
  for (col in numeric_columns) {
    boxplot(train[[col]],
            main = paste("Boxplot of", col),
            col = "lightblue", notch = FALSE)
  }
  par(mfrow = c(1,1))
}
