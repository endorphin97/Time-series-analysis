library(data.table)

# 读取数据
data <- fread("../data/jena_climate_2009_2016.csv")

# 假设时间列名为 'Date Time'
data$Date <- as.Date(data$`Date_Time`, format = "%d.%m.%Y %H:%M:%S")

# 分割数据
train <- data[data$Date >= "2009-01-01" & data$Date <= "2013-12-31", ]
test  <- data[data$Date >= "2014-01-01" & data$Date <= "2015-12-31", ]
valid <- data[data$Date < "2009-01-01" | data$Date > "2015-12-31", ]

# 保存数据
fwrite(train, "../temp/Train.csv")
fwrite(test,  "../temp/Text.csv")   # 按你的要求命名为Text.csv
fwrite(valid, "../temp/Valid.csv")