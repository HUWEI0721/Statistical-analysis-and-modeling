# 加载必要的库
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# 读取CSV文件
setwd("D:/homework/r语言")
sales_data <- read.csv("sales.csv", stringsAsFactors = FALSE)

# 1. 数据预处理

# 检查是否有缺失值并移除含有缺失值的行
sales_data <- na.omit(sales_data)

# 转换datesold为日期格式
sales_data$datesold <- as.Date(sales_data$datesold)

# 添加月份和年份列
sales_data <- sales_data %>%
  mutate(month = floor_date(datesold, "month"),
         year = year(datesold))

# 计算每个月每个房产类型的平均价格
monthly_avg_price <- sales_data %>%
  group_by(year, month, propertyType) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# 2. 使用最优的时间序列模型建模
# 创建一个函数来处理每个组合的时间序列
forecast_model <- function(df) {
  # 创建时间序列对象
  ts_data <- ts(df$avg_price, start = c(min(df$year), min(format(df$month, "%m"))), frequency = 12)
  #print(ts_data)  
  # 分离训练集和测试集
  train_size <- floor(0.8 * length(ts_data))
  train_data <- window(ts_data, end = c(time(ts_data)[train_size], 12))
  test_data <- window(ts_data, start = c(time(ts_data)[train_size + 1], 12))
  
  print(train_data)
  # 自动选择最优ARIMA模型
  fit <- auto.arima(train_data, d = 1, D = 1, seasonal = TRUE)
  
  # 预测测试集
  pred <- forecast(fit, h = length(test_data))
  
  # 计算误差
  accuracy_metrics <- accuracy(pred, test_data)
  
  # 预测未来12个月
  future_forecast <- forecast(fit, h = 12)
  print(future_forecast)
  # 提取预测结果并转换为数据框
  future_df <- data.frame(
    date = seq(max(df$month) + months(1), by = "month", length.out = 12),
    price = future_forecast$mean,
    propertyType = unique(df$propertyType)
  )
  return(list(
    accuracy = accuracy_metrics,
    future_forecast = future_df
  ))
}

# 对每个房产类型应用时间序列建模，并收集评估结果
predictions <- monthly_avg_price %>%
  group_by(propertyType) %>%
  do({
    	result <- forecast_model(.)
	# 3.打印保存评估结果
	current_property_type <- unique(.$propertyType)
	print(current_property_type)
	print(result$accuracy)
      write.csv(result$accuracy,paste(current_property_type,"model_accuracy.csv",collapse=''), row.names = FALSE)
      result$future_forecast
  }) %>%
  ungroup()
# 4. 将预测结果保存到新的CSV文件
write.csv(predictions, "predicted_prices.csv", row.names = FALSE)