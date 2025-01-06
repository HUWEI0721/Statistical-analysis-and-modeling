# 加载必要的库
library(tseries)  # 包含 ADF 检验函数
library(forecast) # 包含差分与模型功能
library(stats)    # 包含 Ljung-Box 检验

# 读取数据并转为时间序列
sales <- read.csv("D:/sales.csv")
sales$datesold <- as.Date(sales$datesold, format = "%Y-%m-%d")

# 按月聚合房价数据（假设使用总房价时间序列）
monthly_price <- sales %>%
  mutate(month = floor_date(datesold, "month")) %>%
  group_by(month) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# 创建时间序列对象
price_ts <- ts(monthly_price$mean_price, start = c(2007, 1), frequency = 12) # 以2007年为起点，频率为12（按月）

# 平稳性检验：ADF 检验
adf_result <- adf.test(price_ts, alternative = "stationary")
print("ADF 检验结果：")
print(adf_result)

# 如果数据不平稳，尝试对数变换和差分
if (adf_result$p.value > 0.05) {  # 检查 p 值是否大于 0.05（不平稳）
  print("数据不平稳，进行对数变换与差分处理")
  
  # 对数变换
  price_ts_log <- log(price_ts)
  
  # 差分（使平稳）
  price_ts_diff <- diff(price_ts_log)
  
  # 检查差分后序列的平稳性
  adf_diff_result <- adf.test(price_ts_diff, alternative = "stationary")
  print("对数变换 + 差分后 ADF 检验结果：")
  print(adf_diff_result)
  
  # 更新使用的时间序列
  price_ts <- price_ts_diff
} else {
  print("数据已平稳，无需变换或差分")
}

# 纯随机性检验：Ljung-Box 检验
ljung_box_result <- Box.test(price_ts, lag = 20, type = "Ljung-Box") # 设定滞后阶数为 20
print("Ljung-Box 检验结果：")
print(ljung_box_result)

if (ljung_box_result$p.value < 0.05) {
  print("时间序列存在显著的自相关性，非纯随机序列")
} else {
  print("时间序列是纯随机序列")
}
