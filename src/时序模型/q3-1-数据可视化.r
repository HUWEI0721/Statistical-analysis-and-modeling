# 加载必要的库
library(tidyverse)   # 数据处理与可视化
library(lubridate)   # 日期处理
library(ggplot2)     # 绘图
library(ggthemes)    # 美化主题

# 读取数据
sales <- read.csv("D:/sales.csv")

# 转换日期格式
sales$datesold <- as.Date(sales$datesold, format = "%Y-%m-%d")

# 检查数据结构
str(sales)

# 按月汇总价格数据
monthly_data <- sales %>%
  mutate(month = floor_date(datesold, "month")) %>%
  group_by(month, propertyType) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# 绘制不同房产类型的月平均价格趋势
ggplot(monthly_data, aes(x = month, y = mean_price, color = propertyType)) +
  geom_line(size = 1) +
  labs(
    title = "不同房产类型月平均价格趋势",
    x = "月份",
    y = "平均价格",
    color = "房产类型"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# 可视化邮政编码区域的房价分布
postcode_price <- sales %>%
  group_by(postcode) %>%
  summarise(mean_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))

ggplot(postcode_price, aes(x = reorder(as.factor(postcode), mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "不同邮政编码区域房价分布",
    x = "邮政编码",
    y = "平均价格"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12)
  )

# 按卧室数量绘制价格分布箱线图
ggplot(sales, aes(x = as.factor(bedrooms), y = price, fill = as.factor(bedrooms))) +
  geom_boxplot() +
  labs(
    title = "按卧室数量的房价分布",
    x = "卧室数量",
    y = "价格",
    fill = "卧室数量"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12)
  )
