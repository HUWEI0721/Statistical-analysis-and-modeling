################################################################################
# 加载必要的库
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# 读取数据
data <- read.csv("data.csv")

# 检查数据结构
str(data)

# 检查缺失值
missing_values <- colSums(is.na(data))
missing_values

# 绘制箱线图来检查异常值
b1 <- ggplot(data, aes(x = "", y = Age)) + 
  geom_boxplot() + 
  ggtitle("Age - Boxplot") +
  theme_minimal()

b2 <- ggplot(data, aes(x = "", y = Severity)) + 
  geom_boxplot() + 
  ggtitle("Severity - Boxplot") +
  theme_minimal()

b3 <- ggplot(data, aes(x = "", y = Anxiety)) + 
  geom_boxplot() + 
  ggtitle("Anxiety - Boxplot") +
  theme_minimal()

b4 <- ggplot(data, aes(x = "", y = Satisfaction)) + 
  geom_boxplot() + 
  ggtitle("Satisfaction - Boxplot") +
  theme_minimal()

grid.arrange(b1, b2, b3, b4, ncol = 2)

# 加载 gridExtra 包
library(gridExtra)

# 绘制各个变量的直方图
p1 <- ggplot(data, aes(x = Age)) + 
  geom_histogram(bins = 10, fill = "skyblue", color = "black") + 
  ggtitle("Age Distribution") + 
  theme_minimal()

p2 <- ggplot(data, aes(x = Severity)) + 
  geom_histogram(bins = 10, fill = "lightgreen", color = "black") + 
  ggtitle("Severity Distribution") + 
  theme_minimal()

p3 <- ggplot(data, aes(x = Anxiety)) + 
  geom_histogram(bins = 10, fill = "salmon", color = "black") + 
  ggtitle("Anxiety Distribution") + 
  theme_minimal()

p4 <- ggplot(data, aes(x = Satisfaction)) + 
  geom_histogram(bins = 10, fill = "lightpink", color = "black") + 
  ggtitle("Satisfaction Distribution") + 
  theme_minimal()

# 将所有图形放置到一个大图中
grid.arrange(p1, p2, p3, p4, ncol = 2)

# 描述性统计分析
summary(data)

################################################################################

# 绘制自变量与因变量之间的散点图
p1 <- ggplot(data, aes(x = Age, y = Satisfaction)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Age vs Satisfaction") +
  theme_minimal()

p2 <- ggplot(data, aes(x = Severity, y = Satisfaction)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Severity vs Satisfaction") +
  theme_minimal()

p3 <- ggplot(data, aes(x = Anxiety, y = Satisfaction)) + 
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Anxiety vs Satisfaction") +
  theme_minimal()

# 将散点图组合在一起
grid.arrange(p1, p2, p3, ncol = 2)


# 加载相关的包
library(ggplot2)
library(reshape2)

# 计算自变量之间的相关系数矩阵
cor_matrix <- cor(data[, c("Age", "Severity", "Surg.Med", "Anxiety", "Satisfaction")])

cor_matrix
# 将相关系数矩阵转换为适合ggplot的数据格式
cor_melted <- melt(cor_matrix)

# 绘制相关系数矩阵热力图
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 0)) +
  labs(title = "Correlation Matrix Heatmap") +
  coord_fixed()

# 加载car包计算VIF
library(car)

# 构建初步回归模型
model <- lm(Satisfaction ~ Age + Severity + Surg.Med + Anxiety, data = data)

# 计算VIF
vif(model)

# 绘制残差与拟合值的散点图
model_residuals <- residuals(model)
model_fitted <- fitted(model)

ggplot(data, aes(x = model_fitted, y = model_residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted Values") + 
  theme_minimal()

# 绘制Q-Q图
qqnorm(model_residuals)
qqline(model_residuals, col = "red")

# 进行Shapiro-Wilk正态性检验
shapiro.test(model_residuals)

################################################################################
# 加载必要的包
library(MASS)

# 构建初步回归模型（包括所有自变量）
full_model <- lm(Satisfaction ~ Age + Severity + Surg.Med + Anxiety, data = data)

# 进行逐步回归
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)
stepwise_model1 <- stepAIC(full_model, direction = "forward", trace = FALSE)
stepwise_model2 <- stepAIC(full_model, direction = "backward", trace = FALSE)

# 1. 模型拟合优度：检查R²和调整后的R²
summary(stepwise_model)
summary(stepwise_model1)
summary(stepwise_model2)

# 2. 残差分析
# 提取拟合值和残差
model_residuals <- residuals(stepwise_model)
model_fitted <- fitted(stepwise_model)

# 残差与拟合值的散点图
ggplot(data, aes(x = model_fitted, y = model_residuals)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted Values") + 
  theme_minimal()

# Q-Q图：检验残差的正态性
qqnorm(model_residuals)
qqline(model_residuals, col = "red")

# Shapiro-Wilk正态性检验
shapiro.test(model_residuals)

# 3. 计算方差膨胀因子（VIF）来检查多重共线性
library(car)
vif(stepwise_model)

# 4. 检查回归系数的显著性（t检验结果已经在summary中输出）
summary(stepwise_model)

################################################################################
# 构建新模型，加入Anxiety变量
new_model <- lm(Satisfaction ~ Age + Severity + Anxiety, data = data)

# 查看新的模型摘要
summary(new_model)

# 绘制箱线图，比较内科和外科患者的满意度
ggplot(data, aes(x = factor(Surg.Med), y = Satisfaction, fill = factor(Surg.Med))) + 
  geom_boxplot() + 
  labs(x = "Surgical vs Medical (Surg.Med)", y = "Satisfaction", 
       title = "Satisfaction by Surgical/Medical Patients") +
  scale_x_discrete(labels = c("Medical", "Surgical")) + 
  theme_minimal()

# 进行独立样本t检验，判断两组之间的差异
t_test_result <- t.test(Satisfaction ~ Surg.Med, data = data)

# 输出t检验结果
t_test_result

################################################################################
# 获取预测值
predicted_values <- predict(stepwise_model)

stepwise_model

# 绘制实际值与预测值的比较图
ggplot(data, aes(x = Satisfaction, y = predicted_values)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Actual vs Predicted Satisfaction") +
  xlab("Actual Satisfaction") +
  ylab("Predicted Satisfaction") +
  theme_minimal()
################################################################################


################################################################################
# 小改进
################################################################################
# 加载必要的包
library(ggplot2)
library(MASS)

# 构建包含三角函数项的新模型
new_model_with_trig <- lm(Satisfaction ~ Age + Severity + Surg.Med  + 
                            sin(Anxiety), data = data)

# 查看新模型的摘要
summary(new_model_with_trig)

# 计算方差膨胀因子（VIF）来检查多重共线性
library(car)
vif(new_model_with_trig)

# 绘制残差与拟合值的散点图
model_residuals_trig <- residuals(new_model_with_trig)
model_fitted_trig <- fitted(new_model_with_trig)

ggplot(data, aes(x = model_fitted_trig, y = model_residuals_trig)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted Values (Trig Model)") + 
  theme_minimal()

# 绘制Q-Q图
qqnorm(model_residuals_trig)
qqline(model_residuals_trig, col = "red")

# Shapiro-Wilk正态性检验
shapiro.test(model_residuals_trig)

# 绘制实际值与预测值的比较图
predicted_values_trig <- predict(new_model_with_trig)

ggplot(data, aes(x = Satisfaction, y = predicted_values_trig)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Actual vs Predicted Satisfaction (Trig Model)") +
  xlab("Actual Satisfaction") +
  ylab("Predicted Satisfaction") +
  theme_minimal()
