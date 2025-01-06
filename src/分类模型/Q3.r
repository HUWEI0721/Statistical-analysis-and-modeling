
# 数据分割（70% 训练集，30% 测试集）
set.seed(123)  # 设置随机种子以保证结果可复现
train_indices <- sample(1:nrow(employee_data), size = 0.7 * nrow(employee_data))
train_data <- employee_data[train_indices, ]
test_data <- employee_data[-train_indices, ]

# 构建逻辑回归模型
logit_model <- glm(LeaveOrNot ~ Education + City + PaymentTier + Age + ExperienceInCurrentDomain + Gender + EverBenched + JoiningYear, 
                   data = train_data, family = binomial)

# 对测试集进行预测（预测离职的概率）
pred_probs <- predict(logit_model, test_data, type = "response")

# 将概率转换为预测标签（阈值0.3）
predictions <- ifelse(pred_probs > 0.3, 1, 0)

# 计算混淆矩阵
conf_matrix <- table(Predicted = predictions, Actual = test_data$LeaveOrNot)

# 打印混淆矩阵
print(conf_matrix)

# 计算准确率、精度、召回率和 F1 分数
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# 打印评估结果
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

summary(logit_model)

