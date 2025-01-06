# 加载 bnlearn 包
library(bnlearn)

# 选择用于构建模型的特征
employee_data_subset <- employee_data


# 数据分割（70% 训练集，30% 测试集）
set.seed(123)  # 设置随机种子以保证结果可复现
train_indices <- sample(1:nrow(employee_data_subset), size = 0.7 * nrow(employee_data_subset))
train_data <- employee_data_subset[train_indices, ]
test_data <- employee_data_subset[-train_indices, ]

# 使用训练数据学习贝叶斯网络
model <- bnlearn::hc(train_data)
fitted_model <- bnlearn::bn.fit(model, data = train_data)

# 对测试数据进行预测
predictions <- predict(fitted_model, test_data, node = "LeaveOrNot")

# 计算混淆矩阵
conf_matrix <- table(Predicted = predictions, Actual = test_data$LeaveOrNot)

# 打印混淆矩阵
print(conf_matrix)

# 计算模型的准确率、精度、召回率和 F1 分数
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# 打印评估结果
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


