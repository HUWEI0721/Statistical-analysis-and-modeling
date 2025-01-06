# 加载数据
employee_data <- read.csv("employee.csv")

# 转变类型
employee_data$Education <- factor(employee_data$Education, 
                                  levels = c("Bachelors", "Masters", "PHD"), 
                                  ordered = TRUE)
employee_data$City <- as.factor(employee_data$City)
employee_data$PaymentTier <- factor(employee_data$PaymentTier, 
                                    levels = c(1, 2, 3), 
                                    ordered = TRUE)
employee_data$Gender <- as.factor(employee_data$Gender)
employee_data$EverBenched <- factor(employee_data$EverBenched, 
                                    levels = c("No", "Yes"), 
                                    ordered = TRUE)
employee_data$LeaveOrNot <- as.factor(employee_data$LeaveOrNot)

employee_data$JoiningYear <- as.numeric(employee_data$JoiningYear)
employee_data$Age <- as.numeric(employee_data$Age)
employee_data$ExperienceInCurrentDomain <- as.numeric(employee_data$ExperienceInCurrentDomain)


employee_data <- na.omit(employee_data)  # 删除缺失值的行

