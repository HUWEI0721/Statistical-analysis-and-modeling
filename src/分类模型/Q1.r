# 加载所需的库
library(tidyverse)


# 1.1 学历背景分布
ggplot(employee_data, aes(x = "", fill = Education)) +
  geom_bar(width = 1, stat = "count") +         
  coord_polar(theta = "y") +                
  ggtitle("员工学历背景分布") +                  
  theme_void() +                                
  theme(axis.text.x = element_blank()) +        
  labs(fill = "学历背景") +                     
  scale_fill_brewer(palette = "Set3") +          
  geom_text(aes(label = paste0(round((..count..)/sum(..count..)*100, 1), "%\n", ..count..)), 
            stat = "count", position = position_stack(vjust = 0.5), size = 5) 


# 1.2 不同城市的员工服务年限差异（箱线图）
# 计算服务年限
employee_data$ServiceYears <- 2024 - employee_data$JoiningYear
ggplot(employee_data, aes(x = City, y = ServiceYears)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("不同城市的员工服务年限差异") +
  xlab("城市") +
  ylab("服务年限") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 进行ANOVA（方差分析）检验不同城市间服务年限的差异是否显著
anova_result <- aov(ServiceYears ~ City, data = employee_data)
summary(anova_result)



# 1.3 薪资等级与当前领域经验的关系
# 进行方差分析，检验不同薪资等级之间的领域经验差异是否显著
anova_result <- aov(ExperienceInCurrentDomain ~ PaymentTier, data = employee_data)
summary(anova_result)








