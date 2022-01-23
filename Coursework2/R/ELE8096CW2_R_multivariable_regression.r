# package
# -------------------------------------------
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
library(car)
library(leaps)
Sys.setlocale("LC_TIME", "English")

# data from data.csv
# -------------------------------------------
data <- data.frame(read.csv(file="C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/data.csv", encoding="UTF-8"))
data <- data[, -1]

# 用scatterplotMatrix函数画出各变量间的散点图：
# scatterplotMatrix(data, lty.smooth = 2, spread = FALSE, main = 'Scatter Plot Matrix')

# 多元线性回归模型
fit <- lm(PM2.5 ~ NO2 + O3 + temperature + humidity, data = data)
summary(fit)
lm(formula = PM2.5 ~ NO2 + O3 + temperature + humidity, data = data)

# 假设检验
# 1、正态性
# qqPlot(fit, id.method='identify', simulate = TRUE, labels=row.names(data), main = 'Q-Q plot')

# 2、独立性
# 进行D-W检验：
# durbinWatsonTest(fit)

# 3、线性关系
# 绘制成分残差图：
# crPlots(fit)

# 4、同方差性
# ncvTest(fit)

# 四、选择最佳模型
# 全子集回归---通过调整R平方决定最佳模型:
leaps <- regsubsets(PM2.5 ~ NO2 + O3 + temperature + humidity, data = data, nbest = 4)
plot(leaps, scale = 'adjr2')

# 全子集回归---通过Mallows Cp统计量决定最佳模型:
# subsets(leaps, statistic = 'cp', main = 'Cp plot for all subsets regression')
# abline(1, 1, lty = 2, col = 'blue')
