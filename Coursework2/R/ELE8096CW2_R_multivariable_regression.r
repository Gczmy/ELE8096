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
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_scatterplotMatrix.pdf",width=8,height=8)
scatterplotMatrix(data, lty.smooth = 2, spread = FALSE, main = 'Scatter Plot Matrix')
dev.off()

# 多元线性回归模型
fit_PM25 <- lm(PM2.5 ~ NO2 + O3 + temperature + humidity, data = data)
summary(fit_PM25)
lm(formula = PM2.5 ~ NO2 + O3 + temperature + humidity, data = data)

fit_NO2 <- lm(NO2 ~ PM2.5 + O3 + temperature + humidity, data = data)
summary(fit_NO2)

fit_O3 <- lm(O3 ~ PM2.5 + NO2 + temperature + humidity, data = data)
summary(fit_O3)

# 假设检验
# 1、正态性
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_qqPlot_PM25.pdf",width=8,height=8)
qqPlot(fit_PM25, id.method='identify', simulate = TRUE, labels=row.names(data), main = 'PM2.5 Q-Q plot')
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_qqPlot_NO2.pdf",width=8,height=8)
qqPlot(fit_NO2, id.method='identify', simulate = TRUE, labels=row.names(data), main = 'NO2 Q-Q plot')
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_qqPlot_O3.pdf",width=8,height=8)
qqPlot(fit_O3, id.method='identify', simulate = TRUE, labels=row.names(data), main = 'O3 Q-Q plot')
dev.off()

# 2、独立性
# 进行D-W检验：
# durbinWatsonTest(fit)

# 3、线性关系
# 绘制成分残差图：
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_crPlots_PM25.pdf",width=8,height=8)
crPlots(fit_PM25)
dev.off()
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_crPlots_NO2.pdf",width=8,height=8)
crPlots(fit_NO2)
dev.off()
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_crPlots_O3.pdf",width=8,height=8)
crPlots(fit_O3)
dev.off()

# 4、同方差性
# ncvTest(fit)

# 四、选择最佳模型
# 全子集回归---通过调整R平方决定最佳模型:
leaps <- regsubsets(PM2.5 ~ NO2 + O3 + temperature + humidity, data = data, nbest = 4)
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_adjr2_PM25.pdf",width=8,height=8)
plot(leaps, scale = 'adjr2')
dev.off()

leaps <- regsubsets(NO2 ~ PM2.5 + O3 + temperature + humidity, data = data, nbest = 4)
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_adjr2_NO2.pdf",width=8,height=8)
plot(leaps, scale = 'adjr2')
dev.off()

leaps <- regsubsets(O3 ~ PM2.5 + NO2 + temperature + humidity, data = data, nbest = 4)
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_adjr2_O3.pdf",width=8,height=8)
plot(leaps, scale = 'adjr2')
dev.off()

# 全子集回归---通过Mallows Cp统计量决定最佳模型:
# pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_Cp_plot_all.pdf",width = 8,height = 8)
# subsets(leaps, statistic = 'cp', main = 'Cp plot for all subsets regression')
# abline(1, 1, lty = 2, col = 'blue')
# dev.off()

# 强影响点
cutoff <- 4/(nrow(states-length(fit$coefficients)-2)) #coefficients加上了截距项，因此要多减1
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/mul_reg_adjr2_O3.pdf",width=8,height=8)
plot(fit,which=4,cook.levels = cutoff)
abline(h=cutoff,lty=2,col="red")
dev.off()