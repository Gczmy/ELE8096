# package
# -------------------------------------------
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(RColorBrewer)
Sys.setlocale("LC_TIME", "English")

# data from data.csv
# -------------------------------------------
data <- data.frame(read.csv(file="C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/data.csv", encoding="UTF-8"))

# parameters
# -------------------------------------------
Date <- dmy(data[, "Date"])
PM25 <- data[, "PM2.5"]
NO2 <- data[, "NO2"]
O3 <- data[, "O3"]
temperature <- data[, "temperature"]
humidity <- data[, "humidity"]

# mean
NO2_mean <- mean(NO2[1:10])
for( i in 1:18) {
    NO2_mean <- append(NO2_mean, mean(NO2[(24*(i-1)+11):(24*(i-1)+11+23)]))
}
NO2_mean <- append(NO2_mean, mean(NO2[443:452]))
# print(NO2_mean)

# median
NO2_median <- median(NO2[1:10])
for( i in 1:18) {
    NO2_median <- append(NO2_median, median(NO2[(24*(i-1)+11):(24*(i-1)+11+23)]))
}
NO2_median <- append(NO2_median, median(NO2[443:452]))
# print(NO2_median)

# Date 去掉重复的日期，只保留单独的每天
Date_less <- as.Date(c("2018-10-18"), "%Y-%m-%d")
for (i in 1:18) {
    Date_less <- append(Date_less, Date[24 * (i - 1) + 11])
}
Date_less <- append(Date_less, "2018-11-06")
# print(Date_less)

# Linear Regression
# -------------------------------------------
# predict
# predict_days <- 18
# Date_predict <- structure(c(17842:(17842 + predict_days - 1)), class = "Date")
# Date_less <- append(Date_less, Date_predict)
# Date.numeric <- as.numeric(Date_less) / (24 * 60 * 60) # 将日期时间格式转化成数值格式
# relation <- lm(NO2_median[1:20]~Date.numeric[1:20]) # 线性回归函数
# # print(Date.numeric)
# # print(summary(relation))

# # 预测之后x天的NO2值
# # a <- data.frame(Date.numeric <- Date.numeric[21:(predict_days + 20)])
# # result <- predict(relation, a) # 预测结果
# a <- data.frame(170)
# result <- predict(relation, a) # 预测结果
# # for (i in 1:predict_days-1) {
# #     a <- data.frame(Date.numeric <- Date.numeric[i + 21])
# #     result <- append(result, predict(relation, a)) # 预测结果
# # }
# print(result)

# plot
# PM25
# -------------------------------------------
# par(mfrow=c(2,2))
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/PM25_Date_Regression.pdf",width=8,height=8)
plot(Date, PM25, col = "blue", main = "PM2.5 Date Regression",
abline(lm(PM25 ~ Date)), cex = 1.3, pch = 16,
xlab = "Date from 18/10/2018 to 06/11/2018", ylab = "PM2.5")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/PM25_O3_Regression.pdf",width=8,height=8)
plot(O3, PM25, col = "red", main = "PM2.5 O3 Regression",
abline(lm(PM25 ~ O3)), cex = 1.3, pch = 16, xlab = "O3", ylab = "PM2.5")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/PM25_NO2_Regression.pdf",width=8,height=8)
plot(NO2, PM25, col = "#0a9233", main = "PM2.5 NO2 Regression",
abline(lm(PM25 ~ NO2)), cex = 1.3, pch = 16, xlab = "NO2", ylab = "PM2.5")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/PM25_temperature_Regression.pdf",width=8,height=8)
plot(temperature, PM25, col = "#c0bd1e", main = "PM2.5 temperature Regression",
abline(lm(PM25 ~ temperature)), cex = 1.3, pch = 16, xlab = "temperature", ylab = "PM2.5")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/PM25_humidity_Regression.pdf",width=8,height=8)
plot(humidity, PM25, col = "#f0960f", main = "PM2.5 humidity Regression",
abline(lm(PM25 ~ humidity)), cex = 1.3, pch = 16, xlab = "humidity", ylab = "PM2.5")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/PM25_box.pdf",width=8,height=8)
boxplot(O3,  main = "PM2.5") # 箱线图
dev.off()

# NO2
# -------------------------------------------
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/NO2_Date_Regression.pdf",width=8,height=8)
plot(Date, NO2, col = "blue", main = "NO2 Date Regression",
abline(lm(NO2 ~ Date)), cex = 1.3, pch = 16,
xlab = "Date from 18/10/2018 to 06/11/2018", ylab = "NO2")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/NO2_O3_Regression.pdf",width=8,height=8)
plot(O3, NO2, col = "red", main = "NO2 O3 Regression",
abline(lm(NO2 ~ O3)), cex = 1.3, pch = 16, xlab = "O3", ylab = "NO2")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/NO2_PM25_Regression.pdf",width=8,height=8)
plot(PM25, NO2, col = "#0a9233", main = "NO2 PM2.5 Regression",
abline(lm(NO2 ~ PM25)), cex = 1.3, pch = 16, xlab = "PM2.5", ylab = "NO2")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/NO2_temperature_Regression.pdf",width=8,height=8)
plot(temperature, NO2, col = "#c0bd1e", main = "NO2 temperature Regression",
abline(lm(NO2 ~ temperature)), cex = 1.3, pch = 16, xlab = "temperature", ylab = "NO2")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/NO2_humidity_Regression.pdf",width=8,height=8)
plot(humidity, NO2, col = "#f0960f", main = "NO2 humidity Regression",
abline(lm(NO2 ~ humidity)), cex = 1.3, pch = 16, xlab = "humidity", ylab = "NO2")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/NO2_box.pdf",width=8,height=8)
boxplot(NO2,  main= "NO2") # 箱线图
dev.off()

# O3
# -------------------------------------------
pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/O3_Date_Regression.pdf",width=8,height=8)
plot(Date, O3, col = "blue", main = "O3 Date Regression",
abline(lm(O3 ~ Date)), cex = 1.3, pch = 16,
xlab = "Date from 18/10/2018 to 06/11/2018", ylab = "O3")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/O3_NO2_Regression.pdf",width=8,height=8)
plot(NO2, O3, col = "red", main = "O3 NO2 Regression",
abline(lm(O3 ~ NO2)), cex = 1.3, pch = 16, xlab = "NO2", ylab = "O3")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/O3_PM25_Regression.pdf",width=8,height=8)
plot(PM25, O3, col = "#0a9233", main = "O3 PM2.5 Regression",
abline(lm(O3 ~ PM25)), cex = 1.3, pch = 16, xlab = "PM2.5", ylab = "O3")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/O3_temperature_Regression.pdf",width=8,height=8)
plot(temperature, O3, col = "#c0bd1e", main = "O3 temperature Regression",
abline(lm(O3 ~ temperature)), cex = 1.3, pch = 16, xlab = "temperature", ylab = "O3")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/O3_humidity_Regression.pdf",width=8,height=8)
plot(humidity, O3, col = "#f0960f", main = "O3 humidity Regression",
abline(lm(O3 ~ humidity)), cex = 1.3, pch = 16, xlab = "humidity", ylab = "O3")
dev.off()

pdf("C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/figures/O3_box.pdf",width=8,height=8)
boxplot(O3,  main = "O3") # 箱线图
dev.off()
# mean
# -------------------------------------------
# x = 1:18
# plot(x,NO2_mean[1:20],col = "blue",main = "NO2 Regression",
# abline(lm(NO2_mean[1:20]~x)),cex = 1.3,pch = 16,xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")

# median
# # -------------------------------------------
# plot(Date_less[1:20], NO2_median[1:20], col = "blue",
# abline(lm(NO2_median[1:20]~Date_less[1:20])), cex = 1.3, pch = 16, xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")
