# package
# -------------------------------------------
library(lubridate,warn.conflicts = FALSE)

# data from data.csv
# -------------------------------------------
data <- data.frame(read.csv(file="C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/data.csv", encoding="UTF-8"))

# parameters
# -------------------------------------------
Date <- dmy(data[, "Date"])
PM25 <- data[, "PM2.5"]
NO2 <- data[, "NO2"]
O3 <- data[, "O3"]
temperature <- data[1:5, "temperature"]
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
# # 生成图表
# -------------------------------------------
# plot(Date,NO2,col = "blue",main = "NO2 Regression",
# abline(lm(NO2~Date)),cex = 1.3,pch = 16,xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")

# mean
# -------------------------------------------
# x = 1:18
# plot(x,NO2_mean[1:20],col = "blue",main = "NO2 Regression",
# abline(lm(NO2_mean[1:20]~x)),cex = 1.3,pch = 16,xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")

# mean and predict
# -------------------------------------------
# NO2_mean <- append(NO2_mean, result)
# plot(Date_less, NO2_mean, col = "blue", main = "NO2 Regression",
# abline(lm(NO2_mean[1:20]~Date_less[1:20])), cex = 1.3, pch = 16, xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")

# median
# -------------------------------------------
plot(Date_less[1:20], NO2_median[1:20], col = "blue", main = "NO2 Regression",
abline(lm(NO2_median[1:20]~Date_less[1:20])), cex = 1.3, pch = 16, xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")

# median and predict
# -------------------------------------------
# NO2_median <- append(NO2_median, result)
# plot(Date_less, NO2_median, col = "blue", main = "NO2 Regression",
# abline(lm(NO2_median[1:20]~Date_less[1:20])), cex = 1.3, pch = 16, xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")
