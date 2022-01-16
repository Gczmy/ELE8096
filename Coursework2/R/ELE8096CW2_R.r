# package
# -------------------------------------------
library(lubridate,warn.conflicts = FALSE)

# data from data.csv
# -------------------------------------------
data <- data.frame(read.csv(file="C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/data.csv", encoding="UTF-8"))

# parameters
# -------------------------------------------
Date <- dmy(data[,'Date'])
PM25 <- data[,'PM2.5']
NO2 <- data[,'NO2']
O3 <- data[,'O3']
temperature <- data[1:5,'temperature']
humidity <- data[,'humidity']

# mean
NO2_mean <- mean(NO2[1:10])
for( i in 1:18) {
    NO2_mean <- append(NO2_mean,mean(NO2[(24*(i-1)+11):(24*(i-1)+11+23)]))
    # Date_less <- append(NO2_mean,mean(NO2[(24*(i-1)+11):(24*(i-1)+11+23)]))
}
NO2_mean <- append(NO2_mean,mean(NO2[443:452]))
# NO2_mean[1] <- NULL
print(NO2_mean)
# Linear Regression
# -------------------------------------------
# Date.numeric = as.numeric(Date)/(24*60*60)
# relation <- lm(Date.numeric~NO2)
# print(summary(relation))
# a <- data.frame(NO2 = 120)
# result <-  predict(relation,a)
# print(result)

# plot

# # 生成图表
# plot(Date,NO2,col = "blue",main = "NO2 Regression",
# abline(lm(NO2~Date)),cex = 1.3,pch = 16,xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")

# mean
x = 1:18
plot(x,NO2_mean[2:19],col = "blue",main = "NO2 Regression",
abline(lm(NO2_mean[2:19]~x)),cex = 1.3,pch = 16,xlab = "Date from 18/10/2018 to 06/11/2018",ylab = "NO2")# -------------------------------------------
# 保存 png 图片
pdf(file = "C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/linearregression.pdf")
