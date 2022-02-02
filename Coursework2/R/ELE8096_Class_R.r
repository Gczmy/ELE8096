# data from data.csv
# -------------------------------------------
data <- data.frame(read.csv(file="C:/Users/zzc/Desktop/Queen's/ELE8096/Coursework2/R/data.csv", encoding="UTF-8"))

model1 = lm(NO2 ~ O3, data=data)

SSE1 = sum(model1$residuals^2)

model2 = lm(NO2 ~ PM2.5, data=data)

SSE2 = sum(model2$residuals^2)

model3 = lm(NO2 ~ O3 + PM2.5, data=data)

SSE3 = sum(model3$residuals^2)

cor(data$NO2, data$PM2.5)