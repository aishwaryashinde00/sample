setwd("~/")
housedata <- read.csv("~/AttD02C.tmp.csv")
data = housedata

## 75% of the sample size
smp_size <- floor(0.8 * nrow(housedata))
print(smp_size)

train_ind <- sample(seq_len(nrow(housedata)), size = smp_size)

train <- housedata[train_ind, ]
test <- housedata[-train_ind, ]
print(train)
relation <- lm((train$price) ~ (train$sqft_living)  + (train$bedrooms)  + train$sqft_lot15 + train$zipcode+ train$yr_built + train$lat + train$yr_renovated  + train$lat +  train$floors + train$waterfront + train$view + train$condition + train$grade + train$long ,train)
cor(train$price,train$sqft_lot15)
plot(housedata$price,housedata$sqft_living)
print(relation)
print(summary(relation))
res_id = residuals(relation)
as.data.frame(res_id)

result <- predict(relation,newdata = test)
print(result)
#rmse <- sqrt( sum( (test$price-result)^2 , na.rm = TRUE ) / nrow(df) )
RMSE <- sqrt(mean(((test$price)-result)^2))
print(RMSE)

