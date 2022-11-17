#reading the csv file
data<-read.csv(c("C:/Users/akshg/Desktop/linear-regression-project/Marketing_Data.csv"))
print(data)

set.seed(123)#random sampling

# creating training data as 80% of the dataset
a_sample <- sort(sample(nrow(data),nrow(data)*0.8))

#training data set
training_dataset  <- c(data[a_sample, ])

#testing data set
testing_dataset <- c(data[-a_sample, ])

#making the model
model <-lm(sales~.,data=training_dataset)
print(model)

# computing correlation matrix
library(corrplot)
cor_data = cor(data)
print("Correlation matrix")
print(cor_data)
corrplot(cor_data, method="circle")
corrplot(cor_data, method="number")


#plotting all the three variables 
library(car)
plot(model)
abline(model)


#prediction of the testing dataset 
testing_dataset <- data[-a_sample,] 
y_pred <- predict(model, newdata=testing_dataset)
print(y_pred)


#cross-validation by calculating RMSE,R^2,MAE

library(Metrics)
RMSE = rmse(data$sales,predict(model, newdata=testing_dataset))
print(RMSE)
R2=summary(model)$r.squared
print(R2)
MAE=mae(data$sales,predict(model, newdata=testing_dataset))
print(MAE)
