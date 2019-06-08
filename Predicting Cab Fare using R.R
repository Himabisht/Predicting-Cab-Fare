#Setting working directory
setwd("C:/Users/HP/Desktop/Project")
getwd()

#Importing train dataset
train = read.csv("train_cab.csv", header =T)

#############
############

#Calculating the missing values
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val$column = row.names(missing_val)
row.names(missing_val)= NULL
names(missing_val)[1] ="Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train))*100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
missing_val = missing_val[,c(2,1)]


#Removing passenger_count variable 
train$passenger_count = NULL


##############
#############

#Outlier Analysis Using Box Plot
#Selecting only numerical variables
train$fare_amount = as.numeric(train$fare_amount)
train$pickup_datetime =as.numeric(train$pickup_datetime)
numeric_index = sapply(train, is.numeric)

numeric_data = train[,numeric_index]
cnames = colnames(numeric_data)
library(ggplot2)

for(i in 1:length(cnames)){
  assign(paste0("gn",i), ggplot(aes_string(y =(cnames[i]), x ="fare_amount"), data=subset(train))+
           stat_boxplot(geom ="errorbar", width =0.5)+
           geom_boxplot(outlier.colour="red", fill ="grey", outlier.shape=18,
                        outlier.size=1, notch=FALSE)+
           theme(legend.position="bottom")+
           labs(y=cnames[i],x ="fare_amount")+
           ggtitle(paste("Box plot of fare_amount for", cnames[i])))
}


#Plotting plots together
gridExtra::grid.arrange(gn2,gn5,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn6,ncol=1)


##########
##########


#Feature Selection
library(corrgram)

corrgram( train[,numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main="Correlation Plot")


############
############

#Feature Scaling
#Normality check
qqnorm(train$fare_amount)
hist(train$fare_amount)

#Normalisation

for(i in cnames){
  print(i)
  train[,i] =(train[,i] - min(train[,i]))/(max(train[,i] - min(train[,i])))
  
}
range(train$fare_amount)


#################
################


# Dividing the whole train dataset into tr and test
library(sampling)
train_index = sample(1:nrow(train), 0.8* nrow(train))
tr = train[train_index,]
test = train[-train_index,]


##################
##################


# Linear Regression
library(usdm)
vif(train[,-1])

vifcor(train[,-1], th=0.9)

#Build model
model =lm(fare_amount ~., data=tr)

summary(model)


#Predict test cases
pre = predict(model, test[,-1])

#Calculate MAPE
MAPE = function(y,yhat){
  mean(abs((y-yhat)/y))
}


MAPE(test[,1], pre)


###################
###################

#Decision tree
library(rpart)
fit = rpart(fare_amount ~., data =tr, method = "annova")


#Predict the test cases
pre_dt = predict(fit,test[,-1])


#Calculate MAPE

MAPE(test[,1], pre_dt)







