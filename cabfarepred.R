rm(list=ls())
setwd('C:/chann/ds/edwisor/cab fare pred/train_cab')
read.csv('train_cab.csv')->data
summary(data)
class(data$fare_amount)
data$fare_amount=as.character(data$fare_amount)
sum(data$fare_amount=='')
data$fare_amount[data$fare_amount=='']=NA
data$fare_amount=as.numeric(data$fare_amount)
# Missing value analysis 
missing_values=data.frame(apply(data,2,function(x){sum(is.na(x))}))
View(missing_values)
row.names(missing_values)->row_names_missing_values
missing_values$variable=row_names_missing_values
missing_values=missing_values[,c(2,1)]
row.names(missing_values)=NULL
names(missing_values)[2]="missing_percent"
missing_values=missing_values[order(-missing_values$missing_percent),]
missing_values$missing_percent=missing_values$missing_percent/nrow(data)*100
# No feature missing percentage greater than 30.So no need to remove those variables
# As missing percentage is less than 1% of data ,we can remove them safely
data=na.omit(data)
#Excluding values other than these ranges
# for latitude range : -90<value<90
#for longitude raange : -180<value<180
data=data[(data$pickup_latitude> -90 & data$pickup_latitude < 90),]
data=data[(data$dropoff_latitude> -90 & data$dropoff_latitude < 90),]
data=data[(data$pickup_longitude > -180 & data$pickup_longitude< 180), ]
data=data[(data$dropoff_longitude > -180 & data$dropoff_longitude< 180), ]
#Removing observations whoose pickup and dropoff latitude and londitude are same
data=data[((data$dropoff_latitude!=data$pickup_latitude) & (data$pickup_longitude!=data$dropoff_longitude)),]

#Histogram of fare_amount
#library(ggplot2)
#library(scales)
ggplot(data,aes_string(x=data$fare_amount))+geom_histogram(bins=15,color="black",fill="orange")+
  theme_bw()+xlab("Fare_amount")+ ylab("Frequency") + ggtitle("Distribution of fare_amount") +
  theme(text=element_text(size=15))
#Outlier analysis of fare_amount
ggplot(data,aes_string(y=data$fare_amount))+stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(outlier.color="red",outlier.size = 1,outlier.shape=18,notch=F,fill="grey")+
  theme(legend.position = "bottom")+ggtitle("Looking for outliers")
# the standard taxi begins at 2    (assumption)
data=data[data$fare_amount>2,]

#OUTLIERS in fare_amount
# As the distance limit was 50km and average fare could be 1.5 $ per KM
value= 1.5 * 50 # 75 $ would be the maximum fare.
val=data$fare_amount[data$fare_amount %in% boxplot.stats(data$fare_amount)$out]
val=val[val >value]      # Getting outliers

data=data[which(!data$fare_amount %in% val),]  #So , we remove fares greater than the value variable

#mean=mean(data$fare_amount)
#std=sd(data$fare_amount)
#data=data[((data$fare_amount> mean-3*std )& (data$fare_amount < mean +3*std)),]

#Histogram after removing outliers
ggplot(data,aes_string(x=data$fare_amount))+geom_histogram(bins=15,color="black",fill="orange")+
  theme_bw()+xlab("Fare_amount")+ ylab("Frequency") + ggtitle("Distribution after removing outliers") +
  theme(text=element_text(size=15))
#Boxplot after removing outliers
ggplot(data,aes_string(y=data$fare_amount))+stat_boxplot(geom = "errorbar",width=0.5)+
  geom_boxplot(outlier.color="red",outlier.size = 1,outlier.shape=18,notch=F,fill="grey")+
  theme(legend.position = "bottom")+ggtitle("Boxplot of fare_amount")
summary(data)
# Passenger_count cannot be decimal.So , we convert it into integer type
data$passenger_count=as.integer(data$passenger_count)
table(as.factor(data$passenger_count))
# Number of passengers cannot be 0 and maximum number of passengers that can fit in a car is 6 
data=data[((data$passenger_count>=1) & (data$passenger_count <=6)),]
table(data$passenger_count)

#Bar_plot  of passenger_count

ggplot(data, aes_string(x= as.integer(data$passenger_count))) + geom_bar(stat="count",fill='blue') +
  theme_bw() + xlab("passenger_count") + ylab("count") + ggtitle("Bar plot")
# Most of the passengers travel alone 
data$passenger_count=as.factor(data$passenger_count)


#######Feature engineering
#library(lubridate)
datetime=ymd_hms(data$pickup_datetime)
data$hour=hour(datetime)
data$hour=as.factor(data$hour)
data$weekday=weekdays(datetime)
data$weekday=as.factor(data$weekday)
data$day=day(datetime)
data$day=as.factor(data$day)
data$month=month(datetime)
data$month=as.factor(data$month)
data$year=year(datetime)
data$year=as.factor(data$year)
data=na.omit(data) #Removing NA values generated while extracting

# let's  calculate distance between source and destination using 
#pickup and  dropoff latitude and longitude
#We can calculate distance using  library -> geosphere

#library(geosphere)
for(i in 1:nrow(data)){
  t=distHaversine(c(data$pickup_longitude[i],data$pickup_latitude[i]),c(data$dropoff_longitude[i],data$dropoff_latitude[i]))
  data$distance[i]=t/1000
} 
# Histogram of distance
ggplot(data,aes_string(x=data$distance))+geom_histogram()+theme_bw()+xlab("Distance in KM")+
  ylab("Frequency ")



# No one chooses to travel in cab for more than 50km and for less than  300 meters
data=data[((data$distance>.3) & (data$distance < 50 )),]
# Histogram of distance after removing outliers
ggplot(data,aes_string(x=data$distance))+geom_histogram()+theme_bw()+xlab("Distance in KM")+
  ylab("Frequency ") +ggtitle("After removing outliers")

#Scatter Plot
ggplot(data,aes_string(x=data$distance,y=data$fare_amount))+geom_point(fill="grey",color="black")+
  ggtitle("Relation between distance and fare_amount")+ xlab("Distance in KM")+
  ylab("Fare_amount")+ theme_bw()   


weekday=aggregate.data.frame(data[,"fare_amount"],by=list(data$weekday),mean)
#Fares  are high on monday and sundayv.Hence it explains target variable and considered as a feature
ggplot(weekday,aes_string(x=weekday$Group.1,y=weekday$x)) + geom_boxplot() +xlab("weekday")+
  ylab("fare_amount")+ggtitle("Average fare Vs weekdays")+theme_bw()

yearr=aggregate.data.frame(data$fare_amount,by=list(data$year),mean)
# Average fares are increasing year by year
ggplot(yearr,aes_string(x=yearr$Group.1,y=yearr$x))+geom_boxplot()+xlab("Year")+
  ylab("Fare_amount")+ggtitle(" Average fare Vs year")
# Scatter plot
hourr=aggregate.data.frame(data$fare_amount,by=list(data$hour),mean)
ggplot(hourr,aes_string(x=hourr$Group.1,y=hourr$x))+geom_point()+xlab("Hour in a day")+
  ylab("Average fare")+ggtitle("Avg fare vs Hour")+theme_bw()

#Pearson's corelation test
cor.test(data$fare_amount,data$distance) # 0.906 corelation
# Fare_amount has a good strength of association with distance

###############################Model development###########################3
subdata=data[,c(7:13,1)]
write.csv(subdata,"data1.csv",row.names = F)
# For partioning data we use caret library
#library(caret)
set.seed(07)
#read.csv("train_index.csv")->index
index=createDataPartition(data$fare_amount,p=.8,list=F)
train=subdata[index,]
test=subdata[-index,]
#write.csv(index,"train_index.csv",row.names = F)

# Linear Regression
lm(fare_amount~.,train)->lr_model
summary(lr_model)
lr_pred=predict(lr_model,test)   # Model is 83.5 % accurate
# To calclate error metrics we use Metrics library
#library(Metrics)
############  We choose rmse as our measure as it prevents cancellation of negative and positive terms during calculation
rmse(test$fare_amount,lr_pred)     # 3.49


# Regression Trees
# to build decision tree model we use rpart library
#library(rpart)

dt_model=rpart(fare_amount~.,train,method = "anova")
dt_pred=predict(dt_model,test)
rmse(test$fare_amount,dt_pred)        # 4
summary(dt_model)
write(capture.output(summary(dt_model)), "rpartRules.txt")
#library(rpart.plot)
rpart.plot(dt_model)

#Random forest 
#library(randomForest)

rf_model0=randomForest(fare_amount~.,data = train,importance=T,ntree=200)
rf_pred0=predict(rf_model0,test)
rmse(test$fare_amount,rf_pred0)       # 4.02


#One hot encoding
#load dummies library
#library(dummies)
new_data=dummy.data.frame(subdata,names=c("passenger_count","weekday","day","month","hour","year"))
str(new_data)



train=new_data[index,]
test=new_data[-index,]
lm(fare_amount~.,train)->lr_model
# Step by step regression
bestlr_mod=step(lr_model)      #Model is 83.5% accurate
summary(bestlr_mod)

lr_pred=predict(bestlr_mod,test)
rmse(test$fare_amount,lr_pred)            #3.48622


rf_model=randomForest(fare_amount~.,data = train,importance=T,ntree=200)
rf_pred=predict(rf_model,test)
rmse(test$fare_amount,rf_pred)         #3.41893


# As rmse value of rf_model is less after one hot encoding of categorical values we fix rf_model for predicting fare_amount
#rm(bestlr_mod,index,lr_model,dt_model,t,test,train)

data=read.csv('test.csv')
summary(data)


#######Feature engineering
#library(lubridate)
datetime=ymd_hms(data$pickup_datetime)
data$hour=hour(datetime)
data$hour=as.factor(data$hour)
data$weekday=weekdays(datetime)
data$weekday=as.factor(data$weekday)
data$day=day(datetime)
data$day=as.factor(data$day)
data$month=month(datetime)
data$month=as.factor(data$month)
data$year=year(datetime)
data$year=as.factor(data$year)
data=na.omit(data)


# let's  calculate distance between source and destination using 
#pickup and  dropoff latitude and longitude
#We can calculate distance using  library -> geosphere

#library(geosphere)
for(i in 1:nrow(data)){
  t=distHaversine(c(data$pickup_longitude[i],data$pickup_latitude[i]),c(data$dropoff_longitude[i],data$dropoff_latitude[i]))
  data$distance[i]=t/1000
} 
# Checking for observations which are voliating the assumed condition
sum(data$distance>50 | data$distance<.3) # They are 197 in number

ind=1:nrow(data)
#We impute them with median

data$distance[((data$distance<0.3)| (data$distance >50))]=median(data$distance)
sum(data$distance>50 | data$distance<.3)

#library(dummies)
new_data=dummy.data.frame(data,names=c("passenger_count","weekday","day","month","hour","year"))
str(new_data)

pred=predict(rf_model,new_data)   # here pred variable is the predicted fare_amount of test data

# head of fare_amount
pred[1:5]
mean(pred)
predictions=data.frame(fare_amount=c(pred))
#write.csv(predictions,"predictions.csv",row.names = F)
