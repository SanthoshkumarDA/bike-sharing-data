
dim(bike)
head(bike)
tail(bike)
summary(bike)

library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
library(caret)
library(caretEnsemble)
library(xgboost)

#converting to date format
bike$dteday=as.POSIXct(bike$dteday)
bike$season=as.factor(bike$season)

#ggplot
#Temp
ggplot(bike,aes(temp,cnt))+geom_point()+theme_bw()
ggplot(bike,aes(temp,cnt))+geom_point(aes(color=temp))+theme_bw()

#Date
ggplot(bike,aes(dteday,cnt,color=temp))+geom_point(alpha=0.5)+theme_bw()
ggplot(bike,aes(factor(holiday),dteday))+geom_boxplot()+theme_bw()

#Weekday
ggplot(bike,aes(factor(weekday),cnt))+geom_boxplot()+theme_bw()                                                                       
ggplot(bike,aes(x=reorder(weekday,cnt),y=cnt))+geom_bar(stat="identity")+theme_bw()                                                                       
ggplot(bike,aes(factor(weekday),cnt,fill=factor(holiday)))+geom_bar(stat="identity")+theme_bw()                                                                       

#Windspeed
ggplot(bike,aes(windspeed,cnt))+geom_point(alpha=0.5,aes(color=factor(temp)))

#Season
ggplot(bike,aes(season,temp))+geom_boxplot()+theme_bw()

#Holiday
ggplot(bike,aes(factor(holiday),cnt))+geom_boxplot()+theme_bw()
ggplot(bike,aes(factor(workingday),cnt))+geom_boxplot()+theme_bw()

#month
ggplot(bike,aes(factor(mnth),cnt))+geom_bar(stat = 'identity',fill='red')+theme_bw()
ggplot(bike,aes(factor(mnth),cnt,fill=factor(season)))+geom_bar(stat = 'identity')+theme_bw()
ggplot(bike,aes(factor(mnth),temp))+geom_bar(stat = 'identity',fill='red')+theme_bw()
ggplot(bike,aes(factor(mnth),cnt,fill=factor(workingday)))+geom_bar(stat = 'identity',fill='red')+theme_bw()

#EDA
#holidays count
hc=bike%>%
  group_by(holiday)%>%
  summarise(count=n())
hc

#working day counts
wk=bike%>%
  group_by(workingday)%>%
  summarise(count=n())
wk

#season counts
sn=bike%>%
  group_by(season)%>%
  summarise(count=n())%>%
  arrange(desc(count))
sn

wor=bike%>%
  group_by(weekday)%>%
  summarise(count=n())%>%
  arrange(desc(count))
wor

#hour wise on weekdays
hor=bike%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor

hor1=bike%>%
  filter(weekday==4)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(cnt)
hor1

hor2=bike%>%
  filter(weekday == 3)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor2

hor3=bike%>%
  filter(weekday == 0)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor3

hor4=bike%>%
  filter(weekday==5)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor4

hor5=bike%>%
  filter(weekday==2)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor5

hor6=bike%>%
  filter(weekday==1)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor6

hor7=bike%>%
  filter(weekday==6)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
hor7

horr=bike%>%
  filter(workingday==1)%>%
  group_by(hr)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
horr

#holidays on weekdays
wh=bike%>%
  filter(holiday==1)%>%
  group_by(weekday)%>%
  summarise(cnt=n())
wh

hol=bike%>%
  filter(holiday==0)%>%
  group_by(weekday)%>%
  summarise(cnt=n())
hol

#temperature counts
tem=bike%>%
  group_by(temp)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))
tem

#temp wise highest count
te=bike%>%
  select(temp,cnt)%>%
  arrange(desc(cnt))
te

#temp wise lowest count
le=bike%>%
  select(temp,cnt)%>%
  arrange(cnt)
le

#monthwise
mo=bike%>%
  group_by(mnth)%>%
  summarise(cnt=n())%>%
  arrange(desc(cnt))%>%
  top_n(5)
mo

#month and season
sm=bike%>%
  group_by(mnth)%>%
  filter(season==1)%>%
  summarise(sea=n())
sm

sm1=bike%>%
  group_by(mnth)%>%
  filter(season==2)%>%
  summarise(sea=n())
sm1

sm2=bike%>%
  group_by(mnth)%>%
  filter(season==3)%>%
  summarise(sea=n())
sm2

sm4=bike%>%
  group_by(mnth)%>%
  filter(season==4)%>%
  summarise(sea=n())
sm4

#season wise avg count
sc=bike%>%
  filter(season==1)%>%
  summarise(avg.count=mean(cnt))
sc

sc1=bike%>%
  filter(season==2)%>%
  summarise(avg.count=mean(cnt))
sc1

sc2=bike%>%
  filter(season==3)%>%
  summarise(avg.count=mean(cnt))
sc2

sc3=bike%>%
  filter(season==4)%>%
  summarise(avg.count=mean(cnt))
sc3

#month wise avg count
mc=bike%>%
  group_by(mnth)%>%
  summarise(avg.cnt=mean(cnt))%>%
  arrange(desc(avg.cnt))
mc

#weekday avg cnt
wc=bike%>%
  group_by(weekday)%>%
  summarise(avg.cnt=mean(cnt))%>%
  arrange(desc(avg.cnt))
wc

#weekday and holiday
wh=bike%>%
  group_by(weekday)%>%
  filter(holiday==1)%>%
  summarise(avg.cnt=mean(cnt))%>%
  arrange(desc(avg.cnt))
wh

wh1=bike%>%
  group_by(weekday)%>%
  filter(holiday==0)%>%
  summarise(avg.cnt=mean(cnt))%>%
  arrange(desc(avg.cnt))
wh1


#convertin to numeric
bike$yr=as.numeric(bike$yr)
bike$holiday=as.numeric(bike$holiday)
bike$weekday=as.numeric(bike$weekday)
bike$workingday=as.numeric(bike$workingday)
bike$weathersit=as.numeric(bike$weathersit)
bike$casual=as.numeric(bike$casual)
bike$registered=as.numeric(bike$registered)
bike$cnt=as.numeric(bike$cnt)
bike$mnth=as.numeric(bike$mnth)


#correlation
cormat=cor(bike[4:17])
corrplot(cormat)
corrplot(cormat, number.cex = .9, method = "square", 
         hclust.method = "ward", order = "FPC",
         type = "full", tl.cex=0.8,tl.col = "black")

#Removing correlated columns
bike1=bike[-c(1,4,10,12,15,16)]

#changing column names
colnames(bike1)=c('date','season','month','hour','holiday','weekday','workingday',
                  'temp','humidity','windspeed','count')

#removing date column
bike2=bike1[-c(1)]

head(bike2)

#converting to factor and numeric
bike2$month=as.factor(bike2$month)
bike2$hour=as.factor(bike2$hour)
bike2$holiday=as.factor(bike2$holiday)
bike2$weekday=as.factor(bike2$weekday)
bike2$workingday=as.factor(bike2$workingday)
bike2$year=as.factor(bike2$year)
bike2$count=as.numeric(bike2$count)
str(bike2)
summary(bike2)
dim(bike2)

#finding outliers
ggplot(bike2,aes(workingday,count))+geom_boxplot()+theme_bw()

#outlier removal
quartiles = quantile(bike2$count, probs=c(.25, .75), na.rm = FALSE)
IQR = IQR(bike2$count)
Lower =quartiles[1] - 1.5*IQR
Upper =quartiles[2] + 1.5*IQR 
bike2 = subset(bike2,bike2$count > Lower & bike2$count < Upper)
summary(bike2)
ggplot(bike2,aes(workingday,count))+geom_boxplot()+theme_bw()


#model building
#Train test split
#Data Split
ind = createDataPartition(bike2$count, 
                          p = 0.70,                         
                          list = FALSE)
ctrain=bike2[ind,]
ctest=bike2[-ind,]

#building linear model
m1=lm(count~., data = ctrain)

#summarize the model
summary(m1)

#predictions
p1=m1%>%
  predict(ctest)

# Model performance
# (a) Prediction error, RMSE
RMSE(p1, ctest$count)

# (b) R-square
R2(p1, ctest$count)

#Decision trees using CARET
# Fit the model on the training set
set.seed(1234)
m2 <- train(
  count ~., data = ctrain, method = "rpart",
  trControl = trainControl("cv", number = 3),
  tuneLength = 10
)

# Plot model error vs different values of
# cp (complexity parameter)
plot(m2)

# Print the best tuning parameter cp that
# minimize the model RMSE
m2$bestTune

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(m2$finalModel)
text(m2$finalModel, digits = 3)

#Decision rules in the model
m2$finalModel

# Make predictions on the test data
predictions <- m2 %>% predict(ctest)
head(predictions)

# Compute the prediction error RMSE
RMSE(predictions, ctest$count)
tree1= rpart(count~., ctrain, 
             control = rpart.control(
               minsplit = 20, minbucket = 7,
               maxdepth = 10, usesurrogate = 2, xval =10 ))

#plot the tuned tree
rpart.plot(tree1,type=3,clip.right.labs = TRUE)

#lets prune the tree
printcp(tree1)
bestcp= tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"]
tree2=prune(tree1,bestcp)
rpart.plot(tree2)

pred <- tree1 %>% predict(ctest)
head(pred)

RMSE(pred, ctest$count)

#Random Forest using CARET
# Fit the model on the training set
set.seed(1234)
m3 = train(
  count ~., data = ctrain, method = "rf",
  trControl = trainControl("cv", number = 3)
)

# Best tuning parameter mtry
m3$bestTune

# Make predictions on the test data
predictions = m3 %>% 
  predict(ctest)
head(predictions)

# Compute the average prediction error RMSE
RMSE(predictions, ctest$count)

# Fit the model on the training set
set.seed(123)
m4 <- train(
  count ~., data = ctrain, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter mtry
m4$bestTune

# Make predictions on the test data
predictions <- m4 %>% predict(ctest)
head(predictions)

# Compute the average prediction error RMSE
RMSE(predictions, ctest$count)

#XGBoosting
train_x = data.matrix(ctrain[, -10])
train_y = ctrain[,10]

test_x = data.matrix(ctest[, -10])
test_y = ctest[, 10]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)


#fitting the model
xgbc = xgboost(data = xgb_train, max.depth = 2, nrounds = 50)
print(xgbc)

##Next, we'll predict the x test data with the xgbc model.
pred_y = predict(xgbc, xgb_test)

#Accuracy check
#Next, we'll check the prediction accuracy with MSE, MAE, and RMSE metrics.
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)
r5 = R2(test_y, pred_y, form = "traditional")


cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse,'r-squared:',r5)

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(train_x),model = xgbc)
xgb.plot.importance (importance_matrix = mat[1:20]) 
