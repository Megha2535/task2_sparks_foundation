#to find:
#What will be predicted score if a student study for 9.25 hrs in a day?
rm(list=ls())
getwd()
setwd("D:/Coursera")
library(dplyr)
library(caret)
library(readxl)
#------------------------------------------------importing
df<-read_xlsx("data.xlsx")
head(df)
#------------------------------------------------- graph
ggplot(df,aes(x=Hours,y=Scores))+geom_point()+scale_x_continuous(name="Hours")+scale_y_continuous(name="Scores")

# we can see there is a positive linear realtionship between x and y
#--------------------------------------------------splitting the data
set.seed(42)
rows<-sample(nrow(df))
shuffled_df<-df[rows,]
split<-round(nrow(df)*0.60)
train<-df[1:split,]
test<-df[split+1:nrow(df),]
#------------------------------------------------modelling (training the model and testing it)
model<-lm(Scores~Hours,train)
plot(Scores~Hours,data=df)
abline(model)
summary(model)
p<-predict(model,test)
p
p<-p[1:10]
test<-test[1:10,]

predict(model,list(Hours=9.5))
#comes out to be 92.03239

#-------------------------------------------------Mean absolute error
library(Metrics)
mae(test[['Scores']],p)


