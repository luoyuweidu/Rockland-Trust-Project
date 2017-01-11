mydata1=read.csv(file.choose(),header=TRUE,na.strings=c("","NA","U","Unknown"))
mydata1<-mydata1[,-1]
summary(mydata1)

#combine level for NASbuck
summary(mydata1$NASbuck)
mydata2<-mydata1 #create a copy of data,just in case
mydata2$NASbuck <- as.character(mydata2$NASbuck)
mydata2$NASbuck[mydata2$NASbuck == "<25k"] <- "<100K"
mydata2$NASbuck[mydata2$NASbuck == "25-49k"] <- "<100K"
mydata2$NASbuck[mydata2$NASbuck == "50-74k"] <- "<100K"
mydata2$NASbuck[mydata2$NASbuck == "75-99k"] <- "<100K"
mydata2$NASbuck <- as.factor(mydata2$NASbuck)

#impute the data
summary(mydata2)
library(mice)
imputed_Data <- mice(mydata2, m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(imputed_Data)
imputed_Data$imp$NASbuck
imputed<-complete(imputed_Data,2)
write.csv(imputed, "imputed_prediction_model1.csv")
names(imputed)


#do level combination
mydata3<-imputed
str(mydata3)
mydata3$ATMRange <- as.character(mydata3$ATMRange)
mydata3$ATMRange[mydata3$ATMRange == "Low"] <- "Yes"
mydata3$ATMRange[mydata3$ATMRange == "Medium"] <- "Yes"
mydata3$ATMRange[mydata3$ATMRange == "High"] <- "Yes"
mydata3$ATMRange <- as.factor(mydata3$ATMRange)

#combine NASbuck
summary(mydata3$NASbuck)
mydata3$NASbuck <- as.character(mydata3$NASbuck)
mydata3$NASbuck[mydata3$NASbuck == "250-499k"] <- ">250K<1mm"
mydata3$NASbuck[mydata3$NASbuck == "500-749k"] <- ">250K<1mm"
mydata3$NASbuck[mydata3$NASbuck == "750-999k"] <- ">250K<1mm"
mydata3$NASbuck <- as.factor(mydata3$NASbuck)




#fit the model##################################################################

#read in data
mydata_Q1=read.csv("List containing Age.csv",na.strings=c("","NA","Unk","Un","Unknown"))
names(mydata_Q1)
mydata_Q1=mydata_Q1[,-c(16,17)]



#delete unecessary variable
mydata1_Q1<-mydata_Q1[,-c(2,3,7)]
names(mydata1_Q1)

#combine insignificant level
summary(mydata1_Q1$NASbuck)
mydata2_Q1<-mydata1_Q1 #create a copy of data,just in case
mydata2_Q1$NASbuck <- as.character(mydata2_Q1$NASbuck)
mydata2_Q1$NASbuck[mydata2_Q1$NASbuck == "<25k"] <- "<100K"
mydata2_Q1$NASbuck[mydata2_Q1$NASbuck == "25-49k"] <- "<100K"
mydata2_Q1$NASbuck[mydata2_Q1$NASbuck == "50-74k"] <- "<100K"
mydata2_Q1$NASbuck[mydata2_Q1$NASbuck == "75-99k"] <- "<100K"
mydata2_Q1$NASbuck <- as.factor(mydata2_Q1$NASbuck)

#data imputation
#make a assumption that the probability of being missing for HO(16%) and NAS(2%) are not relevent to themselves
summary(mydata2_Q1)
library(mice)
imputed_Data_Q1 <- mice(mydata2_Q1, m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(imputed_Data_Q1)
imputed_Data_Q1$imp$NASbuck
imputed_Q1<-complete(imputed_Data_Q1,2)
names(imputed)

model1=glm(If.response~Age+OnlineRange+ATMRange+NASbuck+HOBucket+BillPay+BranchRange+SumBookBal+RMAge+DigitalInFlag,data=imputed_Q1,family = binomial)
summary(model1)

#identify influential observations
library(car)
residualPlots(model1)
marginalModelPlots(model1)
influenceIndexPlot(model1,id.n = 3)
influencePlot(model1,col="red",id.n=3)

influential=imputed_Q1[c(2162,3124,3962,10233,10353,10374,10379,10386),]
mydata3_Q1=imputed_Q1[-c(2162,3124,3962,10233,10353,10374,10379,10386),]

#fit the model again without influential observations
model2=glm(If.response~Age+OnlineRange+ATMRange+NASbuck+HOBucket+BillPay+BranchRange+SumBookBal+RMAge+DigitalInFlag,data=mydata3_Q1,family = binomial)
summary(model2)


#combine ATM Range
str(mydata3_Q1)
mydata3_Q1$ATMRange <- as.character(mydata3_Q1$ATMRange)
mydata3_Q1$ATMRange[mydata3_Q1$ATMRange == "Low"] <- "Yes"
mydata3_Q1$ATMRange[mydata3_Q1$ATMRange == "Medium"] <- "Yes"
mydata3_Q1$ATMRange[mydata3_Q1$ATMRange == "High"] <- "Yes"
mydata3_Q1$ATMRange <- as.factor(mydata3_Q1$ATMRange)
model3=glm(If.response~Age+OnlineRange+ATMRange+NASbuck+HOBucket+BillPay+BranchRange+SumBookBal+RMAge+DigitalInFlag,data=mydata3_Q1,family = binomial)
summary(model3)

#combine NASbuck
summary(mydata3_Q1$NASbuck)
mydata3_Q1$NASbuck <- as.character(mydata3_Q1$NASbuck)
mydata3_Q1$NASbuck[mydata3_Q1$NASbuck == "250-499k"] <- ">250K<1mm"
mydata3_Q1$NASbuck[mydata3_Q1$NASbuck == "500-749k"] <- ">250K<1mm"
mydata3_Q1$NASbuck[mydata3_Q1$NASbuck == "750-999k"] <- ">250K<1mm"
mydata3_Q1$NASbuck <- as.factor(mydata3_Q1$NASbuck)
table(mydata3_Q1$If.response,mydata3_Q1$NASbuck)

model4=glm(If.response~Age+OnlineRange+ATMRange+NASbuck+HOBucket+BillPay+BranchRange+SumBookBal+RMAge+DigitalInFlag,data=mydata3_Q1,family = binomial)
summary(model4)



#fit three possible interactions in the model, delete them one by one
model17=glm(If.response~Age+ATMRange+NASbuck+HOBucket+BillPay+BranchRange+SumBookBal+RMAge+DigitalInFlag+NASbuck*BranchRange+ATMRange*BranchRange,data=mydata3_Q1,family = binomial)
summary.glm(model17)


#get complete data
?predict
names(mydata3)
names(mydata3_Q1)
colnames(mydata3)[5]<-"HOBucket"
colnames(mydata3)[6]<-"Age" 
colnames(mydata3)[7]<-"DigitalInFlag"
colnames(mydata3)[9]<-"SumBookBal"

mydata4<-mydata3
mydata4$DigitalInFlag<-as.character(mydata4$DigitalInFlag)
mydata4$DigitalInFlag[mydata4$DigitalInFlag==1]<-TRUE
mydata4$DigitalInFlag[mydata4$DigitalInFlag==0]<-FALSE
mydata4$DigitalInFlag <- as.logical(mydata4$DigitalInFlag)

mydata4$HOBucket<-as.character(mydata4$HOBucket)
mydata4$HOBucket[mydata4$HOBucket=="Probable Homeowner 70-79"]<-"Likely"
mydata4$HOBucket[mydata4$HOBucket=="Probable Homeowner 80-89"]<-"Likely"
mydata4$HOBucket[mydata4$HOBucket=="Probable homeowner 90-100"]<-"Likely"
mydata4$HOBucket[mydata4$HOBucket=="Homeowner"]<-"Likely"
mydata4$HOBucket[mydata4$HOBucket=="Probably Renter"]<-"Renter"
mydata4$HOBucket<-as.factor(mydata4$HOBucket)

str(mydata3)
score1<-predict(model17,newdata=as.data.frame(mydata4),type="response")

combined_file=cbind(mydata4$CIFKEY,score1)
write.csv(combined_file,"probability_model1.csv")

mytable<-table(score1,exclude=NULL)
prop.table(mytable)
hist(score1,xlim=c(0,0.2),breaks=500)
