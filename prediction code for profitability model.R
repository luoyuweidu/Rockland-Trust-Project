##run the model first
#read in data
mydata=read.csv("combined list.csv",na.strings=c("","NA","U","Unknown"))  #question 2 combined list
summary(mydata) 
table(mydata$If..1000)

#recode response variable
mydata$If..1000<-factor(mydata$If..1000,
                        levels = c(0,1),
                        labels= c("No","Yes"))
attach(mydata)
names(mydata)


#Plot for each predictor
windows(7,4)
par(mfrow=c(1,2))
plot(If..1000,ClosestBranchDistance,xlab="If >1000",ylab="Branch Distance",col="green")
plot(If..1000,CustomerAge,xlab="If >1000",ylab="Age",col="green 4")

windows(7,4)
par(mfrow=c(1,2))
windows(7,4)
par(mfrow=c(1,2))
plot(If..1000,RMAge,xlab="If >1000",ylab="RMAge",col="green")
plot(If..1000,Deposit.Balance,xlab="If >1000",ylab="Deposit Balance",col="green 4")

plot(If..1000,Loan.Balance,xlab="If >1000",ylab="Loan.Balance",col="green")
plot(If..1000,Ann.Pre.Tax.Profit,xlab="If >1000",ylab="Ann.Pre.Tax.Profit",col="green 4")

table(If..1000,ATMRange)
table(If..1000,BillPay)
table(If..1000,BranchRange)
table(If..1000,COMBINED_HOMEOWNER_RENTER_DESC)
table(If..1000,DigitalIndflag)
table(If..1000,DWELLING_TYPE)
table(If..1000,EDUCATION)
table(If..1000,ESTIMATED_INCOME)
table(If..1000,LoanFlag)
table(If..1000,NASBcuk)
table(If..1000,RMProductFlag)
table(If..1000,SafeDepositFlag)
table(If..1000,DebitCardFlag)

#Do logistic regresion on each predictor
check1=glm(If..1000~factor(EDUCATION),family=binomial)
summary(check1)

#remove estimated income because it doesn't make sense to me
#keep education, HO, NAS

#lOOK at deleted variables need special attention:closestBranchDistance,Deposit.Balance,Loan.Balance
summary(ClosestBranchDistance)
mean1<-mean(ClosestBranchDistance[which(ClosestBranchDistance!=0)],na.rm=TRUE)
Combine_row<-ifelse(Combine_row$ClosestBranchDistance==0|is.na(Combine_row$ClosestBranchDistance),mean1,Combine_row$ClosestBranchDistance)
row=ClosestBranchDistance[which(ClosestBranchDistance!=0)]
row=na.omit(row)
summary(row) #after imputation, this variable is still insignificant
row<-scale(ClosestBranchDistance)
which(row>3|row< -3)
Combine_row=mydata[,c(5,21)]
Combine_row<-Combine_row[-which(row>3|row< -3),]
summary(Combine_row)

summary(Deposit.Balance)
row1<-scale(Deposit.Balance)
which(row1>3|row1< -3)
Combine_row1=mydata[,c(14,21)]
Combine_row1<-Combine_row1[-which(row1>3|row1< -3),]
check1=glm(Combine_row1$If..1000~Combine_row1$Deposit.Balance,family=binomial)
summary(check1) #seems this variable may have some predictive power, retained first

summary(Loan.Balance)
row2<-scale(Loan.Balance)
which(row2>3|row2< -3)
Combine_row2=mydata[,c(15,21)]
Combine_row2<-Combine_row2[-which(row2>3|row2< -3),]
check1=glm(Combine_row2$If..1000~Combine_row2$Loan.Balance,family=binomial)
summary(check1) # After removes outlier, this variable seems to have some predictive power 


#delete unnecassary variables and insignificant ones
names(mydata)
mydata=mydata[,-c(5,9,11,18,20)] 
summary(mydata)


#Variables having insignificant levels, need transformation
#combine low/medium/high for ATMRange
mydata1=mydata #make a copy of data just in case 
mydata1$ATMRange <- as.character(mydata1$ATMRange)
mydata1$ATMRange[mydata1$ATMRange == "Low"] <- "Yes"
mydata1$ATMRange[mydata1$ATMRange == "Medium"] <- "Yes"
mydata1$ATMRange[mydata1$ATMRange == "High"] <- "Yes"
mydata1$ATMRange <- as.factor(mydata1$ATMRange)
table(If..1000,mydata1$ATMRange)

#recode digital flag
mydata1$DigitalIndflag<-as.character(mydata1$DigitalIndflag)
mydata1$DigitalIndflag[mydata1$DigitalIndflag==1]<-TRUE
mydata1$DigitalIndflag[mydata1$DigitalIndflag==0]<-FALSE
mydata1$DigitalIndflag <- as.logical(mydata1$DigitalIndflag)

#change incorrect data types
mydata1$EDUCATION<-as.factor(EDUCATION)
mydata1$Loan.Balance<-as.numeric(Loan.Balance)

#check multicollinearity
names(mydata1)
str(mydata1)
library(usdm)
X=model.matrix(If..1000~CustomerAge+RMAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+ESTIMATED_INCOME+LoanFlag+NASBcuk+COMBINED_HOMEOWNER_RENTER_DESC+RMProductFlag+Deposit.Balance+Loan.Balance+EDUCATION,data=mydata1)[,-1]
vifstep(X,th=10)
mydata1=mydata1[,-9] #remove loan flag due to multicollinearity



#missing rate is HO26%, Education20%, NAS13% do we need to do imputation
#make an assumption that the probability of a value being missing is not related to itself
#################try imputating the data##################################
summary(mydata1)
library(mice)
imputed_Data <- mice(mydata1, m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(imputed_Data)
imputed_Data$imp$NASBcuk
imputed<-complete(imputed_Data,2)
write.csv(imputed, "imputed_modeldata2.csv")
names(imputed)

#check three imputed variables
check1=glm(If..1000~NASBcuk,data=imputed,family=binomial)
summary(check1)
check1=glm(If..1000~COMBINED_HOMEOWNER_RENTER_DESC,data=imputed,family=binomial)
summary(check1)
check1=glm(If..1000~EDUCATION,data=imputed,family=binomial)
summary(check1)
summary(imputed$EDUCATION)

model1=glm(If..1000~CustomerAge+RMAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION,data=imputed,family = binomial)
summary(model1)

#combine insignificant levels
#Ho
mydata2=imputed #make a copy of imputed data
summary(mydata2$COMBINED_HOMEOWNER_RENTER_DESC)
mydata2$COMBINED_HOMEOWNER_RENTER_DESC<-as.character(mydata2$COMBINED_HOMEOWNER_RENTER_DESC)
mydata2$COMBINED_HOMEOWNER_RENTER_DESC[mydata2$COMBINED_HOMEOWNER_RENTER_DESC=="Probable Homeowner 70-79"] <-"Probable Homeowner"
mydata2$COMBINED_HOMEOWNER_RENTER_DESC[mydata2$COMBINED_HOMEOWNER_RENTER_DESC=="Probable Homeowner 80-89"] <-"Probable Homeowner"
mydata2$COMBINED_HOMEOWNER_RENTER_DESC[mydata2$COMBINED_HOMEOWNER_RENTER_DESC=="Probable homeowner 90-100"] <-"Probable Homeowner"
mydata2$COMBINED_HOMEOWNER_RENTER_DESC<-as.factor(mydata2$COMBINED_HOMEOWNER_RENTER_DESC)
check1=glm(If..1000~COMBINED_HOMEOWNER_RENTER_DESC,data=mydata2,family=binomial)
summary(check1)



model1=glm(If..1000~CustomerAge+RMAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION,data=mydata2,family = binomial)
summary(model1)



#identify influential observations
library(car)
residualPlots(model1)
marginalModelPlots(model1)
influenceIndexPlot(model1,id.n = 3)
influencePlot(model1,col="red",id.n=3)


mydata3<-mydata2[-c(414,498,541,1319,2006,2121,2275),]
names(mydata3)

influential<-mydata2[c(414,498,541,1319,2006,2121,2150,2275),]

names(mydata3)
model2=glm(If..1000~CustomerAge+RMAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION,data=mydata3,family = binomial)
summary(model2)

#try removing RMAge
model3=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION,data=mydata3,family = binomial)
summary(model3)

anova(model3,model2,test="Chisq") #removal confirm

#try second-order terms
model4=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2),data=mydata3,family = binomial)
summary(model4)

#EDUCATION
mydata4<-mydata3
summary(mydata4$EDUCATION)
mydata4$EDUCATION<-as.character(mydata4$EDUCATION)
mydata4$EDUCATION[mydata4$EDUCATION=="15"]<-"HS or below"
mydata4$EDUCATION[mydata4$EDUCATION=="51"]<-"HS or below"
mydata4$EDUCATION<-as.factor(mydata4$EDUCATION)
model7=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2),data=mydata4,family = binomial)
summary(model7)
#HO
summary(mydata4$COMBINED_HOMEOWNER_RENTER_DESC)


#try interaction terms
interactions<-c("CAnn","CA","CB","CBR","CD","CN","CR","CDE","CL","cco","CE","AB","ABR","AD","AN","AR","ADE","AL","ACO","AE","AnnA","AnnB","Annbr","AnnD","AnnN","AnnR","AnnDe","AnnL","AnnCO","AnnE","BBR","BD","BN","BR","BDE","BL","BCO","BE","BRD","BRN","BRR","BRDE","BRL","BRCO","BRE","DN","DR","DDE","DL","DCO","DE","NR","NDE","NL","NCO","NE","RDE","RL","RCO","RE","DEL","DECO","DEE","LCO","LE","COE")
sample(interactions)

model8=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2)+CustomerAge*BranchRange+DigitalIndflag*Deposit.Balance+Ann.Pre.Tax.Profit*BillPay+BillPay*Deposit.Balance+CustomerAge+BillPay*EDUCATION+BranchRange*DigitalIndflag,data=mydata4,family = binomial)
summary(model8)
step(model8,direction = "both")
model9=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2)+CustomerAge*BranchRange+BranchRange*DigitalIndflag+DigitalIndflag*Deposit.Balance+Ann.Pre.Tax.Profit*BillPay,data=mydata4,family = binomial)
summary(model9)
anova(model8,model9,test="Chisq")

#try deleting BranchRange*DigitalIndflag
model10=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+RMProductFlag+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2)+CustomerAge*BranchRange+BranchRange*DigitalIndflag+DigitalIndflag*Deposit.Balance+Ann.Pre.Tax.Profit*BillPay,data=mydata4,family = binomial)
summary(model10)
anova(model10,model9,test="Chisq") 

#add back BranchRange*DigitalIndflag
model11=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2)+CustomerAge*BranchRange+BranchRange*DigitalIndflag+DigitalIndflag*Deposit.Balance+Ann.Pre.Tax.Profit*BillPay,data=mydata4,family = binomial)
summary(model11)
anova(model9,model11,test="Chisq") #delete RMproduct Flag

#Try combine BranchRange
mydata5<-mydata4
mydata5$BranchRange<-as.character(mydata5$BranchRange)
mydata5$BranchRange[mydata5$BranchRange=="Low"]<-"LM"
mydata5$BranchRange[mydata5$BranchRange=="Medium"]<-"LM"
mydata5$BranchRange<-as.factor(mydata5$BranchRange)
model12=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2)+CustomerAge*BranchRange+DigitalIndflag*Deposit.Balance+Ann.Pre.Tax.Profit*BillPay,data=mydata5,family = binomial)
summary(model12)
anova(model12,model11,test = "Chisq")

#keep combining BranchRange
mydata5$BranchRange<-as.character(mydata5$BranchRange)
mydata5$BranchRange[mydata5$BranchRange=="LM"]<-"Yes"
mydata5$BranchRange[mydata5$BranchRange=="High"]<-"Yes"
mydata5$BranchRange<-as.factor(mydata5$BranchRange)
model13=glm(If..1000~CustomerAge+Ann.Pre.Tax.Profit+ATMRange+BillPay+BranchRange+DigitalIndflag+NASBcuk+Deposit.Balance+Loan.Balance+COMBINED_HOMEOWNER_RENTER_DESC+EDUCATION+I(CustomerAge^2)+CustomerAge*BranchRange+DigitalIndflag*Deposit.Balance+Ann.Pre.Tax.Profit*BillPay,data=mydata5,family = binomial)
summary(model13)
anova(model13,model11,test="Chisq")

#keep with original levels of BranchRange

predicted_probability<-predict(model11,newdata=as.data.frame(mydata4),type="response")
combined_file=cbind(predicted_probability,mydata4$If..1000)
write.csv(combined_file,"probability_HO_EDUCATION1.csv")

library(AUC) #0.7043988
auc(roc(predicted_probability,mydata4$If..1000))
auc(sensitivity(predicted_probability,mydata4$If..1000))
auc(specificity(predicted_probability,mydata4$If..1000))
plot(sensitivity(predicted_probability,mydata4$If..1000))
prediction_label<-as.numeric(predicted_probability>0.2)
table(mydata4$If..1000,prediction_label)

#cross validation
library(boot)
cost<-function(Y_obs,prediction_prob)
  return(mean(abs(Y_obs-prediction_prob)>0.15))
cv.glm(mydata4,model10,cost)$delta[1]


#create predicted probaility plot
mytable<-table(predicted_probability,exclude=NULL)
prop.table(mytable)
hist(predicted_probability,xlim=c(0,1),breaks=500)


plot(roc(prediction_label,mydata4$If.response))



#prediction for new dataset
newdata=read.csv(file.choose(),header = T,na.strings=c("","NA","U","Unknown")) #rawdata 2
newdata<-newdata[,-1]

#change names
names(newdata)
names(mydata4)
colnames(newdata)[11]<-"NASBcuk"
colnames(newdata)[12]<-"Loan.Balance"



#combine low/medium/high for ATMRange
newdata1<-newdata #make a copy of data just in case 
newdata1$ATMRange <- as.character(newdata1$ATMRange)
newdata1$ATMRange[newdata1$ATMRange == "Low"] <- "Yes"
newdata1$ATMRange[newdata1$ATMRange == "Medium"] <- "Yes"
newdata1$ATMRange[newdata1$ATMRange == "High"] <- "Yes"
newdata1$ATMRange <- as.factor(newdata1$ATMRange)


#recode digital flag
newdata1$DigitalIndflag<-as.character(newdata1$DigitalIndflag)
newdata1$DigitalIndflag[newdata1$DigitalIndflag==1]<-TRUE
newdata1$DigitalIndflag[newdata1$DigitalIndflag==0]<-FALSE
newdata1$DigitalIndflag <- as.logical(newdata1$DigitalIndflag)


#change incorrect data types
str(newdata1)
newdata1$EDUCATION<-as.factor(newdata1$EDUCATION)

#imputation
library(mice)
imputed_newData <- mice(newdata1, m=5, maxit = 50, method = 'polyreg', seed = 500)
summary(imputed_newData)
imputed_newData$imp$NASBcuk
imputed_new<-complete(imputed_newData,2)
write.csv(imputed_new,"imputed_prediction_model2.csv") #save imputed data for later reference
names(imputed_new)
names(mydata4)
imputed_new<-read.csv("imputed_prediction_model2.csv")

#combine levels for education
imputed_new1<-imputed_new
summary(imputed_new1$EDUCATION)
imputed_new1$EDUCATION<-as.character(imputed_new1$EDUCATION)
imputed_new1$EDUCATION[imputed_new1$EDUCATION=="15"]<-"HS or below"
imputed_new1$EDUCATION[imputed_new1$EDUCATION=="51"]<-"HS or below"
imputed_new1$EDUCATION<-as.factor(imputed_new1$EDUCATION)

#combine level for HO
summary(imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC)
imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC<-as.character(imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC)
imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC[imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC=="Probable Homeowner 70-79"] <-"Probable Homeowner"
imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC[imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC=="Probable Homeowner 80-89"] <-"Probable Homeowner"
imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC[imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC=="Probable homeowner 90-100"] <-"Probable Homeowner"
imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC<-as.factor(imputed_new1$COMBINED_HOMEOWNER_RENTER_DESC)

#prediction
predicted_probability<-predict(model11,newdata=as.data.frame(imputed_new1),type="response")
comb<-cbind(imputed_new1$CIFKEY,predicted_probability)
write.csv(comb,"prediction_probability_model2.csv")

#frequency plot
mytable<-table(predicted_probability,exclude=NULL)
prop.table(mytable)
hist(predicted_probability,xlim=c(0,1),breaks=500)

prediction_label<-as.numeric(predicted_probability>0.2)
length(which(prediction_label==1))  #set cut-off point=0.2 then have 15173 =1

