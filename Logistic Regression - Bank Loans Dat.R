# Logistic regression is a multivariate predictive modelling where Y 
# is discrete i.e. 1/0 or yes/no. 

# Probability of an event happening or not happening. 
# E.g. Customer churn, Customer will buy/not buy Customer default/not default.

#--------------------------------------------------------------------
#Import the data
#--------------------------------------------------------------------

#Set Directory
setwd("C:/Users/DELL PC/Desktop/Bank Loans")

mydata<-read.csv("bankloans.csv",header=T)
#Understand the data
str(mydata)
names(mydata)
View(mydata)
head(mydata)
tail(mydata)
dim(mydata)
nrow(mydata)
ncol(mydata)
summary(mydata)

#Understand distribution of the data
summary1 <- summary(mydata)
summary2 <- str(mydata)
library(psych)
summary3 <- describe(mydata)

write.csv(summary1, file = "summary1.csv")
write.csv(summary2, file = "summary2.csv")
write.csv(summary3, file = "summary3.csv")


mydata$ed <- factor(mydata$ed)

#Create user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    pctl <- quantile(x, na.rm=T, p=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9, 0.95,0.99))
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,pctl=pctl,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

#Vector of numaerical variables

num_var= sapply(mydata,is.numeric)
Other_var= !sapply(mydata,is.numeric)

#Applying above defined function on numerical variables

my_num_data<-t(data.frame(apply(mydata[num_var], 2, var_Summ)))
my_cat_data<-data.frame(t(apply(mydata[Other_var], 2, var_Summ)))
View(my_num_data)
View(my_cat_data)

# missing values
apply(is.na(mydata[,]),2,sum)

mydata1 <- mydata[!is.na(mydata$default),]
New_cust <- mydata[is.na(mydata$default),]

View(mydata1)
View(New_cust)

#Missing Value Treatment
mydata1[,num_var] <- apply(data.frame(mydata1[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
mydata1[,Other_var] <- apply(data.frame(mydata1[,Other_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})

M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
mydata1[,3] <- M1_fun( mydata1[,3] )
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#Splitting data into Training, Validaton and Testing Dataset
#--------------------------------------------------------------------

train_ind <- sample(1:nrow(mydata1), size = floor(0.70 * nrow(mydata1)))

training<-mydata1[train_ind,]
testing<-mydata1[-train_ind,]
#--------------------------------------------------------------------

require(InformationValue)

#--------------------------------------------------------------------
#Building Models for training dataset
#--------------------------------------------------------------------

fit<-glm(default~age+ed+employ+address+income+debtinc+creddebt+othdebt,data = training,
         family = binomial(logit))

#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model
#--------------------------------------------------------------------


#--------------------------------------------------------------------
#
#--------------------------------------------------------------------

coeff<-fit$coef #Coefficients of model
write.csv(coeff, "coeff.csv")

install.packages("InformationValue")
require(InformationValue)

ls("package:InformationValue")

train1<- cbind(training, Prob=predict(fit, type="response")) 
View(train1)

Concordance(train1$default, train1$Prob)

cut1<-optimalCutoff(train1$default, train1$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)

ROCTable<-data.frame(cut1$sensitivityTable)
View(ROCTable)

require(dplyr)
write.csv(ROCTable, "ROCTable.csv")

ks_table<-ks_stat(train1$default, train1$Prob, returnKSTable=TRUE)
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#Stepwise regression
#--------------------------------------------------------------------

step1=step(fit)

#Final Model
fit2<-glm(default ~ employ + address + debtinc + creddebt,data = training,
          family = binomial(logit))
summary(fit2)

train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#
#--------------------------------------------------------------------
Concordance(train1$default, train1$Prob)

cut1<-optimalCutoff(train1$default, train1$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)

ROCTable<-data.frame(cut1$sensitivityTable)

ks_stat(train1$default, train1$Prob, returnKSTable=TRUE)

confusionMatrix(train1$default, train1$Prob, threshold=0.27)

plotROC(train1$default, train1$Prob, Show.labels=F)
#--------------------------------------------------------------------


#--------------------------------------------------------------------
#validation
#--------------------------------------------------------------------

test1<- cbind(testing, Prob=predict(fit2,newdata=testing, type="response")) 
#View(test1)

ks_table<-ks_stat(test1$default, test1$Prob, returnKSTable=TRUE)
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# VALIDATION - Manual calculations
#--------------------------------------------------------------------

#Decile Scoring for 
##Training dataset
train1<- cbind(training, Prob=predict(fit2, type="response")) 
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1,0.9,by=0.1))
train1$decile <- findInterval(train1$Prob,c(-Inf,decLocations, Inf))
View(train1)
require(dplyr)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), default_cnt=sum(default), 
                             non_default_cnt=total_cnt -default_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))
View(decile_summ_train)

write.csv(decile_summ_train,"fit_train_DA1.csv",row.names = F)
#Decile Analysis Reports
require(sqldf)
fit_train_DA <- sqldf("select decile, min(Prob) as Min_prob
                      , max(Prob) as max_prob
                      , sum(default) as default_Count
                      , (count(decile)-sum(default)) as Non_default_Count 
                      from train1
                      group by decile
                      order by decile desc")

write.csv(fit_train_DA,"fit_train_DA1.csv",row.names = F)

##Testing dataset
test1<- cbind(testing, Prob=predict(fit2,testing, type="response")) 
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1,0.9,by=0.1))
test1$decile <- findInterval(test1$Prob,c(-Inf,decLocations, Inf))
names(test1)
#Decile Analysis Reports
require(sqldf)

fit_test_DA <- sqldf("select decile, count(decile) as count, min(Prob) as Min_prob
                     , max(Prob) as max_prob 
                     , sum(default) as default_cnt
                     from test1
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)
#--------------------------------------------------------------------

New_cust1<-cbind(New_cust, Prob=predict(fit2, New_cust, type="response"))
View(New_cust1)
New_cust1$default <- ifelse(New_cust1$Prob>0.27, 1,0)
sum(New_cust1$default)





