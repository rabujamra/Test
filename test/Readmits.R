# import the data set
setwd("~/Desktop/Desktop/Projects/Projects_New/SOA Presentation")
library("data.table")
library("readxl")
data<- as.data.table(read_excel("Readmits.xlsx"))
head(data)
# 1.DRG Classification --------------------------------------------
# Classify DRG into 2 groups: DRG medical/surgical.Link of classifi-cation is below:
# Https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MedicareFeeforSvcPartsAB/downloads/DRGdesc08.pdf

# valid codes for DRG surgical
surgicalG<- c(1:42,113:117,129:139,163:168,215:264,326:358,405:425,453:517,573:585,614:630,652:675,707:718,734:750,765:770,799:804,820:830,853:858,876,901:909,927:929,939:941,955:959,969:970,981:989)
# valid codes for DRG medical
medicalG<- c(52:103,121:125,146:159,175:208,280:316,368:395,432:446,533:566,592:607,637:645,682:700,722:730,754:761,774:795,808:816,834:849,862:872,880:897,913:923,933:935,945:951,963:965,974:977)
# create DRG.Class variable
data[DRG %in% surgicalG,DRG.Class:=as.factor("SURG")]
data[DRG %in% medicalG,DRG.Class:=as.factor("MED")]
data[is.na(DRG.Class),DRG.Class:=as.factor("UNGROUP")]

# 2.Determine length of stay --------------------------------------
data[,LOS:=as.Date(as.character(Discharge.Date),"%Y-%m-%d")- as.Date(as.character(Admit.Date),"%Y-%m-%d") +1]

# 3.Determine age (up to the date of admission) -------------------
data[,Age:=year(as.Date(as.character(Admit.Date),"%Y-%m-%d"))-year(as.Date(as.character(Birthday),'%Y%m%d'))]

# 4.Changing labels for Gender ------------------------------------
data[,Gender:=as.factor(Gender)]
levels(data$Gender)<- c("M","F")

# 5. Changing labels for Race -------------------------------------
data[,Race:=as.factor(Race)]
levels(data$Race)<- c("White","Black","Others","Hispanic")

# 6.Calculate HCC riskscore ---------------------------------------

# A. Calculate demographic riskscore
# a) Subset important information to calculate riskscore: need age,gender,and HCCs
demo.get<- data[,.(ID.Codes,Age,Gender)]
# b) create a data table that contains risk-score for each demo-graphic group
Age<-rep(seq(0,120),2)
Gender<- c(rep("F",121),rep("M",121))
demo.score<- c(rep(0.198,35),rep(0.212,10),rep(.274,10),rep(.359,5),rep(.416,5),rep(.283,5),rep(.346,5),rep(.428,5),rep(.517,5),rep(.632,5),rep(.755,5),rep(.775,26),rep(.079,35),rep(.119,10),rep(.165,10),rep(.292,5),rep(.332,5),rep(.309,5),rep(.378,5),rep(.464,5),rep(.565,5),rep(.647,5),rep(.776,5),rep(.963,26))
demo<- as.data.table(cbind(Age,Gender,demo.score))
# convert age and demo.score into numerics:
cols<- c("Age","demo.score")
demo[,(cols):=lapply(.SD,as.numeric),.SDcols=cols] 
# convert Gender into categorical:
demo[,Gender:=as.factor(Gender)]
# c) Merge demo into demo.get, using Age and Gender columns to merge:
demo.get<- merge(demo.get,demo,by=c("Age","Gender"))
demo.get<- demo.get[order(ID.Codes)]
rm(demo)
# B. Calculate disease riskscore:
# a) Subset important information to calculate riskscore:
hcc.get<- data[,c(22:100),with=FALSE]
hcc.get<- matrix(sapply(hcc.get,as.numeric),nrow=66782,ncol=79)

# b) Input Disease Coefficients (Community Factor):
# Use data in Table 1:Preliminary Community and Institutional Rela-tive Factors for the CMS-HCC Risk Adjustment Model 
# this data set doesn't have HCC51, HCC52, HCC138, HCC139, HCC140, HCC141, HCC159, HCC160
diseaseC<- as.matrix(c(.492,.520,.557,2.425,1.006,0.695,.330,.180,0.334,.334,.124,.653,.342,.240,1.003,.425,.313,.337,.257,.279,.423,.376,1.078,.306,.258,.358,.358,.471,.318,1.075,.868,.441,1.016,.036,.281,.460,.482,.555,.252,.533,1.732,.769,.326,.361,.283,.283,.210,.276,.371,.333,.481,.212,1.313,.417,.288,.388,.388,.294,.691,.212,.223,.248,.617,.617,.227,.277,1.071,1.071,.473,.458,.533,.141,.441,.363,.379,.555,1.032,.609,0.804),nrow=79,ncol=1)
# c) the riskscore vector is the multiplication between hcc.get and diseaseC:
hcc.get<- as.data.table(hcc.get %*% diseaseC)
hcc.get<- cbind(data$ID.Codes,hcc.get)
names(hcc.get)<- c("ID.Codes","hcc.score")
hcc.get<- hcc.get[order(ID.Codes)]
# C. Calculate the total HCC riskscore and add it into the big data set:
data<- data[order(ID.Codes)]
data$HCC.Riskscore<- demo.get$demo.score+hcc.get$hcc.score

# 7. Mapping DRG Complication ------------------------------------
SurgMCC.CC<-c(1,5,11,20,23,25,28,31,34,37,40,163,166,216,219,222,224,226,228,231,233,235,237,239,242,246,248,250,252,255,258,260,326,329,332,335,338,341,347,420,423,453,456,459,461,463,466,469,471,474,477,480,485,492,495,500,503,510,515,573,576,579,616,619,622,625,628,653,656,659,662,665,668,673,736,739,799,802,820, 823,826,856,901,907,939,957,969,981,984,987,12,21,26,29,32,35,38,41,113,116,129,131,133,135,137,164,167,217,220,229,240,243,253,256,261,327,330,333,336,339,342,345,348,351,354,357,464,467,472,475,478,481,483,486,488,490,493,496,498,501,504,507,511,513,516,574,577,580,582,584,614,617,620,623,626,629,654,657,660,663,666,669,671,674,707,709,711,713,715,717,734,737,740,742,744,746,749,800,803,821,824,827,829,854,857,902,908,928,940,958,982,985,988)

SurgNoC<- c(2,6,13,22,24,27,30,33,36,39,42,114,117,130,132,134,136,138,165,168,218,219,220,221,223,224,225,226,227,230,232,234:236,238,241,244,247,249,250,251,254,257,259,262,328,331,334,337,340:343,349,352,355,358,407:410,413:419,422,425,455,459,460,462,465,468,470,473,476,479,482,484,487,489,491,494,497,499,502,505,508,512,514,517,575,578,581,583,661,664,667,670,672,675,708,710,712,714,716,718,735,738,741,743,745,747,750,766,801,804,822,825,828,830,855,858,903,905,909,929,941,959,970,983,986,989)

MedicalMCC.CC<- c(54,56,58,61,64,67,70,73,77,80,82,85,88,91,94,97,100,102,124,146,150,152,154,157,175,177,180,183,186,190,193,196,199,205,280,283,286,288,291,296,299,302,304,306,308,314,368,371,374,377,380,383,385,388,391,393,432,435,438,441,444,533,535,539,542,545,548,551,553,555,557,559,562,564,592,595,597,602,604,606,637,640,643,682,686,689,693,698,722,725,727,754,757,808,811,814,834,837,840,843,846,862,865,867,871,896,913,915,917,922,947,963,974,52,59,62,65,71,75,78,83,86,89,92,95,98,121,147,155,158,178,181,184,187,191,194,197,200,202,281,284,289,292,294,297,300,309,315,369,372,375,378,381,386,389,394,433,436,439,442,537,540,543,546,549,560,565,593,598,600,638,644,687,691,699,723,729,755,758,760,765,809,815,835,838,841,844,847,868,920,945,949,964,975)

MedicalNoC<- c(53,55,60,63,66,67,68,72,74,76,79,81,84,87,90,96,99,101,103,122,125,148,151,156,159,176,179,182,185,188,192,195,198,201,203,206,282,285,287,290,293,295,298,301,303,305,307,310,316,370,373,376,379,382,384,387,390,392,395,434,437,440,443,446,534,536,538,541,544,547,550,552,554,556,558,561,563,566,594,596,599,601,603,605,607,639,641,645,684,688,690,692,693,694,696,700,724,728,730,756,759,761,775,810,812,816,834,835,836,839,842,845:848,866,869,871,872,897,914,916,918,921,923,933,946,948,950,965,976,977)

data[DRG %in% SurgMCC.CC, DRG.Complication:=as.factor("SurgMCC.CC")]
data[DRG %in% SurgNoC, DRG.Complication:=as.factor("SurgNoC")]
data[DRG %in% MedicalMCC.CC, DRG.Complication:=as.factor("MedicalMCC.CC")]
data[DRG %in% MedicalNoC, DRG.Complication:=as.factor("MedicalNoC")]
data[is.na(DRG.Complication),DRG.Complication:=as.factor("Other")]

# 8. Budilding Logistic Regression --------------------------------
# subset important variables to build the model
final<- data[,c(1,4,7:8,101:106)]
# make sure the response variable is categorical
final$Readmission.Status<-as.factor(final$Readmission.Status)
head(final)
#write.csv(final, file = "Readmit_R.csv")

# split data into training and test set, using training set to build model and test set to validate the model
set.seed(1)
# 70% of data as training set
train <- sample(1:nrow(final),46747) 
final.train<-final[train,] 
final.test<- final[-train,]
head(final.test)

# proportional binomial/logit model:
fit.train<-glm(Readmission.Status~ Age + Gender + LOS + HCC.Riskscore + Race + DRG.Class + ER,family="binomial",data=final.train)
summary(fit.train)
# this model is built on the training set

# Variable selection and model selection:

# stepwise regression using backward elimination method (without any interaction terms)
fit2<- step(fit.train,direction="backward")
# investigate interaction terms in the model
library("MASS")
fit3<- update(fit2,.~.^2)
summary(fit2)

# Using chi-square test to perform deviance analysis:
anova(fit2,fit3,test="Chi") #--> prefer the reduced model: fit2
anova(fit2,fit.train,test="Chi") #--> prefer the reduced model: fit3

# fit this model on the test set:
fitpreds = predict(fit2,newdata=final.test,type="response")

# Ramzi
install.packages("ggplot2")
library(ggplot2)
head(final)
fit.data<-glm(Readmission.Status~ Age + Gender + LOS + HCC.Riskscore + Race + DRG.Class + DRG.Complication + ER,family="binomial",data=final)
fit2<- step(fit.data,direction="backward")
summary(fit2)
final$preds <- 1 - predict(fit2,final,type="response")
head(final)
hist(final$HCC.Riskscore)
ggplot(final, aes(x = Readmission.Status, y = HCC.Riskscore)) + geom_bar(stat = "identity")

# 9. Cutoff value and related plots -------------------------------
# determine the optimal cutoff value (where sensitivi-ty==specificity):
library("ROCR")
fitpredsk<- prediction(fitpreds,final.test$Readmission.Status)
t<- performance(fitpredsk,"ppv")
k<-unlist(t@x.values)
k2<-unlist(t@y.values)

y<- as.numeric(final.test$Readmission)-1
perf = function(cut, fitpreds,y)
{
  yhat = (fitpreds>cut) ## logical value: TRUE or FALSE if predicted prob. >cutoff
  w = which(y==1) #index of true population of readmission cases
  sensitivity = mean( yhat[w] == 1 ) # probability of readmission given that the patient is readmitted
  specificity = mean( yhat[-w] == 0 ) # probability of no readmis-sion given that the patient is not readmitted
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
  colnames(out) = c("sensitivity", "specificity", "c.rate", "dis-tance")
  return(out)
}

s = seq(.001,.99,length=1000)
OUT = matrix(0,1000,4)
for(i in 1:1000) OUT[i,]=perf(s[i],fitpreds,y)
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=8,axes=FALSE,col=2,
     main="Fit 2")
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=2)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=2)
lines(s,OUT[,2],col="darkgreen",lwd=8)
# lines(k,k2,lwd=2,col="black")
box()
leg-end(.25,.8,col=c(2,"darkgreen"),cex=1,lwd=c(3,3,3,3),c("Sensitivity","Specificity"))
abline(v=0.11,lty=3,lwd=2)
abline(0,1,lty=2)
points(.11,0.6638,pch=19,lwd=10)
## The intersection between sensitivity and specificity curves is 0.11

# obtain ROC curve for this model:
plot(1-OUT[,2],OUT[,1],main="ROC Curve",
     xlab=c("1-Specificity"), ylab="Sensitivity",
     type="l",lwd=10,col="orange")
abline(0,1)
# obtain c-statistic or area under the curve:
(c.stat<- performance(fitpredsk,measure="auc")@y.values)

# 10. Model performance by quantiles ------------------------------# Find the quantiles and the mean of prediction within each quantile:
quan<- quantile(fitpreds,c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
# mean prediction within each quantile:
mean<- c()
for (i in 2:11){
  mean[i-1]<- mean(fitpreds[(fitpreds>= quan[i-1])&(fitpreds<= quan[i])])
}

# actual cases of readmission within each quantiles:
actualOut<- c()
for (i in 2:11){
  actualOut[i-1]<- length(which((fitpreds>= quan[i-1]) & (fitpreds<= quan[i]) & actual==1))
}

# number of observations in each quantile:
num<- c()
for (i in 2:11){
  num[i-1]<- length(fitpreds[(fitpreds>= quan[i-1])&(fitpreds<= quan[i])])
}

# predicted outcomes:
predictedOut<- mean*num

## Model Performance by Quantiles Plot:
actual<- c(32,62,104,136,189,228,348,335,466,649)
predicted<- c(103,118,131,146,165,192,230,292,406,745)
plot(actual,type="l",lwd=6,col="orange",
     xlim=c(0,10),xaxt = "n",xlab="Quantiles",
     ylim=c(0,800),ylab="Number of outcomes",
     cex.lab=1)
grid()
points(predicted,type="l",lwd=6,col="darkturquoise")
axis(1, at=1:10, labels=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9,1),lwd=4)
axis(2,lwd=4)
title("Model Performance by Quantiles")
box()
leg-end(1,400,col=c("orange","darkturquoise"),cex=1,lwd=c(2,2,2,2),c("Actual","Predicted"))


