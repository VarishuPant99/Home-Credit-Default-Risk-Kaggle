library(InformationValue)
library(caTools)
library(MLmetrics)
setwd("C:/Users/Varishu Pant/Desktop/Praxis docs/Financial Analytics")
df = read.csv("output_valid.csv")
set.seed(101)

zeros = optimalCutoff(df$Actual ,df$Predicted, "Zeros")
pred = ifelse(pred_prob > zeros, 1, 0)
pred_prob=df$Predicted
sensitivity(df$Actual, pred)
specificity(df$Actual, pred)
precision(df$Actual, pred)

ones = optimalCutoff(df$Actual ,df$Predicted, "Ones")
pred = ifelse(pred_prob > ones, 1, 0)
pred_prob=df$Predicted
sensitivity(df$Actual, pred)
specificity(df$Actual, pred)
precision(df$Actual, pred)

misclass = optimalCutoff(df$Actual ,df$Predicted, "misclasserror")
pred = ifelse(pred_prob > misclass, 1, 0)
pred_prob=df$Predicted
sensitivity(df$Actual, pred)
specificity(df$Actual, pred)
precision(df$Actual, pred)

both = optimalCutoff(df$Actual ,df$Predicted, "both")
pred = ifelse(pred_prob > both, 1, 0)
pred_prob=df$Predicted
sensitivity(df$Actual, pred)
specificity(df$Actual, pred)
precision(df$Actual, pred)

F1_Score=(2*precision(df$Actual, pred)*sensitivity(df$Actual, pred))/(precision(df$Actual, pred)+sensitivity(df$Actual, pred))




pred_prob=df$Predicted

pred = ifelse(pred_prob > 0.5, 1, 0)

table(df$Actual,pred)

sensitivity(df$Actual, pred)

#Specificity
specificity(df$Actual, pred)

#Precision
precision(df$Actual, pred)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(df$Actual, pred)

#Mis-classification Error
misClassError(df$Actual, pred)

?Concordance

Conc = Concordance(df$Actual, pred_prob)
Conc
C = Conc$Concordance
D = Conc$Discordance
T = Conc$Tied


#Goodman - Kruskal Gamma
#------------------------------------------------------------------------
gamma = (C-D)/(C+D+T)
gamma


#Somer D
#------------------------------------------------------------------------
D = (C-D)/(C+D)
D

#K-S Statistic
#------------------------------------------------------------------------
library(sqldf)
#Creating an initial table
df$X<-NULL
newdata = df[order(-df$Predicted), ]

nrow(newdata)/10

#Create the groups in using index
groups = rep(1:10,each=floor(nrow(newdata)/10)) 
extra = rep(10, nrow(newdata)-length(groups))   #Filling up the extras
groups = c(groups,extra)

#Attach the groups to the data
newdata$groups = groups
View(newdata)


#3B -> Creating a Gain Table
#--------------------------------------------------------------------------------------


#Calculate the number of Bads (or 1's) in each of the groups (keeping track of the total counts in each groups)
gainTable = sqldf("select groups, count(actual) as N, 
                  sum(actual) as N1 from newdata 
                  group by groups ")
class(gainTable)
View(gainTable)

#Calculate the cumulative sum of bads (or 1's)
gainTable$cumN1 = cumsum(gainTable$N1)


#Calculate the cumulative percentage of bads (or 1's)
gainTable$Gain = round(gainTable$cumN1/sum(gainTable$N1)*100,3)


#Calculate Cumulative Lift
gainTable$Lift = round(gainTable$Gain/((1:10)*10),3)


#Print the Gain Table
gainTable


#3C -> Plot the Cumulative Gain and Cumulative Lift Chart
#-------------------------------------------------------------------------------------

#Gain Chart
plot(gainTable$groups, gainTable$Gain, type="b", 
     main = "Gain Plot",
     xlab = "Groups", ylab = "Gain")



#Lift Chart
plot(gainTable$groups, gainTable$Lift, type="b", 
     main = "Lift Plot",
     xlab = "Groups", ylab = "Lift")



ks = sqldf("select groups, count(actual) as N, sum(actual) as N1, 
           count(actual)-sum(actual) as N0 from newdata group by groups ")

View(ks)


#Calculate Percentage Events and Non-Events
ks$PerN0 = round(ks$N0/sum(ks$N0)*100,2)
ks$perN1 = round(ks$N1/sum(ks$N1)*100,2)


#Calculate Cumulative Percentage of Events and Non-Events
ks$CumPerN0 = cumsum(ks$PerN0)
ks$CumPerN1 = cumsum(ks$perN1)


#Calculation of KS
ks$KS = abs(ks$CumPerN0 - ks$CumPerN1)


#Print the Table
ks

plot(ks$groups, ks$CumPerN0, type="l", col = "Green")
lines(ks$groups, ks$CumPerN1, col = "Red")


#Hosmer - Lemeshow Goodness of Fit
#------------------------------------------------------------------------
library(ResourceSelection)
hoslem.test(newdata$Actual, newdata$Predicted, g=10)
ks_plot(newdata$Actual, newdata$Predicted)
ks_stat(newdata$Actual, newdata$Predicted)
