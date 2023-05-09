###################################### LUCY GITIRIA ############################
# PACKAGES 
library(EdSurvey)
library(data.table)
library(Hmisc)

#setwd("D:/MA ED Measurement Evaluation and Assessment")

## DATA
#Downloading data
#downloadTALIS(years = 2018, root = "C:/", cache=FALSE)

#Reading the data
FIN18 <- readTALIS("../TALIS/2018",
                 countries = c("fin"),isced = "b",dataLevel = "teacher",
                 forceReread = FALSE,verbose = TRUE)
### EXPLORE DATA
FIN18
colnames(x = FIN18)
dim(FIN18)
#Show code book
View(showCodebook(FIN18))

#Show weights 
showWeights(data = FIN18, verbose = TRUE)

#Show plausible values
showPlausibleValues(data = FIN18, verbose = TRUE)

#Variables of interest 
searchSDF('t3team', data=FIN18)  #Team innovativeness / Scalar (All)
searchSDF('t3satat', data=FIN18) #Satisfaction with target class autonomy/Metric (All)
searchSDF('tt3g11b', data=FIN18) #Experiences As a teacher in total
searchSDF('tt3g01', data=FIN18)  #Gender - T

#Descriptive statistics
summary2(FIN18, "t3team") #collective innovativeness
summary2(FIN18, "t3satat")#Teacher autonomy
summary2(FIN18, "tt3g11b")# teaching experience
summary2(FIN18, "tt3g01")# gender

#Correlations
cor.sdf("t3team","t3satat",data=FIN18, method='Pearson')
#0.06957806 (0.22)[0.02533851, 0.1135456]

cor.sdf("t3team","tt3g11b",data=FIN18, method='Pearson')
#0.09846029 (0.21)[0.05605754, 0.1405085]

#### REGRESSION ANALYSIS 
#RQ:Is there an association between collective teacher innovativeness and 
#teacher autonomy in Finish schools?

#Model 1
sf <- lm.sdf(formula = t3team ~ t3satat, data = FIN18)#single predictor-outcome relation. 
summary(sf)
# Multiple R-squared: 0.0048

#Model 2
mf <- lm.sdf(formula = t3team ~ tt3g01 + t3satat, data = FIN18)#one control variable
summary(mf)
#Multiple R-squared: 0.0057

#Final Model
hf <- lm.sdf(formula = t3team ~ tt3g01+ tt3g11b + t3satat, data = FIN18)#two control
summary(hf)

#Multiple R-squared: 0.0154
#Beta Coefficients
#tt3g01MALE  -0.1198802  (0.0748697)     
#tt3g11b      0.0176114  (0.0037445) ***
#t3satat      0.0619431  (0.0191576) **

####################################### END ###################################