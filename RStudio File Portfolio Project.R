
#CA Test Score Analysis

library("dplyr")

setwd("C:/R")


mydata <- read.csv("R Ready Dataset.csv") #import data file

df <- mydata[,c(6:7)] #isolating 6th and 8th grade mean scores
School68ScoreAvg <- apply(X=df, MARGIN = 1, FUN=mean, na.rm = TRUE) #Deriving mean with missing values ignored

df <- mydata[,c(9:10)] #isolating 2011 and 2012 avg income 'serves as past (5 yrs) income to derive a trend
Zip20112012AvgIncome <- apply(X=df, MARGIN = 1, FUN=mean, na.rm = TRUE) #Deriving mean with missing values ignored

df <- mydata[,c(11:12)] #isolating 2016 and 2017 avg income 'serves as present income
Zip20162017AvgIncome <- apply(X=df, MARGIN = 1, FUN=mean, na.rm = TRUE) #Deriving mean with missing values ignored

Zip5YrTrendAvgIncome <- (Zip20162017AvgIncome-Zip20112012AvgIncome)/Zip20112012AvgIncome # Calculate Percent change from 5 years ago for average income
Zip20162017AvgIncome <-Zip20162017AvgIncome/1000

#moving onto suicide and homicide counts
df <- mydata[,c(13:14)] #isolating 2010 and 2011 homicide +suicide count 'serves as 'past' (5 yrs) count to derive a tren
Zip20102011AvgCideCount <- apply(X=df, MARGIN = 1, FUN=mean, na.rm = TRUE) #Deriving mean with missing values ignored

df <- mydata[,c(15:16)] #isolating 2015 and 2016 homicide +suicide count 'serves as 'current' (5 yrs) count to derive a trend and signify present state
Zip20152016AvgCideCount <- apply(X=df, MARGIN = 1, FUN=mean, na.rm = TRUE) #Deriving mean with missing values ignored

mydata$Total.population <- mydata$Total.population/1000
Zip20102011AvgCideCountPerKCapita <- Zip20102011AvgCideCount / mydata$Total.population # Establish 'past' homicide and suicide per capita'
Zip20152016AvgCideCountPerKCapita <- Zip20152016AvgCideCount / mydata$Total.population # Establish 'current' homicide and suicide per capita'

Zip5YrDeltaAvgCideCountPerKCapita <- (Zip20152016AvgCideCountPerKCapita-Zip20102011AvgCideCountPerKCapita) #change from 5 years ago for homicide/suicide per capita
#Cannot use %change for trend variable as many zipcodes have 0 in 'old' value which makes % change valuation result in an error.

mydata2 <- cbind(mydata[,c(1:5,17:19)],School68ScoreAvg,Zip20162017AvgIncome,Zip5YrTrendAvgIncome, 
                 Zip20152016AvgCideCountPerKCapita,Zip5YrDeltaAvgCideCountPerKCapita) #Creating new dataframe 

#Replicate/duplicate rows so that each row represents an individual student score.  The score will remain the same (the mean of the group), but the 
                ##weighting across schools will be appropriate 
                ###Credit technique to Adam Erickson (2018) Stackoverflow.com retrieved from 
                ####https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame/52305196#52305196?newreg=97a0150be1bc41e78d8bff3ea585e409
mydata3 <- as.data.frame(lapply(mydata2, rep, mydata2$Students.with.Scores))

#Create dummy variables for the different demographic names
#Adding 0 to convert from True/False to 1/0
Male <- mydata3$Demographic.Name=='Male'
Military <- mydata3$Demographic.Name=='Military'
Disability <- mydata3$Demographic.Name=='Students with disability'
#This field is treated differently as it is leveraged by the ethnicity subgroup AND the ethnicity for econ advant/disadvantaged
EconDisadvantaged <- mydata3$Demographic.Name=='Economically disadvantaged' | mydata3$Student.Group=='Ethnicity for Economically Disadvantaged'

#Ethnicity Subgroups
BlackorAfricanAmerican <- mydata3$Demographic.Name =='Black or African American'
AmericanIndianorAlaskaNative <- mydata3$Demographic.Name =='American Indian or Alaska Native'
Asian <- mydata3$Demographic.Name =='Asian'
Filipino <- mydata3$Demographic.Name =='Filipino'
HispanicorLatino <- mydata3$Demographic.Name =='Hispanic or Latino'
NativeHawaiianorPacificIslander <- mydata3$Demographic.Name =='Native Hawaiian or Pacific Islander'
White <- mydata3$Demographic.Name =='White'
Multi<- mydata3$Demographic.Name == 'Two or more races'

#Parent Education sub groups
Notahighschoolgraduate <- mydata3$Demographic.Name =='Not a high school graduate'
Highschoolgraduate <- mydata3$Demographic.Name =='High school graduate'
Somecollege <- mydata3$Demographic.Name =='Some college (includes AA degree)'
Collegegraduate <- mydata3$Demographic.Name =='College graduate'
Graduateschool <- mydata3$Demographic.Name =='Graduate school/Post graduate'

Sys.Date()

#Ethnicity & Econ Subgroups
BlackDisadvantaged <- BlackorAfricanAmerican & EconDisadvantaged
BlackAdvantaged <- BlackorAfricanAmerican & !EconDisadvantaged

AmericanIndianDisadvantaged <- AmericanIndianorAlaskaNative & EconDisadvantaged
AmericanIndianAdvantaged <- AmericanIndianorAlaskaNative & !EconDisadvantaged

AsianDisadvantaged <- Asian & EconDisadvantaged
AsianAdvantaged <- Asian & !EconDisadvantaged

FilipinoDisadvantaged <- Filipino & EconDisadvantaged
FilipinoAdvantaged <- Filipino & !EconDisadvantaged

HispanicDisadvantaged <- HispanicorLatino & EconDisadvantaged
HispanicAdvantaged <- HispanicorLatino & !EconDisadvantaged

HawaiianDisadvantaged <- NativeHawaiianorPacificIslander & EconDisadvantaged
HawaiianAdvantaged <- NativeHawaiianorPacificIslander & !EconDisadvantaged

WhiteDisadvantaged <- White & EconDisadvantaged
WhiteAdvantaged <- White & !EconDisadvantaged

MultiDisadvantaged <- Multi & EconDisadvantaged
MultiAdvantaged <- Multi & !EconDisadvantaged

mydata4 <- cbind(mydata3,Male,Military,Disability,EconDisadvantaged,BlackorAfricanAmerican,AmericanIndianorAlaskaNative,Asian,Filipino,
                 HispanicorLatino,NativeHawaiianorPacificIslander,White,Notahighschoolgraduate,Highschoolgraduate,Somecollege,
                 Collegegraduate,Graduateschool,BlackDisadvantaged,BlackAdvantaged,AmericanIndianDisadvantaged,AmericanIndianAdvantaged,
                 AsianDisadvantaged,AsianAdvantaged,FilipinoDisadvantaged,FilipinoAdvantaged,HispanicDisadvantaged,HispanicAdvantaged,
                 HawaiianDisadvantaged,HawaiianAdvantaged,WhiteDisadvantaged,WhiteAdvantaged,MultiDisadvantaged,MultiAdvantaged,HawaiianMixed)

#Create student group specific data frames to avoid double counting across groups (and remove student count now being represented by row count)
##These groups are MUTUALLY EXCLUSIVE data rows, but represent the same students placed in different buckets.  Only one grouping should be used at once
###The number of rows will be different as the subgroups different way of 'slicing' the students resulted in different small groups that had to be removed/hidden due to privacy concerns.
mydataAllStudents <- mydata4[mydata4$Student.Group=='All Students',-4]
mydataGender <- mydata4[mydata4$Student.Group=='Gender',-4]
mydataEconomicStatus <- mydata4[mydata4$Student.Group=='Economic Status',-4]
mydataMilitaryStatus <- mydata4[mydata4$Student.Group=='Military Status',-4]
mydataDisabilityStatus <- mydata4[mydata4$Student.Group=='Disability Status',-4]
mydataParentEducation <- mydata4[mydata4$Student.Group=='Parent Education',-4]
mydataEthnicity <- mydata4[mydata4$Student.Group=='Ethnicity',-4]
mydataEthnicityAdvantaged <- mydata4[mydata4$Student.Group=='Ethnicity for Economically Disadvantaged'|mydata4$Student.Group=='Ethnicity for Not Economically Disadvantaged',-4]

summary(mydataEthnicityAdvantaged$Mean.Scale.Score)

Sys.Date()

#Histogram to illustrate distribution of test score
hist(mydataEthnicityAdvantaged$Mean.Scale.Score,breaks=seq(2300,2800,30),col="gray",main="7th Grade Math CA Test Score", xlab="Test Score")
datalines <-c(as.numeric(quantile(mydataEthnicityAdvantaged$Mean.Scale.Score),.25))
datalines <- datalines[2:4]
abline(v=c(mean(mydataEthnicityAdvantaged$Mean.Scale.Score),datalines),lty=c(1,2,3,4),lwd=2)
legend("topright",legend=c("Mean","1st Qtr","Median","3rd Qtr"),lty=c(1,2,3,4), lwd=2)

#Gender Single Variable Linear Regression
Genderlm <- lm(Mean.Scale.Score ~ Male, data = mydataGender)
summary(Genderlm)
Sys.Date()

#Economic Status Single Variable Linear Regression
EconomicStatuslm <- lm(Mean.Scale.Score ~ EconDisadvantaged, data = mydataEconomicStatus)
summary(EconomicStatuslm)
Sys.Date()

#Military Status Single Variable Linear Regression
MilitaryStatuslm <- lm(Mean.Scale.Score ~ Military, data = mydataMilitaryStatus)
summary(MilitaryStatuslm)
Sys.Date()

#Disability Status Single Variable Linear Regression
DisabilityStatuslm <- lm(Mean.Scale.Score ~ Disability, data= mydataDisabilityStatus)
summary(DisabilityStatuslm)
Sys.Date()

#ParentEducation Multi Variable Linear Regression
#Not including Declined to state, that will be represented by solely the y-intercept (all dummy variables equal to zero)
ParentEducationlm <- lm(Mean.Scale.Score ~ Notahighschoolgraduate + Highschoolgraduate + Somecollege + Collegegraduate + Graduateschool, data = mydataParentEducation)
summary(ParentEducationlm)
Sys.Date()

#Ethnicity Multi Variable Linear Regression
#Not including Multi-race, that will be represented by solely the y-intercept (all dummy variables equal to zero)
Ethnicitylm <- lm(Mean.Scale.Score ~ BlackorAfricanAmerican + AmericanIndianorAlaskaNative + Asian + Filipino + HispanicorLatino + NativeHawaiianorPacificIslander + White, data = mydataEthnicity)
summary(Ethnicitylm)
Sys.Date()

#Ethnicity AND Economically Advantage Multi Variable Linear Regression
#Not including Multi-advantaged, that will be represented by solely the y-intercept (all dummy variables equal to zero)
EthnicityEconlm <- lm(Mean.Scale.Score ~ BlackDisadvantaged + BlackAdvantaged + AmericanIndianDisadvantaged + AmericanIndianAdvantaged + AsianDisadvantaged + AsianAdvantaged + FilipinoDisadvantaged + FilipinoAdvantaged + 
                HispanicDisadvantaged + HispanicAdvantaged + HawaiianDisadvantaged + HawaiianAdvantaged + WhiteDisadvantaged + WhiteAdvantaged + MultiDisadvantaged, data = mydataEthnicityAdvantaged)
summary(EthnicityEconlm)
Sys.Date()

#Confidence Interval visualizations demonstrated an overlap between the Hawaiian/EconomicallyAdvantaged 
  ##and Hawaiian/EconomicallyDisadvantaged when analyzed with all ethnicity/econ groups
#Performing a two sample t-test of the mean scores to confirm the conclusions
HawaiianAdvantagedScores <- subset(mydataEthnicityAdvantaged, HawaiianAdvantaged==TRUE, select = "Mean.Scale.Score")
HawaiianDisadvantagedScores <- subset(mydataEthnicityAdvantaged, HawaiianDisadvantaged==TRUE, select = "Mean.Scale.Score")

TTestRes <- t.test(HawaiianAdvantagedScores,HawaiianDisadvantagedScores,var.equal = TRUE)
TTestRes

#Total Sample Mean (restricted to school subgroups qualified under ethnicity and economic status cross matrix)
mean(mydataEthnicityAdvantaged$Mean.Scale.Score)

Sys.Date()

#Calculating the total student (rows) count by subgroup 
##This will be leveraged in Tableau as the bar width
BlackAdvantagedCount <- sum(mydataEthnicityAdvantaged$BlackAdvantaged)
BlackDisadvantagedCount <- sum(mydataEthnicityAdvantaged$BlackDisadvantaged)
AmericanIndianAdvantagedCount <- sum(mydataEthnicityAdvantaged$AmericanIndianAdvantaged)
AmericanIndianDisadvantagedCount <- sum(mydataEthnicityAdvantaged$AmericanIndianDisadvantaged)
AsianAdvantagedCount <- sum(mydataEthnicityAdvantaged$AsianAdvantaged)
AsianDisadvantagedCount <- sum(mydataEthnicityAdvantaged$AsianDisadvantaged)
FilipinoAdvantagedCount <- sum(mydataEthnicityAdvantaged$FilipinoAdvantaged)
FilipinoDisadvantagedCount <- sum(mydataEthnicityAdvantaged$FilipinoDisadvantaged)
HispanicAdvantagedCount <- sum(mydataEthnicityAdvantaged$HispanicAdvantaged)
HispanicDisadvantagedCount <- sum(mydataEthnicityAdvantaged$HispanicDisadvantaged)
HawaiianAdvantagedCount <- sum(mydataEthnicityAdvantaged$HawaiianAdvantaged)
HawaiianDisadvantagedCount <- sum(mydataEthnicityAdvantaged$HawaiianDisadvantaged)
WhiteAdvantagedCount <- sum(mydataEthnicityAdvantaged$WhiteAdvantaged)
WhiteDisadvantagedCount <- sum(mydataEthnicityAdvantaged$WhiteDisadvantaged)
MultiAdvantagedCount <- sum(mydataEthnicityAdvantaged$MultiAdvantaged)
MultiDisadvantagedCount <- sum(mydataEthnicityAdvantaged$MultiDisadvantaged)

CountFrame <- c(BlackDisadvantagedCount,BlackAdvantagedCount,AmericanIndianDisadvantagedCount, AmericanIndianAdvantagedCount,AsianDisadvantagedCount, AsianAdvantagedCount, 
                 FilipinoDisadvantagedCount, FilipinoAdvantagedCount, HispanicDisadvantagedCount, HispanicAdvantagedCount, HawaiianDisadvantagedCount, HawaiianAdvantagedCount,
                 WhiteDisadvantagedCount, WhiteAdvantagedCount, MultiDisadvantagedCount, MultiAdvantagedCount)
CountFrame2<- cbind(c("BlackDisadvantagedCount","BlackAdvantagedCount","AmericanIndianDisadvantagedCount", "AmericanIndianAdvantagedCount","AsianDisadvantagedCount", 
                "AsianAdvantagedCount", "FilipinoDisadvantagedCount", "FilipinoAdvantagedCount", "HispanicDisadvantagedCount", "HispanicAdvantagedCount", 
                  "HawaiianDisadvantagedCount", "HawaiianAdvantagedCount", "WhiteDisadvantagedCount", "WhiteAdvantagedCount","MultiDisadvantagedCount", "MultiAdvantagedCount"), CountFrame)
CountFrame2

##Moving forward with the Race/Economic usage of the demographic field.  Time to add the other variables
#First thing to check is for high correlation between independent variables (collinearity)
CorrCheck <- mydataEthnicityAdvantaged[,5:12]
CorrMatrix <- cor(CorrCheck, use = "complete.obs")
CorrMatrix <- round(abs(CorrMatrix),2)
CorrMatrix

Sys.Date()

'Split out the Enthnicity Advantaged data set into train and test sample (80%/20%)'
set.seed(123)
sample.size <- floor(.8 * nrow(mydataEthnicityAdvantaged))
train.index <- sample(seq_len(nrow(mydataEthnicityAdvantaged)),size = sample.size)
mydataTestEthnicityAdvantaged <- mydataEthnicityAdvantaged[- train.index,]
mydataTrainEthnicityAdvantaged <- mydataEthnicityAdvantaged[train.index,]

#Reduce Scientific Notation
options(scipen = 3)

'Multi-variate linear regression model'
MultiVarLM <- lm(Mean.Scale.Score ~ BlackDisadvantaged + BlackAdvantaged + AmericanIndianDisadvantaged + AsianDisadvantaged + AsianAdvantaged + FilipinoDisadvantaged + FilipinoAdvantaged + 
              HispanicDisadvantaged + HispanicAdvantaged + HawaiianDisadvantaged + HawaiianAdvantaged + WhiteDisadvantaged + WhiteAdvantaged + MultiDisadvantaged + Total.population + Median.age..years. + Average.Household.size + 
              School68ScoreAvg + Zip20162017AvgIncome + Zip5YrTrendAvgIncome + Zip20152016AvgCideCountPerKCapita + Zip5YrDeltaAvgCideCountPerKCapita, data = mydataTrainEthnicityAdvantaged)

summary(MultiVarLM)

Sys.Date()

#Time to apply the testing dataset to the final model'
PredictedTestData <- predict(MultiVarLM,mydataTestEthnicityAdvantaged, interval = "prediction")

#Adding the actuals and renaming the columns'
results <- cbind(mydataTestEthnicityAdvantaged$Mean.Scale.Score,PredictedTestData)
names(results) <- c("Actual","Predicted","LowerBoundPred","HigherBoundPred")

#calculating the residuals and identifying the actuals within the predicted interval
Residual <- results[,1] - results[,2]
WithinPredInterval <- results[,1] > results[,3] & results[,1] < results[,4]
results2 <- cbind(results,Residual,WithinPredInterval)
results3 <- results2[complete.cases(results2),]

#Percentage of test dataset that fell within predicted interval bounds
mean(results3[,6])

Sys.Date()


