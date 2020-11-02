# IBM attrition Project code

library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
# Create Subsets of Data
# Exploratory Data Analysis
# Linear Discriminant Analysis
# Canonical Correlation Analysis
# Principal Component Analysis

emp = read.csv(file="IBMattrition.csv", header=TRUE, sep=",")
head(emp) 
summary(emp)
str(emp) 

empOriginal <- emp

# Convert categorical variables into numeric.
emp$Attrition <- as.numeric(emp$Attrition)
emp$BusinessTravel <- as.numeric(emp$BusinessTravel)
emp$Department <- as.numeric(emp$Department)
emp$EducationField <- as.numeric(emp$EducationField)
emp$EnvironmentSatisfaction <- as.numeric(emp$EnvironmentSatisfaction)
emp$Gender <- as.numeric(emp$Gender)
emp$JobRole <- as.numeric(emp$JobRole)
emp$MaritalStatus <- as.numeric(emp$MaritalStatus)
emp$OverTime <- as.numeric(emp$OverTime)


# Subsets of data

# This is the subset of data variables that will be used for Exploratory data analysis, Linear Regression, and Linear Discriminant Analysis
emp2 <- emp[,c(1:8, 11:21, 23:26, 28:35)]

# Two subsets of datasets for Canonical Correlation Analysis (CCA).
worklife <-   emp[c( 24, 29, 32, 33, 34, 35)]  
jobGains <-   emp[c(4, 6, 13, 19, 20, 21)] 

# This is the subset of data that contains only continuous and discrete variables to be used for PCA.
empPCA <- emp[c(1, 4, 6, 7, 11, 13:15, 17, 19:21, 24:26, 28:35)] 


######## General Exploratory Data Analysis ########
# boxplots, histograms, correlations, scatterplots

library(ggplot2)
ggplot(empOrignial, aes(as.factor(PerformanceRating), MonthlyIncome , fill = as.factor(Attrition))) + geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.8, alpha=0.9) + theme_minimal() +
  theme (
    legend.position = "right",
    plot.title = element_text(size=11)
  ) +
  labs(title = "Does employee satisfaction change with monthly income and does it affect attrition?", 
                     x = "Performance Rating", y = "Monthly Income", fill = "Attrition") 

ggplot(empOrignial, aes(as.factor(JobRole), MonthlyIncome , fill = as.factor(Attrition))) + geom_boxplot() + 
   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.8, alpha=0.9) + theme_minimal() +
  theme (
    legend.position ="right",
    plot.title = element_text(size=11)
    ) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) + 
  labs(title = "Does monthly income based on job role affect employee attrition?",
       x = "Job Role", y = "Monthly Income", fill = "Attrition")

boxplot(MonthlyRate~JobSatisfaction,data = emp, main="boxplot",
        xlab="Job Satisfaction Level", ylab="Monthly Rate", col = "cyan")

boxplot(HourlyRate~WorkLifealance,data = emp, main="boxplot",
        xlab="jobGains", ylab="work life balance", col = "cyan")

boxplot(YearsAtCompany~JobSatisfaction,data = emp, main="boxplot",
        xlab="satisfaction", ylab="yrs at company", col = "cyan")
# Histograms
ggplot(emp, aes(x = HourlyRate, y = len)) + geom_boxplot() + 
  geom_point() 
hist(emp2$HourlyRate, col = 'light blue')
hist(emp2$JobSatisfaction, col = 'light blue')
hist(emp2$EnvironmentSatisfaction, col = 'light blue')
hist(emp2$PercentSalaryHike, col = 'light blue')

# scatterplots
ggplot(emp2, aes(x=YearsAtCompany, y = JobSatisfaction)) + geom_point()
ggplot(emp2, aes(x=YearsAtCompany, y = PercentSalaryHike)) + geom_point()
ggplot(emp2, aes(x=JobSatisfaction, y=Attrition)) + geom_jitter()

str(emp)
summary(emp)

# correlations / corplot
a = cor(emp2, method="spearman")
a
library(corrplot)
corrplot(a)

# linear regression for job satisfaction and attrition
model1 = lm(formula= JobSatisfaction ~., data=emp2)
summary(model1) 

model2 = lm(Attrition ~ ., data = emp2)
summary(model2)

#check for multicollinearity
library(DescTools) #VIF Function
VIF(model1)  # Job satisfaction
VIF(model2)  # attrition


######### Linear Discriminant Analysis of job satisfaction ########
# With Cross Validation
# The independent and dependent variables are categorical
empLDA <- lda(Attrition ~ ., data=emp2, CV=TRUE)
empLDA

#To Plot the Data, you cannot use CV
empLDA <- lda(Attrition ~ ., data=emp2)
empLDA

plot(empLDA, xlab = "LD1", ylab = "LD2")

# Try to predict the class from the original data
# Note ... this is JUST a test to see how this works
# In practice you will want to use cross-validation!
p = predict(empLDA, newdata=emp2[,-15])$class    
p

# Compare the results of the prediction (Confusion Matrix)
table(p, emp2$JobSatisfaction)

accuracy <- (52+6+205+272)/(52+6+205+272+197+152+7+135+125+8+28+109+32+3+22)
accuracy
# 0.3954176

mean(p== emp2$JobSatisfaction)
# 0.3863936


# Creating Training and Testing Samples
require(caTools)  
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(emp2, SplitRatio = 0.70) 
train =subset(emp2,sample ==TRUE) # creates a training dataset
test=subset(emp2, sample==FALSE)

library(MASS)

# The dependent variable must be categorical (Assuming No Cross-Validation)
empLDA = lda(Attrition ~ ., data=train)
empLDA

plot(empLDA)



########## Canonical Correlation Analysis ##################

summary(worklife)

ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

# worklife and hobbies_interest dataframes

# This gives us the cannonical correlates, but no significance tests
c = cancor(worklife, jobGains)
c

#Breakdown of the Correlations
matcor(worklife, jobGains)

#Correlations 
cc_mm = cc(worklife, jobGains)
cc_mm$cor

#Functions for CCA
ls(cc_mm)

#XCoef Correlations
cc_mm$xcoef

#YCoef Correlations
cc_mm$ycoef

#Calculate Scores
loadings_mm = comput(worklife, jobGains, cc_mm)
ls(loadings_mm)

#Correlation X Scores
loadings_mm$corr.X.xscores

#Correlation Y Scores
loadings_mm$corr.Y.yscores

#Wilk's Lambda Test
wilks_mm = ccaWilks(worklife, jobGains, cc_mm)
round(wilks_mm, 2)

# Now, calcualte the standardized coefficients
s1 = diag(sqrt(diag(cov(worklife))))
s1 %*% cc_mm$xcoef

s2 = diag(sqrt(diag(cov(jobGains))))
s2 %*% cc_mm$ycoef

# A basic visualization of the cannonical correlation
plt.cc(cc_mm)


library(yacca)
library(CCA)

c2 = cca( worklife, jobGains)
c2

#CV1
helio.plot(c2, cv=1, x.name="worklife values", 
           y.name="jobGains values")

#CV2
helio.plot(c2, cv=2, x.name="worklife values", 
           y.name="jobGains values")





########### Principal Component Analysis #########
empPCA <- emp[c(1, 4, 6, 7, 11, 13:15, 17, 19:21, 24:26, 28:35)] 

a = prcomp(empPCA, center=T, scale=T)

# Scree Plot
plot(a)
abline(1, 0)

library(factoextra)
a2 <- prcomp(empPCA, scale = TRUE) 
fviz_eig(a2)

# PCA Summary
summary(a)
print(a)

# PCA Analysis with varimax rotation and 4 loadings
a3 = psych::principal(empPCA, rotate="varimax", nfactors = 4, scores=TRUE)
a3
print(a3$loadings, cutoff = .4, sort=T)

# visualization of PCA loadings
library(gridExtra)
for (i in 1:4){
  nam <- paste("plot_", i, sep = "")
  temp_loadings_data = data.frame(label=colnames(empPCA),loading= a3$loadings[,i])
  plot_temp = ggplot(data=temp_loadings_data,aes(x=label,y=loading)) + geom_bar(stat="identity",fill="cyan3") +
    coord_flip() + theme_dark() +  theme(axis.text.x = element_text(size=3))
  assign(nam, plot_temp) 
  
}
grid.arrange(plot_1,plot_2,plot_3,plot_4,ncol=2,nrow=2) 
