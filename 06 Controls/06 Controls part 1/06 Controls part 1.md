# Practice-code

# 06 Controls part 1
oj <- read.csv("~/Desktop/Data Science/GitHub/Data/oj.csv", header = TRUE)
basefit <- lm(log(sales) ~ log(price), data=oj)
#regression where sales is dependent and price is independent variable, using the data from data set oj
coef(basefit)
#give the intercept and slope coefficient of the regression

brandfit <- lm(log(sales) ~ brand + log(price), data=oj)
#regression where sales is dependent and price and brand are independent variable, using the data from data set oj
coef(brandfit)
#give the intercept and slope coefficient of the regression

pricereg <- lm(log(sales) ~ brand, data=oj)
#regression where sales is dependent and brand is independent variable, using the data from data set oj
phat <- predict(pricereg, newdata=oj)
#generates the predicted value based on the model pricereg, using the data from the data set oj
presid <- log(oj$price) - phat
#calculates the residual
residfit <- lm(log(sales) ~ presid, data=oj)
#regression where sales is dependent and residual (presid) is independent variable, usint the data from the data set oj
coef(basefit)
#give the intercept and slope coefficient of the regression

data <- read.table("~/Desktop/Data Science/GitHub/Data/abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')
#give the column names
data <- data[!(data$state%in%c(2,9,12)),] 
#remove the rows from the dataset where the values of state variable is 2, 9 or 12
data <- data[data$year>84 & data$year<98,] 
#only select rows where the year is between 1984 and 1998
data$pop <- log(data$pop)
#takes the logarithm of pop and put it in a new variable
t <- data$year - 85
#represents the time
s <- factor(data$state) 
#new factor vairable where states are numbered alphabetically
controls <- data.frame(data[,c(3,10:17)])
#use column 3, 10 and 17 as controls
y <- data$y_murd
#represent the outcome variable related to mudered
d <- data$a_murd
#represent some predictor/treatment varaiable

orig <- glm(y ~ d + t + s +., data=controls)
summary(orig)$coef['d',]
#summary of the coefficient d with the coeffiecient (-2.09), se (4.1), t value (-5.1) and significanty
dcoef <- summary(orig <- glm(y ~ d + t + s +., data=controls) )$coef['d',][1]
exp(dcoef) - 1
#estimated percent change in the odds of the events y associated with a one-unit increase in the variable d -> -0.18

cell <- read.csv("~/Desktop/Data Science/GitHub/Data/us_cellphone.csv")
cellrate <- 5*cell[,2]/(1000*cell[,3]) 
#calculates: 5 times the data in the second column of cell data set divided by 1000 times the third column of cell data set

phone <- cellrate[ t + 1 ]
#vector of values in cellrate at position t+1
tech <- summary(glm(y ~ phone + t + s +., data=controls))$coef['phone',]
#vector with the summary of the regression of y on phone, t, s and other variables
phonecoef <- tech[1]
#numeric value of the coefficient for the variable phone
exp(phonecoef) - 1
#estimated percentage chang in the odds of the event y associated with a one-unit increase in the variable phone, which is -0.31

t <- factor(t)
interact <- glm(y ~ d + t + phone*s + .^2, data=controls)
summary(interact)$coef["d",]

library(gamlr)
sna <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
#factor vector based on the orginal factor variable s ???
x <- sparse.model.matrix( ~ t + phone*sna + .^2, data=controls)[,-1]
#create a sparse matrix of the values displayed and it takes out the intercept column of the matrix
dim(x)
#624 rows ad 143 columns

naive <- cv.gamlr(cbind(d,x),y)
#makes a cross valuation regression
head(coef(naive))
#give the first observations of naive

coef(naive)["d",]
#gives the coefficienbt of the threatment variable d from the model naive, which gives -0.09

treat <- cv.gamlr(x,d, lmr=1e-3)
#makes a cross valuation regressoion
head(summary(treat))
#give the first observations of summary of treat
predtreat <- predict(treat, x, select="min")
#predictions based on the model with the minimum/least cross validated errors
head(predtreat)
#give the first observations of predtreat
dhat <- drop(predtreat)
#drops dimensions from the prediction
length(dhat)
#calculated the length of the vector dhat

par(mai=c(.9,.9,.1,.1))
plot(dhat,d,bty="n",pch=21,bg=8, cex=.8, yaxt="n")
axis(2, at=c(0,1,2,3))
#plot

cor(drop(dhat),d)^2
#calculates the squared correlation between dhat and d (drops removes the dimentions so there is only a one level before caculating the correlation), which is 0.985
coef(summary( glm( y ~ d + dhat) ))
#gives the coefficient summary of the regression of y on d and dhat
causal <- cv.gamlr(cbind(d,dhat,x),y,free=2,lmr=1e-3)
#fits a penalized logisitic regression, using treatment varaibble d on predicted treatment dhat and other covariances x
coef(causal, select="min")["d",]
#coefficient of the treatment variable d from the penalized logistic regression, which is 0.018

library(gamlr)
data(hockey)
head(goal, n=2)
#information about goal hometeam, season, away team name, home team name, game period, goal differential and indicator for playoffs
player[1:2, 2:7] 
#players on ice. +1 is home players. 0 is off ice.
team[1, 2:6] 
#shows the different teams in that season
config[5:6, 2:7] 
#sparse matrix of the team formation, . for not true, 1 for true

x <- cbind(config,team,player)
#matrix of these variables combined
y <- goal$homegoal
#takes the variables of the homegoal column out of the goal data set and puts it into vector y
fold <- sample.int(2,nrow(x),replace=TRUE)
#creates vector that assigns each observation in the data set (randomly) to one or two fold for cross validation
head(fold)
#shows the first observations of the vector fold

nhlprereg <- gamlr(x[fold==1,], y[fold==1],free=1:(ncol(config)+ncol(team)), family="binomial", standardize=FALSE)
#fitting penalized logistic regression of the training set, fitted to the subset of the design matrix x where fold=1 and response variable is y
selected <- which(coef(nhlprereg)[-1,] != 0)
#shows non-zero coefficients in the fitted model above
xnotzero <- as.data.frame(as.matrix(x[,selected]))
#new data frame that only contains the values of the matrix x corresponding to the selected non-zero coefficients
nhlmle <- glm( y ~ ., data=xnotzero, subset=which(fold==2), family=binomial)
#regression to test fold2 using the nonzero dataframe
summary(nhlmle)
#gives the intercept, standard deviation, t value, and significanty level

x[1,x[1,]!=0] 
#check first observation for players on the ice, which elements are non-zero in the first row of matrix x and return those values
fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$fit; fit
#predicted probabilities for the first observation in the new data frame xnotzero ???, which is 0.47
se.fit <- predict(nhlmle, xnotzero[1,,drop=FALSE], type="response", se.fit=TRUE)$se.fit; se.fit
#gives the standard error for predicted probabilites of the first observation in the new data frame xnotzero, which is 0.06
CI = fit + c(-2,2)*se.fit; CI
#lower bound of the confidence interval is 0.354 and the upper bound of the confidence interval is 0.599
