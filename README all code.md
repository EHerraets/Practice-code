# Practice-code

# 01 introduction
CEO_Diary <- read.csv("~/Desktop/Data Science/GitHub/Data/survey_response_data.csv", header=FALSE)
View(CEO_Diary)
#How to select/import my data from my files to R studio

CEO_Diary[1:15,c(1:5,37, 39, 40)] 
CEO_Diary[1:15,c(9:15,17, 29, 31)]
#The different rows and columns you can select to show as results

#apply function testing
apply(CEO_Diary, 2, class)
apply(CEO_Diary, 2, sort)
apply(CEO_Diary, 2, table)
#Applying a function to margins of the array

nrow(CEO_Diary)
ncol(CEO_Diary)
summary(CEO_Diary[1:5])
summary(CEO_Diary)
#Count rows and columns and give summary from variables

#making column names
my.colnames <- CEO_Diary[1,]
colnames(CEO_Diary) <- my.colnames
CEO_Diary=CEO_Diary[-c(1),]
View(CEO_Diary)
 
#check working directory
 getwd()
 
#Create 'figs' directory if it doesn't exist
if (!file.exists("figs")) {
  dir.create("figs")
}

 png(filename="/Users/ellenherraets/Desktop/Data Science/GitHub/Practice-code/figs/CEOTypes.png", width=800, height=300)
 par(mar=c(9, 3 ,1,1))
 barplot(prop.table(table(CEO_Diary$type)), las=2)
 dev.off()
#creating a barplot in a png file from a column of the data

#check what every individual line does
barplot(prop.table(table(CEO_Diary$type)))
table(CEO_Diary$type)
prop.table(table(CEO_Diary$type))
#making a table of the different types, create a % of how often each type is found in the dataset

#make the variables numeric to make them 'countable'
str(CEO_Diary)
any(is.na(CEO_Diary))
CEO_Diary$strategy <- as.numeric(CEO_Diary$strategy)
CEO_Diary$consultants <- as.numeric(CEO_Diary$consultants)
CEO_Diary$politicians <- as.numeric(CEO_Diary$politicians)

fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); summary(fit)
#fit the generalized linear model, strategy is DV and consultans + politicians is IV 

# 02 Uncertainty
browser <- read.csv("~/Desktop/Data Science/GitHub/Data/web-browsers.csv", header=TRUE)
dim(browser)
#download the data set into R

#Frequentist
mean(browser$spend)
var(browser$spend)/1e4
var(browser$spend)
var(browser$spend)/1e8
sqrt(var(browser$spend)/1e4)
#the 1e4 explains where you put the comma

#Bootstrap
  B <- 1000
  mub <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    mub <- c(mub, mean(browser$spend[samp_b]))
  }
  sd(mub)
  
  samp_b <- sample.int(nrow(browser), replace=TRUE)
  print(samp_b)
   
   mub <- c(mub, mean(browser$spend[samp_b]))
   print(mub)

 C <- 5 
  muc <- c()
  for (c in 1:5){
    samp_c <- sample.int(nrow(browser), replace=TRUE)
    muc <- c(muc, mean(browser$spend[samp_c]))
  }
  sd(muc)
#Big C is the times you loop the proces; for (c in 1:5) implies what you are going to loop; sample.int is the random sample you run; sd is the standard deviation; so if C is smaller the sd becomes smaller
  
   D <- 10000
  mud <- c()
  for (d in 1:10000){
    samp_d <- sample.int(nrow(browser), replace=TRUE)
    mud <- c(mud, mean(browser$spend[samp_d]))
  }
  sd(mud)
#if d is larger the sd becomes larger
  
    A <- 1000
  mua <- c()
  for (a in 1:1000){
    samp_a <- sample.int(nrow(browser), replace=FALSE)
    mua <- c(mua, mean(browser$spend[samp_a]))
  }
  sd(mua)
#replacing true with false, the sampling will now be without replacement -> each element is selected only once; sd is zero implies that all the samples means are identical; the data and sampling process reamin the same so you get the same set of 'random' variables. As a result the means calculated from these unique indices will be identical in each interation. 

#making a histogram
hb <- hist(mub)
  xfit <- seq(min(mub), max(mub), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(hb$mids[1:2]) * length(mub) 
#creating histogram of spend; xfit is the min and max values of x axis; yfit creating the corresponding values of the y axis of the variables from the x axis; dnorm is normal distribution

lines(xfit, yfit, col = "black", lwd = 2)
#drawing a normal distribution line

hc <- hist(muc)
  xfit <- seq(min(muc), max(muc), length = 40) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(hc$mids[1:2]) * length(muc) 
 
  lines(xfit, yfit, col = "red", lwd = 4)
#col is color, lwd is the thickness of the line

hd <- hist(mud)
  xfit <- seq(min(mud), max(mud), length = 100) 
  yfit <- dnorm(xfit, mean = mean(browser$spend), sd = sqrt(var(browser$spend)/1e4)) 
  yfit <- yfit * diff(hd$mids[1:2]) * length(mud) 
 
  lines(xfit, yfit, col = "orange", lwd = 0.5)

#Bootstrapping regression
 B <- 1000
  betas <- c()
  for (b in 1:1000){
    samp_b <- sample.int(nrow(browser), replace=TRUE)
    reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
    betas <- rbind(betas, coef(reg_b))
  }; head(betas, n=3)
  
  cov(betas[,"broadband"],betas[,"anychildren"])
  
 BR <- 10
  betas <- c()
  for (br in 1:10){
    samp_br <- sample.int(nrow(browser), replace=TRUE)
    reg_br <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_br,])
    betas <- rbind(betas, coef(reg_br))
  }; head(betas, n=3)
  
cov(betas[,"broadband"],betas[,"anychildren"])
#making the loop smaller the cov is also becoming much smaller

BR2 <- 10
  betas <- c()
  for (br2 in 1:10){
    samp_br2 <- sample.int(nrow(browser), replace=TRUE)
    reg_br2 <- glm(spend ~ broadband + anychildren, data=browser[samp_br2,])
    betas <- rbind(betas, coef(reg_br2))
  }; head(betas, n=3)
#Have to set the DV in the log, otherwise you get way to high numbers

#algorithm example 1
browser2 <- read.csv("~/Desktop/Data Science/GitHub/Data/web-browsers.csv", header=TRUE)

#Create 'figs' directory if it doesn't exist
if (!file.exists("figs")) {
  dir.create("figs")
}

spendy <- glm(log(spend) ~ . -id, data=browser2)
#regression of spend (DV) on every other variable except id
  round(summary(spendy)$coef,2)
#round the variable up to 2 decimals
  pval <- summary(spendy)$coef[-1, "Pr(>|t|)"]
#giving the p values and  removes the first column (the intercept) from the summary that is shown
  pvalrank <- rank(pval)
#rank the p values so can compare the significance 
  reject <- ifelse(pval< (0.1/9)*pvalrank, 2, 1) 
#reject every p value that is less/lower than (0.1/9) * the rank. If the element is TRUE, indicating that the corresponding p-value is below the threshold, the corresponding element in reject is set to 2; if the element is FALSE, indicating that the corresponding p-value is not below the threshold, the corresponding element in reject is set to 1.
  png(filename="/Users/ellenherraets/Desktop/Data Science/GitHub/Practice-code/figs/AlgoritmeE1.png",
  width=600, height=350)
#where to save the graph in my files 
  plot(pvalrank, pval, ylab="p-value", xlab="p-value rank", pch=16, col=reject)
#plotting the graph; on the y axis the p value and the x axis the rank of the p value. The color of each dot depends on if it was rejected or not
  lines(pvalrank, (0.1/9)*pvalrank)
#plot a line of the p value ranks
  dev.off()

#algorithm example 2
SC <- read.csv("~/Desktop/Data Science/GitHub/Data/semiconductor.csv", header=TRUE)
dim(SC)
#download the data set into R; dim function is to set the dimensions of a object such as a matrix or an array; giving 1477 observation of 201 variable

install.packages("logistf")
install.packages("glmnet")
full <- glm(FAIL ~ ., data=SC, family=binomial)
#regression of fail to all the other variables of data set SC; familiy=bionomial is two possible outcomes 0 or 1
pvals <- summary(full)$coef[-1,4] 

#histogram example 2
hist(pvals, xlab="p-value", main="Histo Algo 2", col="pink")

#graph example 2
fdr_cut <- function(pvals, q=0.1){
  pvals <- sort(pvals[!is.na(pvals)])
#sorts on p-value and remoces NA values
  N <- length(pvals)
  k <- rank(pvals, ties.method="min")
  alpha <- max(pvals[ pvals<= (q*k/(N+1)) ])
#calculates the threshold p value
  
  plot(pvals, log="xy", xlab="order", main=sprintf("FDR of %g",q),
#plot the graph
   ylab="p-value", bty="n", col=c(8,2)[(pvals<=alpha) + 1], pch=20)
#assigns color to dots whether the p-values is less or equal to alpha
  lines(1:N, q*(1:N)/(N+1))
#adds a line to the plot represnting FDR threshold
  return(alpha)
}
fdr_cut(pvals)
#test the false or postives among the rejected null hypotheses

# 03 Regression
oj <- read.csv("~/Desktop/Data Science/GitHub/Data/oj.csv", header=TRUE)
head(oj, n=12)
#gives the first 12 observation
tail(oj, n=9)
#gives the last 9 observation

glm(log(sales) ~ brand + log(price), data=oj)
#linear model of sales (DV) on brand and price(IVs)
#the results null deviance (goodness fit of a model) = 30080 residual deviance (how well the response variable predicted by the entire model) = 18220 -> for both lower values predict a better fit. Values high?

x <- model.matrix(~ brand + log(price), data=oj); head(x); tail(x)
#head(x) gives the first few rows/ tail(x) gives the last few rows

y <- model.matrix(~ brand + log(price), data=oj); head(oj, n=6); tail(oj, n=6)
#this does not work the same as the equation above

oj$brand = as.factor(oj$brand)
#convert the brand to a factor

oj$mybrand = relevel(oj$brand, "tropicana")
#set tropicana as a factor that is the reference level, thus when mybrand has new variables you interpret them in comparison with tropicana which is the base/reference line you draw?

z <- model.matrix(~ mybrand + log(price), data=oj); head(x)

glm(log(sales) ~ log(price)*brand*feat, data=oj)
#linear model of sales on interaction varaiable between price, brand and feat(ure) 

email <- read.csv("~/Desktop/Data Science/GitHub/Data/spam.csv", header=TRUE)
dim(email)

colnames(email)
#gives all the column names of the dataset

glm(spam ~ ., data=email, family='binomial')
#linear model/regression of spam on every other variable, and tells with family that the distribution is binomial (two possible outcomes) 

spammy <- glm(spam ~ ., data=email, family='binomial')
coef(spammy)["word_free"]; exp(coef(spammy)["word_free"])
#it gives the coefficent associated with variable "word_free" influence the outcome in the analysis (positive or negative); the second number gives the odds of something happening (or being spammy) is 4.6 as much when the word free is present compared when it is not
#how much the presence of the word free affects the likelihood of something being spammy. In this dataset it is 4.6 more likely to be spammy if the word free is in it

coef(spammy)["word_george"]; exp(coef(spammy)["word_george"]); 1/exp(coef(spammy)["word_george"]
#the 1/exp gives the opposite of exp -> so positive number makes it here less likely instead of more likely

coef(spammy)["word_business"]; exp(coef(spammy)["word_business"]); 1/exp(coef(spammy)["word_business"]
#does not work with every variable for example adress gives NA 

predict(spammy, newdata = email[c(1,4000),], type="response")
#makes a prediction based on the fitted model -> prediction on 1 and 4000; type = response -> gives the predicted probability/ likelihood of the observation belonging to being spammy

predict(spammy, newdata = email[c(4,99,1846),], type="response")
#the 4th email is almost certain spam, same for the 99th email. The 1846 is almost certain not spam

summary(spammy)$deviance
summary(spammy)$null.deviance
#null deviance reference point if you use no predictors. How lower the value how better thus because the deviance is lower than the null deviance it is better (fit to the model) to use predictors

D <- summary(spammy)$deviance; D
D0 <- summary(spammy)$null.deviance; D0
R2 <- 1 - D/D0; R2
#gives the R squared (how well the IV in the regression model explain the variablility  of the DV) => low R2 (close to 0) the model does not explain much of the variablility, high R2 (close to 1) does explain the variablility. In this model R2 is 75, this may considered high
Dtest <- summary(spammy$deviance); D
#now you get everything instead of only the deviance variable

# 04 Regularization
SC <- read.csv("~/Desktop/Data Science/GitHub/Data/semiconductor.csv", header=TRUE)
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance

#out of sample prediction experiment
deviance <- function(y, pred, family=c("gaussian","binomial")){
#makes a function of the observed variables (y), the predicted values (pred) and makes a vector of the distribution which will be normal or binomial
 family <- match.arg(family)
#selects the variables in the right distribution either gaussian or binomial
 if(family=="gaussian"){
 return( sum( (y-pred)^2 ) )
 }else{
#if the distribution is gaussian the function will return the sum of squared (observed values - predicted values) -> so predicts if this model is a good fit to the data
 if(is.factor(y)) y <- as.numeric(y)>1
#checks if the binomial is a factor or numeric value. When it is a factor, it changes it in a numeric value (either 0 or 1). So it can be measured.
 return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
#calculate the (log) likelihood contribution for each observation. because the observed value (y) is 0 or 1 you have to use y and the 1-y function, otherwise the predicts values of the observed value of 0 will not be considered into the formula. 
 }
}
#will do the same steps for the 0 deviance
R2 <- function(y, pred, family=c("gaussian","binomial")){
 fam <- match.arg(family)
 if(fam=="binomial"){
 if(is.factor(y)){ y <- as.numeric(y)>1 }
 }
#checks if the binomial is a factor or numeric value. When it is a factor, it changes it in a numeric value (either 0 or 1). So it can be measured.
 dev <- deviance(y, pred, family=fam)
 dev0 <- deviance(y, mean(y), family=fam)
 return(1-dev/dev0)
#calculates the R squared with the deviance and the null deviance calculated above

#setup the experiment
n <- nrow(SC) 
#the number of observations
K <- 10 
#the number of `folds'=> divided the data set into 10 roughly equal parts/folds

foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]
#repeat the values of the folders 1 to K (10) and the ceiling explain how many times it should be repeated -> n/K is that it is repeated enough times that all the variables (n) are covered. and sample randomly assigns the order of the fold

Out <- data.frame(full=rep(NA,K))
#creates a data frame with 1 column names full and the values of the vector NA,K which is a vector of K length and each element/row is empty 

for(k in 1:K){ 
#create loop from 1 to number of folds

 train <- which(foldid!=k) 
#train on all but fold `k'

 rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
#regression of fail on all variables, where you only use the data from subset train
 

 predfull <- predict(rfull, newdata=SC[-train,], type="response")
#predict the values of the regression rfull, from which the data was not in the data set train and give the predicted values back in the probabilities (type=response)

 Out$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
#the data frame 'out' column 'full' fold 'k' <- R2 is the R squared variable we calculated before, the observed values (y) are in the dataset SC column 'fail' excluding the training set observation; pred is the predicted probabilites for the validation set; with a binomial distribution.

 cat(k, " ")
#cat function is to join together objects to the console -> you can print multiple objects. Here we want the print the number of k followed by a space 
 
}
colMeans(Out)
boxplot(Out, col="plum", ylab="R2")


#setup the experiment playing around with the code
n <- nrow(SC)
T <- 4
foldid <- rep(1:T)[sample(1:n)]
Test <- data.frame(full=rep(NA,T)) 
for(k in 1:T){ 
	train2 <- which(foldid!=t) 
	rfull2 <- glm(FAIL~., data=SC, subset=train2, family=binomial)
	predfull2 <- predict(rfull2, newdata=SC[-train2,], type="response")
	Test$full[k] <- R2(y=SC$FAIL[-train2], pred=predfull2, family="binomial")
	cat(k, " "); Test
colmeans(Test)
boxplot(Test, col="plum", ylab="R2")


null <- glm(FAIL~1, data=SC)
#regression only for the intercepts and without predictor variables
fwd <- step(null, scope=formula(full), dir="forward")
#forward stepwise regression 
length(coef(fwd))

null2 <- glm(FAIL~1, data=SC)
bwd <- step(null2, scope=formula(full), dir="backward")
length(coef(bwd))
#backward stepwise regression
null3 <- glm(FAIL~1, data=SC)
bothwd <- step(null2, scope=formula(full), dir="both")
length(coef(bothwd))
#both directions stepwise regression

install.packages("gamlr")
library(gamlr)
web <-read.csv("~/Desktop/Data Science/GitHub/Data/browser-domains.csv", header=TRUE)
sitenames <- scan("~/Desktop/Data Science/GitHub/Data/browser-sites.txt", what = "character")
#incidates/scans the data which variable are character and makes a vector of it in sitenames -> it succesfully read 1000 items
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
#signs the site from the database web as a factor. It assigns the factor to the right sitename label
web$id <- factor(web$id, levels=1:length(unique(web$id)))
#creates the similar thing as the previous line but now for the variable id
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
#vector with corresponding sum of the visitis with every specific website id
visitpercent <- 100*web$visits/machinetotals[web$id]
#vector with the percentages of the previous code line
xweb <- sparseMatrix(
	i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
	dims=c(nlevels(web$id),nlevels(web$site)),
	dimnames=list(id=levels(web$id), site=levels(web$site)))
#matrix containing the vectorvisitpercent. the rows corresponend to id and columns correspond to site

yspend <- read.csv("~/Desktop/Data Science/GitHub/Data/browser-totalspend.csv", row.names=1)  
yspend <- as.matrix(yspend)
#value yspend will be replaced with a matrix representation

spender <- gamlr(xweb, log(yspend), verb=TRUE)
#gives the relationship between xweb and the logaritm of yspend

plot(spender)
#plot the value spender

cv.spender <- cv.gamlr(xweb, log(yspend))
plot(cv.spender)
#gives the cross-validation results 

betamin = coef(cv.spender, select="min")
#betamin contain the coefficients of the gam corresponding to the model complexity -> perfomed the best terms of cross-validated

head(AIC(spender))
#a lower AIC value indicate better fitting model. with the values I can decide/ make a decision between model fit and complexity


# 05 Classification

install.packages("MASS")
library(MASS)
data(fgl)
dim(fgl)
#214 rows and 10 columns
 head(fgl, n = 2)
#shows values of first tow rows of fgl dataset

par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6), las=2)
#plotting the different tipes 

x <- scale(fgl[,1:9]) 
#column 10 is class label, scale converts to mean 0 sd 1
apply(x,2,sd) 
#apply function sd to columns of x

library(class) 
#has knn function
test <- sample(1:214,10) 
#draws a random sample of 10 rows
nearest1 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=1) 
#this vector will contain the predicted class labels for the test set
nearest2 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=2)
nearest3 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=3)
nearest4 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=4)
nearest5 <- knn(train=x[-test,], test=x[test,], cl=fgl$type[-test], k=5)
#creating a vector for 5 others
data.frame(fgl$type[test],nearest1,nearest2, nearest3, nearest4, nearest5)
#shows in a data frame the 5 different knns with of the 10 different rows

credit <- read.csv("~/Desktop/Data Science/GitHub/Data/credit.csv", header = TRUE)
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
#history variable in the credit data set will be converted to a factor
levels(credit$history) = c("good","good","poor","poor","terrible")
#will give the history factor variable different 'scores' 
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
#factor vector of the foreign in the credit data set, levels are set to A201 or A202 with corresponding labels foreign and german
credit$rent <- factor(credit$housing=="A151")
#factor vector variable for the rent -> will show each observation that is in the category A151 of the housing variable with the label TRUE otherwise it gets the label FALSE
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
#the purpose variable in the data set credit will be converted to the levels
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")
#now the purpuse variable is updated to new/different levels
credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]
#data set will be modified to include only the columns listed

head(credit)
#shows the first few rows of the dataset
dim(credit)
#data set obtains 1000 rows and 9 columns

library(gamlr)
credx <- sparse.model.matrix(Default ~ . ^ 2, data=naref(credit)); colnames(credx)
#sparse matrix representation of credit model

default <- credit$Default
#variable contain the values of default column
credscore <- cv.gamlr(credx, default, family="binomial")
#cross-validation performance of regression -> gives information of the cross-validated performance ot hte model

par(mfrow=c(1,2))
#plots displayed side by side
plot(credscore$gamlr)
plot(credscore)
#displays the different plots

sum(coef(credscore, s="min")!=0)
#calculate the count of non-zero coefficients in the model, which are 85
sum(coef(credscore$gamlr)!=0)
#calculate the count of non-zero coefficients in the regression model, which are 90
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0)
#calculate the count of non-zero coefficients in the regression model and minimizes the AIC > select the model with the best fit and complexity, here it is 140

1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]
#?? outcome is 0.1827528

pred <- predict(credscore$gamlr, credx, type="response")
#calculates the predicted values for the new data provided in credx
pred <- drop(pred) 
#removes the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))
#plot

rule <- 1/5 
rule2 <- 1/87 
rule3 <- 1/456 
#gives the outcome of this equation
sum( (pred>rule)[default==0] )/sum(pred>rule) 
#false positive rate at 1/5 rule, 0.5379538
sum( (pred<rule)[default==1] )/sum(pred<rule) 
#false negative rate at 1/5 rule, 0.05076142

sum( (pred>rule2)[default==0] )/sum(pred>rule2) 
#false positive rate at 1/87 rule, 0.7
sum( (pred<rule2)[default==1] )/sum(pred<rule2) 
#false negative rate at 1/87 rule, NaN?

sum( (pred>rule3)[default==0] )/sum(pred>rule3) 
#false positive rate at 1/456 rule, 0.7
sum( (pred<rule3)[default==1] )/sum(pred<rule3) 
#false negative rate at 1/456 rule, NaN?

sum( (pred>rule)[default==1] )/sum(default==1) 
#sensitivity (the ability of model to correcly indentify true positives), 0.9333333
sum( (pred>rule2)[default==1] )/sum(default==1) 
#sensitivity, 1
sum( (pred>rule3)[default==1] )/sum(default==1) 
#sensitivity, 1

sum( (pred<rule)[default==0] )/sum(default==0) 
#specificity (the ability of modle to cerrecly indentify true negatives), 0.5342857 
sum( (pred<rule2)[default==0] )/sum(default==0) 
#specificity, 0
sum( (pred<rule3)[default==0] )/sum(default==0) 
#specificity, 0

test <- sample.int(1000,500)
#generates random sample of 500 between 1 and 1000
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
#regresion
predoos <- predict(credhalf, credx[test,], type="response")
#predictions generated out of sample data using the previous regression
defaultoos <- default[test]
#vector with true labels for the out-of-sample data

install.packages("glmnet")
library(glmnet)
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] 
#Design matrix includes chemical composition variable
gtype <- fgl$type
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") 
#cross validation experiments glassfit

plot(glassfit)
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4))
plot(glassfit$glm, xvar="lambda")
#plots

B <- coef(glassfit, select="min") 
#extract coefficients
B <- do.call(cbind, B) 
colnames(B) <- levels(gtype) 
#column names dropped in previous command. This command adds them back.

DeltaBMg <- B["Mg", "WinNF"] - B["Mg", "WinF"]; DeltaBMg 
#Represent the difference between the element in the Mg row and WinNF column and the element in the Mg row and WinF in matrix B, which is -0.44 
exp(DeltaBMg);
#represent the odds of the event, which is 0.64
1 - exp(DeltaBMg)
#represent the odds of the complementary event, which is 0.36

probfgl <- predict(glassfit, xfgl, type="response") 
#matrix where each row corresponds to an observation and column correspons to a class -> variable gives the predicted probability for each class for each observation
dim(probfgl)
#gives the number of rows 214and colums 6, and there is one layer
head(probfgl,n=2)
#gives the first 2 observations
tail(probfgl,n=2)
#gives the last 2 observations

probfgl <- drop(probfgl)
#remove other dimensions -> only 1 dimension left
n <- nrow(xfgl)
#calculates the number of rows in the matrix
trueclassprobs <- probfgl[cbind(1:n, gtype)]
#vector containing the predicted probabilities of the true classs for each observation
head(trueclassprobs,n=3)
##gives the first 3 observations
tail(trueclassprobs,n=3)
#gives the last 3 observations

plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
	xlab="glass type", ylab="prob( true class )") 
#plot

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
 
# 06 Controls part 2

install.packages("AER")
library(AER)
library(gamlr)
dreg <- function(x,d){ cv.gamlr(x, d, lmr=1e-5) }
#cross validated penalized logisitc regression of the matrix x and d
yreg <- function(x,y){ cv.gamlr(x, y, lmr=1e-5) }
#cross validated penalized logisitc regression of the matrix x and y


orthoLTE <- function(x, d, y, dreg, yreg, nfold=2)
{
#???
 nobs <- nrow(x)
#creates vector for number of observation/rows in matrix x
 foldid <- rep.int(1:nfold, times = ceiling(nobs/nfold))[sample.int(nobs)]
 I <- split(1:nobs, foldid)
#randomly assigns observations to folds

 ytil <- dtil <- rep(NA, nobs)
#capture the difference between the observed and predicted values
 cat("fold: ")
 for(b in 1:length(I)){
#starts a loop
 dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
 yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
 dhat <- predict(dfit, x[I[[b]],], type="response")
 yhat <- predict(yfit, x[I[[b]],], type="response")
 dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
 ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
 cat(b," ")
 }
#loop iterates over each fold, fits penalized logistic regression model, predicted values, calculates rsiduals and prints the fold index
 rfit <- lm(ytil ~ dtil)
#regression  where residual outcome ytil is regressed on residual treatment dtil
 gam <- coef(rfit)[2]
#gives the coeffficent of the estimated causal effect
 se <- sqrt(vcovHC(rfit)[2,2])
#calculates the standard error
 cat(sprintf("\ngamma (se) = %g (%g)\n", gam, se))
 return( list(gam=gam, se=se, dtil=dtil, ytil=ytil) )
#gives the estimated causal effect gam and its standard error se

#OrthoML and effect of abortion access on crime
resids <- orthoLTE( x=x, d=d, y=y, dreg=dreg, yreg=yreg, nfold=5)
#vector of the previous loop and set nfold to 5 so max 5 folds
head(resids$dtil)
#gives the first observations of residual treatment
head(resids$ytil)
#gives the first observations of residual outcome
2*pnorm(-abs(resids$gam)/resids$se) #p-value supports no effect of abortion access on crime
#calculates two sided p value for nul hypothesis testing, if gam (estimated causal effect) is equal to zero

install.packages("foreign")
library(foreign)
descr <- read.dta("~/Desktop/Data Science/GitHub/Data/oregonhie_descriptive_vars.dta")
prgm <- read.dta("~/Desktop/Data Science/GitHub/Data/oregonhie_stateprograms_vars.dta")
s12 <- read.dta("~/Desktop/Data Science/GitHub/Data/oregonhie_survey12m_vars.dta")

all(s12$person_id == descr$person_id)
all(s12$person_id == prgm$person_id)
#organize one row per person

P <- descr[,c("person_id","household_id", "numhh_list")]
#new data frame of selected column from the dataset descr
P$medicaid <- as.numeric(prgm[,"ohp_all_ever_firstn_30sep2009"]=="Enrolled")
#add a new column to the data set P, out of the existing dataset prgm 
P$selected <- as.numeric(descr[,"treatment"]=="Selected")
#add a new colmn to the data set P, gives the value 1 to the treatment (which was selected in the dataset) and 0 otherwise
levels(P$numhh_list) <- c("1","2","3+")
#gives levels (1, 2, 3+) to the factor variable numhh_list

Y <- s12[,c("weight_12m",
	"doc_any_12m","doc_num_mod_12m",
	"er_any_12m","er_num_mod_12m",
	"hosp_any_12m","hosp_num_mod_12m")]
#create new data frame y with selected columns
Y$doc_any_12m <- as.numeric(Y$doc_any_12m=="Yes")
#give 1 to row where value in doc_any_12m is yes, otherwise 0
Y$er_any_12m <- as.numeric(Y$er_any_12m=="Yes")
#give 1 to row where value in er_any_12m is yes, otherwise 0
Y$hosp_any_12m <- as.numeric(Y$hosp_any_12m=="Yes")
#give 1 to row where value in hosp_any_12m is yes, otherwise 0

X <- s12[,121:147]
#new data frame X by selected columns 121-147 of existing data frame s12
X$dt_returned <- factor(format(s12$dt_returned_12m, "%Y-%m"))
#new factor vector -> creating caterogries based on month and year of the value dt_returned_12m

insurv <- which(s12$sample_12m_resp == "12m mail survey responder")
#checks the rows in s12 where sample_12m_resp is equal to 12m mail survey
X <- X[insurv,]
Y <- Y[insurv,]
P <- P[insurv,]
#new data frames including only the rows corresponding to 12m mail survey responder

sapply(Y,function(y) sum(is.na(y)))
#applies a function to each column in the dataframe y, and counts the number of missing values in each column
nomiss <- which( !apply(Y,1, function(y) any(is.na(y))) )
#identify the rows in y where there are no missing values across all columns
X <- X[nomiss,]
Y <- Y[nomiss,]
P <- P[nomiss,]
#new data frames including only the rows without missing values

Y <- Y[,-1]
#removes the first column in data frame y
X$hhsize_12m[X$hhsize_12m>10] <- 10
#updates values in hhsize_12m in data frame x, every value above ten is set to ten
X$num19_12m <- NULL
#removes num19_12m from the dataframe

P$doc_any_12m <- Y$doc_any_12m 
#assigns the value from data frame y to data frame p
P <- P[,c(1,2,6,5,4,3,7)]
#reorder the columns of data frame p
names(P)[6] <- "numhh"
#update the name of the sixth column to numhh

head(P,n=3)
#shows the first 3 observation of data frame p
dim(P)
#gives the number of rows (23107) and columns (6)

ybar <- tapply(P$doc_any_12m, P$selected, mean)
#calculates the mean of doc_any_12m
( ATE = ybar['1'] - ybar['0'] )
#substracting the mean for the control group from the mean for the treatment group, which is 0.011
nsel <- table(P[,c("selected")])
#creates a table of frequencies for the selected variables in data frame p
yvar <- tapply(P$doc_any_12m, P$selected, var)
#calculates the variance of the doc_any_12m
( seATE = sqrt(sum(yvar/nsel)) )
#calculates the standard error of the ATE, which is 0.0115
ATE + c(-2,2)*seATE
#calculates a confidence interval for the ATE, add and substract 2 times the standard error from the ATE, lower bound is -0.012 and upper bound is 0.034

lin <- glm(doc_any_12m ~ selected + numhh, data=P)
#regression dependent variable doc_any_12m on independent variable selected and numhh of the data set P
round( summary(lin)$coef["selected",],4) 
#gives the coefficient of the selected variable out of the summary of the regression.

#could not find naref in sources and after a longs search not sure what to change to make the code working...
levels(X$edu_12m)
source("naref.R")
levels(naref(X$edu_12m))
X <- naref(X) 
xnum <- X[,sapply(X,class)%in%c("numeric","integer")]
xnum[66:70,]
colSums(is.na(xnum))
xnumna <- apply(is.na(xnum), 2, as.numeric)
xnumna[66:70,]
mzimpute <- function(v){
 if(mean(v==0,na.rm=TRUE) > 0.5) impt <- 0
 else impt <- mean(v, na.rm=TRUE)
 v[is.na(v)] <- impt
 return(v) }
xnum <- apply(xnum, 2, mzimpute)
xnum[66:70,]
#using this code to indicate the missing values and to replace them 

for(v in colnames(xnum)){
 X[,v] <- xnum[,v]
 X[,paste(v,"NA", sep=".")] <- xnumna[,v] }
X[144:147,]
#replace/add the variables in new data frame

xhte <- sparse.model.matrix(~., data=cbind(numhh=P$numhh, X))[,-1]
#creating sparse matrix using the data from data set p and x
xhte[1:2,1:4]
#show first two rows and first four colums of resulting sparse matrix
dim(xhte)
#2133 rows and 64 columns

dxhte <- P$selected*xhte
#multiplies each column of the sparse matrix by th value selected out of the p data frame
colnames(dxhte) <- paste("d",colnames(xhte), sep=".")
#renames the colums of modified matrix by d
htedesign <- cbind(xhte,d=P$selected,dxhte)
#combines three matrices into one

library(gamlr)
htefit <- gamlr(x=htedesign, y=P$doc_any_12m, free=c("numhh2","numhh3+","d"))
#nrow(htedesign), length(P$doc_any_12m) => different values htedesign contains 2113 and doc_any_12m 23107
gam <- coef(htefit)[-(1:(ncol(xhte)+1)), ]
round(sort(gam)[1:6],4)
round(sort(gam, decreasing=TRUE)[1:6],4)
#didnt work because could not make the htefit

load("~/Desktop/Data Science/GitHub/Data/dominicks-beer.rda")
head(wber)
#shows the first observations of data frame wber 
wber = wber[sample(nrow(wber), 100000), ]
#randomly samples 100000 rows from data frame wber
head(upc)
#shows the first observations of data frame wber
dim(upc)
#287 rows and 2 columns
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"]) 
#new variable lp in data frame wber with the values of the answer of the calculation corresponding to the upc data frame

coef( margfit <- lm(log(MOVE) ~ lp, data=wber[,]) )
#shows coefficient of the new variable margfit which is a regression of the dependent variale move on the independent variable lp and using the dataset wber

wber$s <- factor(wber$STORE)
#new factor variable s by converting existing store variable to a factor
wber$u <- factor(wber$UPC)
#new factor variable u by converting existing upc variable to a factor
wber$w <- factor(wber$WEEK)
#new factor variable w by converting existing week variable to a factor
xs <- sparse.model.matrix( ~ s-1, data=wber)
#creating sparse matrix -> s minus 1 for no intercept term
xu <- sparse.model.matrix( ~ u-1, data=wber)
#creating sparse matrix -> u minus 1 for no intercept term
xw <- sparse.model.matrix( ~ w-1, data=wber)
#creating sparse matrix -> w minus 1 for no intercept term

library(tm)
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
#convert the text into a format suitable for text mining, into data frame upc
descr <- DocumentTermMatrix(descr)
#creates document from the text corpus, represents the frequency of terms in the document
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), 
 dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))
#creates sparse matrix
descr[1:5,1:6]
#shows the first 5 rows and first 6 columns of the sparse matrix
descr[287,descr[287,]!=0]
#takes out the non-zero elements in the 287th row of the sparse matrix
controls <- cbind(xs, xu, xw, descr[wber$UPC,])
#new data frame combining all the columns
dim(controls) 
#1000000 rows and 851 columns

#naive lasso
naivefit <- gamlr(x=cbind(lp=wber$lp,controls)[,], y=log(wber$MOVE), free=1, standardize=FALSE)
#regression
print( coef(naivefit)[1:2,] )
#shows first 2 rows of the coefficient of the regression above

#interact items and text with price
lpxu <- xu*wber$lp
#multiplies xu by the column lp out of the data frame wber
colnames(lpxu) <- paste("lp",colnames(lpxu),sep="")
#gives new column names

xhte <- cbind(BASELINE=1,descr[wber$UPC,])
#new matrix combining column baseline with descr
d <- xhte*wber$lp
#multiplies xhte with coumn lp out of the data frame wber
colnames(d) <- paste("lp",colnames(d),sep=":")
#gives new column names
eachbeer <- xhte[match(rownames(upc),wber$UPC),]
#subset matrix by selecting rows from xhte and match it to upc codes in the wber data frame
rownames(eachbeer) <- rownames(upc)
#ensure row names of eachbeer are set to the row neames of the upc data frame 


lnwberMOVE <- log(wber[['MOVE']])
#calculates the logarithm of move variable of wber data frame
fullhte <- gamlr(x=cbind(d,controls), y=lnwberMOVE, lambda.start=0)
#regression
gamfull <- coef(fullhte)[2:(ncol(lpxu)+1),]
#extract coefficient from fullhte model
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])
#??

coef(fullhte)
#give the coefficient form the regression

hist(gamfull, main="", xlab="elasticity", col="darkgrey", freq=FALSE)
#makes histogram of results