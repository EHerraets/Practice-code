# Practice-code

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