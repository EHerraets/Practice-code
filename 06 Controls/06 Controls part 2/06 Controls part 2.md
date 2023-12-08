# Practice-code

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