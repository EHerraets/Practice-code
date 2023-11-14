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
#linear model/regression of spam on every other variable, and makes family a binomial variable so it can be measured.

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
