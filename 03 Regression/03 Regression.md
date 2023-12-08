# Practice-code

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
