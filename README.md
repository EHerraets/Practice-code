# Practice-code

# 01 introduction
CEO_Diary <- read.csv("~/Desktop/Data Science/GitHub/Data/survey_response_data.csv", header=FALSE)
View(CEO_Diary)

CEO_Diary[1:15,c(1:5,37, 39, 40)] 
CEO_Diary[1:15,c(9:15,17, 29, 31)] 

#apply function testing
apply(CEO_Diary,2,class)
apply(CEO_Diary, 2, sort)
apply(CEO_Diary, 2, table)

nrow(CEO_Diary)
ncol(CEO_Diary)
summary(CEO_Diary[1:5])
summary(CEO_Diary)

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

#check individual lines
barplot(prop.table(table(CEO_Diary$type)))
table(CEO_Diary$type)
prop.table(table(CEO_Diary$type))

#make the variables numeric
str(CEO_Diary)
any(is.na(CEO_Diary))
CEO_Diary$strategy <- as.numeric(CEO_Diary$strategy)
CEO_Diary$consultants <- as.numeric(CEO_Diary$consultants)
CEO_Diary$politicians <- as.numeric(CEO_Diary$politicians)

fit <- glm(strategy ~ consultants + politicians, data=CEO_Diary); summary(fit)
