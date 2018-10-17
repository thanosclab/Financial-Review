#Basic: fin <- read.csv("Future 500.csv")
fin <- read.csv(file.choose(), na.strings=c(""))
fin
head(fin, 20)
tail(fin, 10)
summary(fin)
str(fin)

#Changing from non-factor to factor:
fin$ID <- factor(fin$ID)
fin$ID
is.factor(fin$ID)
summary(fin)
str(fin)

fin$Inception <- factor(fin$Inception)
summary(fin)
str(fin)


#Factror Variable Trap (FVT)
#Converting into Numerics For Characters
a <- c("12", "13", "14", "12", "12")
a
typeof(a)
b <- as.numeric(a)
b
typeof(b)

#Converting into Numerics For factors
z <- factor(c("12", "13", "14", "12", "12"))
z
str(z)
typeof(z)
y <- as.numeric(z)
y
typeof(y)

#------- Correct way:
x <- as.numeric(as.character(z))
x
typeof(x)


#sub() and gsub()


fin$Expenses <- gsub("Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
head(fin)
str(fin)


fin$Revenue <- gsub("\\$", "",fin$Revenue)
fin$Revenue <- gsub(",", "",fin$Revenue)
head(fin)

fin$Growth <- gsub("%", "", fin$Growth)
head(fin)
str(fin)


fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
str(fin)
summary(fin)


#------- Dealing with Misising Data:

#-------- Locate missing Data
#Updated import to: fin <- read.csv("Future 500.csv", na.strings=c(""))

head(fin,24)
fin[!complete.cases(fin),]

str(fin)


#Filtering: using which() for non-missing data
head(fin)
fin[fin$Revenue == 9746272,]

which(fin$Revenue == 9746272)
?which()
fin[which(fin$Revenue == 9746272),]

head(fin)
fin[fin$Employees == 45,]
fin[which(fin$Employees == 45),]


#Filterin: using is.na() for missing data

head(fin, 24)

fin$Expenses == NA
fin[fin$Expenses == NA,]

?is.na()

a <- c(1, 24, 543, NA, 76, 54, NA)
is.na(a)

is.na(fin$Expenses)
fin[is.na(fin$Expenses),]


is.na(fin$State)
fin[is.na(fin$State),]


#Removing records with missing data
fin_backup <- fin


fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin[!is.na(fin$Industry),] #opposite
fin <- fin[!is.na(fin$Industry),]
fin

fin[!complete.cases(fin),]

#Reseting the dataframe index
fin
rownames(fin) <- 1:nrow(fin)
fin

fin
rownames(fin) <- NULL
fin

#-----------Replacing Missing Data: Factual Analysis
fin[!complete.cases(fin),]

fin[is.na(fin$State),]

fin[is.na(fin$State) & fin$City =="New York",]
fin[is.na(fin$State) & fin$City =="New York", "State"] <- "NY"
#check:
fin[c(11,377),]

fin[!complete.cases(fin),]
fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City =="San Francisco",]
fin[is.na(fin$State) & fin$City =="San Francisco", "State"] <- "CA"
fin[c(82,265),]

#Replacing Missing Data: Median Imputatuin Method(part1)
fin[!complete.cases(fin),]

median(fin[,"Employees"], na.rm=TRUE)
#mean(fin[,"Employees"], na.rm=TRUE)
med_empl_retail <- median(fin[fin$Industry=="Retail","Employees"], na.rm=TRUE)
#mean(fin[fin$Industry=="Retail","Employees"], na.rm=TRUE)
med_empl_retail

fin[is.na(fin$Employees) & fin$Industry=="Retail",]
fin[is.na(fin$Employees) & fin$Industry=="Retail", "Employees"] <- med_empl_retail
#check
fin[3,]

med_empl_finserv <- median(fin[fin$Industry=="Financial Services","Employees"], na.rm=TRUE)
med_empl_finserv
fin[is.na(fin$Employees) & fin$Industry=="Financial Services",]
fin[is.na(fin$Employees) & fin$Industry=="Financial Services", "Employees"] <- med_empl_finserv
#check
fin[330,]

#Replacing Missing Data: Median Imputatuin Method(part2)

fin[!complete.cases(fin),]

med_growth_constr <- median(fin[fin$Industry=="Construction","Growth"], na.rm=TRUE)
med_growth_constr
fin[is.na(fin$Growth) & fin$Industry=="Construction",]
fin[is.na(fin$Growth) & fin$Industry=="Construction", "Growth"] <- med_growth_constr
#check
fin[8,]

fin[!complete.cases(fin),]
med_rev_constr <- median(fin[fin$Industry=="Construction", "Revenue"], na.rm = TRUE)
med_rev_constr
fin[is.na(fin$Revenue) & fin$Industry=="Construction",]
fin[is.na(fin$Revenue) & fin$Industry=="Construction", "Revenue"] <- med_rev_constr
#check
fin[8,]
fin[42,]

fin[!complete.cases(fin),]
med_exp_constr <- median(fin[fin$Industry=="Construction", "Expenses"], na.rm = TRUE)
med_exp_constr
fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit), "Expenses"] <- med_exp_constr
#check
fin[8,]
fin[42,]


#Replacing Missing DataQ Deriving values
#Revenue - Expenses = Profit
#Expenses = Revenue - Profit

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
fin[c(8,42),]

fin[!complete.cases(fin),]
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]
fin[15,]

fin[!complete.cases(fin),]

#------------Visualization
library(ggplot2)
#A scaterplot classified by Industry showing Revenue, Expenses, Profit

p <- ggplot(data=fin)
p
p + geom_point(aes(x=Revenue, y=Expenses,
                   colour=Industry, size=Profit))


#A scatterplot that includes industry trends for the expenses~revenue relationship
d <- ggplot(data=fin, aes(x=Revenue, y=Expenses,
                          colour=Industry))

d + geom_point() + 
  geom_smooth(fill=NA, size=1.2)

#Boxplots showing growth by industry
f<- ggplot(data=fin, aes(x=Industry, y=Growth,
                         colour=Industry))
f + geom_boxplot(size=1)

#Extra
f + geom_jitter() + 
  geom_boxplot(size=1, alpha=0.5, 
               outlier.color=NA)






