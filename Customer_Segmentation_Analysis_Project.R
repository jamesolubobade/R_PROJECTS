#CHANGING DIRECTORY:

setwd('C:\\Users\\Owner\\Desktop\\PROJECT R')
getwd()

#LOADING CSV DATA TO DATA FRAME

customer_data<-read.csv("customer_dataset.csv")

customer_data[customer_data=='']<-NA #converting Null to Na

#EDA(EXPLORATORY DATA ANALYSIS)

#SHAPE OF DATA:
dim(customer_data)

#NAME OF COLUMNS
colnames(customer_data)

#STRUCTURE OF DATA
str(customer_data)

#CLASSES OF ALL COLUMNS
sapply(customer_data, class)  

#HEAD OF THE DATA SET
head(customer_data)
head(customer_data,11)

#TAIL OF THE DATA SET
tail(customer_data)

#NUMBER OF MISSING VALUES
colSums(is.na(customer_data))

#Percentage of missing values:
round(colMeans(is.na(customer_data))*100,2)#2:digit=2


#REMOVE NA'S
completerecords=(na.omit(customer_data))
completerecords

#PERCENTAGE OF MISSING VALUES
round(colMeans(is.na(df))*100,2)#2:digit=2

#SUMMARY OF DATA
summary(customer_data)
summary(customer_data$Gender)
summary(customer_data$Ever_Married)
summary(customer_data$Age)
summary(customer_data$Graduated)
summary(customer_data$Profession)
summary(customer_data$Work_Experience)
summary(customer_data$Spending_Score)
summary(customer_data$Family_Size)
summary(customer_data$Segmentation)

#MAKE A COPY
customer_data_org<-customer_data

#DATA CLEANING:

#1.DUPLICATE DATA

duplicated(customer_data)#Returns for me TRUE for observation that is duplicated 
#otherwise returns FALSE

#I removed those records that don't have any meaningful features to extract from them.
customer_data[,c("ID","Var_1")]<-NULL

dim(customer_data)

head(customer_data)
tail(customer_data)

#INSTALL PACKAGES:
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(formattable)
library(treemap)

#BUSINESS PROBLEM:I want to determine why certain segments of the data sets spend less than 
#the others, and the target will be Spending_Score.

#Q1.What is the percentage of Spending_Score based on the data set?
table(customer_data$Spending_Score)
tbl<-table(customer_data$Spending_Score)
tbl

# Pie Chart with Percentages
count<-table(customer_data$Spending_Score)
count
freq1<- c(count[1], count[2], count[3])
lbls<-c("Low","Average","High")
pct<- round(freq1/sum(freq1)*100)
lbls<- paste(lbls, pct) 
lbls<- paste(lbls,"%",sep="") 
pie(table(customer_data$Spending_Score))
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Spending Score")

# Simple Bar Plot
counts <- table(customer$Spending_Score)
counts
barplot(c(count[1], count[2], count[3]), main="Spending_Score",
        ylab="Number",col = 'blue',horiz = FALSE)

#Q2.What is the distribution of Segmentation?

# Pie Chart with Percentages
count<-table(customer_data$Segmentation)
count
freq1<- c(count[1], count[2], count[3], count[4])
lbls<-c("A","B","C","D")
pct<- round(freq1/sum(freq1)*100)
lbls<- paste(lbls, pct) # add percents to labels and by default separate them by space
lbls<- paste(lbls,"%",sep="") # ad % to labels
pie(table(customer_data$Segmentation))
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Segmentation")

# Simple Bar Plot
tbl<-table(customer_data$Segmentation)
tbl
counts <- table(customer$Segmentation)
counts
barplot(c(count[1], count[2], count[3], count[4]), main="Segmentation",
        ylab="Number",col = 'green',horiz = FALSE)

#Q3.What is the distribution of Gender?

# Pie Chart with Percentages
count<-table(customer_data$Gender)
count
freq1<- c(count[1], count[2])
lbls<-c("Male","Female")
pct<- round(freq1/sum(freq1)*100)
lbls<- paste(lbls, pct) 
lbls<- paste(lbls,"%",sep="") 
pie(table(customer_data$Gender))
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Gender")


# Simple Bar Plot
tbl<-table(customer_data$Gender)
tbl

counts <- table(customer$Gender)
counts
barplot(c(count[1], count[2]), main="Gender",
        ylab="Number",col = 'red',horiz = FALSE)



#Q4.What is the distribution of Profession?

# Pie Chart with Percentages
count<-table(customer_data$Profession)
count
freq1<- c(count[1], count[2], count[3], count[4],count[5], count[6], count[7], count[8], count[9])
lbls<-c("Healthcare","Engineer","Lawyer","Entertainment","Artist","Doctor","Homemaker","Executive",
        "Marketing")
pct<- round(freq1/sum(freq1)*100)
lbls<- paste(lbls, pct) 
lbls<- paste(lbls,"%",sep="")
pie(table(customer_data$Profession))
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Profession")

# Simple Bar Plot
tbl<-table(customer_data$Profession)
tbl

counts <- table(customer$Profession)
counts
barplot(c(count[1], count[2], count[3], count[4],count[5], count[6], count[7], count[8], count[9]), 
        main="Profession",
        ylab="Number",col = 'yellow',horiz = FALSE)


#Q5.Is there any relation between Segmentation and target(Spending_Score)?

#Bivariate analysis for Segmentation Vs. Spending_Score
#Bivariate analysis for categorical vs. categorical:for visualization:stacked barchart 
#or grouped bar chart, ...
#for summarization: Contingency table(two-way table)
#for test of independence: chi-square test


# Stacked Bar Plot with Colors and Legend

tbl<-table(customer_data$Segmentation,customer_data$Spending_Score)
tbl
counts<-tbl[1:4,1:3]
counts
barplot(counts, main="Segmentation Vs. Spending_Score",
        xlab="Spending_Score", col=c("darkblue","red","green","yellow"),
        legend = rownames(counts))
#OR
add<-addmargins(table(customer_data$Segmentation,customer_data$Spending_Score))
add

xtabs(~ Segmentation+Spending_Score,data=customer_data)
add<-addmargins(xtabs(~Segmentation+Spending_Score,data=customer_data))
add
add[2:4,2:4]
prop.table(xtabs(~ Segmentation+Spending_Score,data=customer_data))[2:3,2:3]


library(MASS)       

#Problem:
#Test the hypothesis whether the Spending_Score is independent of the Segmentation  
#at .05 significance level.

# Null hypothesis  Spending_Score is independent of Segmentation

#Solution
#We apply the Chi-Square Test function to the contingency table tbl, and found the p-value to be 2.2e-16
tbl <- table(customer_data$Spending_Score,customer_data$Segmentation)
tbl
tbl <-tbl[1:3,1:4]   #tbl[-1,-1]           # the contingency table
tbl
chisq.test(tbl)

# since p-value is less than 5% we reject null hypotheses and conclude there is a statistically 
#association between Segmentation and Spending_Score at 5% significant level

# or Mosaic plots provide a way to visualize contingency tables. A mosaic plot is a visual 
#representation of the association between two variables.

library(vcd)
mosaicplot(tbl, shade=TRUE)

mosaicplot(x, color= NULL, main = "Title")

tbl<- table(customer_data$Spending_Score, customer_data$Segmentation)
mosaicplot(tbl, shade=TRUE)


#Association Plots
assoc(tbl, shade=TRUE)


#the blue color means the actual value for that category is bigger than expected value if they are 
#independent

#Answer:
#As the p-value 2.2e-16=0.00000000000000022 is less than the .05 significance level, we reject the 
#null hypothesis that the Spending_Score is independent of the Segmentation and conclude that in our 
#data, the Spending_Score and Segmentation are statistically significantly associated (p-value = 0)

#Q6.What is the distribution of Work_Experience and is there any relation between Work_Experience
#and target Spending_Score?
#levels(customer_data$Work_Experience)

table(customer_data$Work_Experience)
addmargins(xtabs(~ Work_Experience+Spending_Score,data=customer_data))
prop.table(xtabs(~ Work_Experience+Spending_Score,data=customer_data))

#Problem:
#Test the hypothesis whether the Spending_Score is independent of the Work_Experience at .05 significance level.
# Null hypothesis Spending_Score is independent of Work_Experience


#Solution
#We apply the Chi-Square Test function to the contingency table tbl, and found the p-value to be 8.064e-06
tbl <-table(customer_data$Spending_Score,customer_data$Work_Experience)
tbl# the contingency table
tbl<-tbl [-1,-c(1,13)]                
tbl
chisq.test(tbl)
#Answer:
#As the p-value p-value = 0.0006 is greater than the .05 significance level, we accept the null hypothesis that
#the Work_Experience is dependent of the Segmentation so there is no association between Spending_Score
# and Work_Experience at 5% significant level


#Q9.what is the distribution of Ever_Married and is there any relation between Ever_Married and 
#target Spending_Score?

table(customer_data$Ever_Married)
customer_data$Ever_Married[customer_data$Ever_Married=="Married"]<-"Yes"
table(customer_data$Ever_Married)
addmargins(xtabs(~ Ever_Married+Spending_Score,data=customer_data))
prop.table(xtabs(~ Ever_Married+Spending_Score,data=customer_data))

#Problem:
#Test the hypothesis whether the Spending_Score is independent of the Ever_Married at .05 significance level.
# Null hypothesis  Spending_Score is independent of  Ever_Married

#Solution
#We apply the Chi-Square Test function to the contingency table tbl, and found the p-value to be 2.2e-16
tbl <- table(customer_data$Spending_Score,customer_data$Ever_Married)
tbl
tbl <-tbl[-1,-c(1,13)]
tbl# the contingency table
chisq.test(tbl)
#Answer:
#As the p-value < 1.009e-11 is less than the .05 significance level, we reject the null hypothesis that
#the Spending_Score is independent of the Ever_Married
#so the Spending_Score and Ever_Married are statistically significantly associated (p-value = 0)

#Q8.what is the distribution of Profession and is there any relation between Profession and target
#Spending_Score?
table(customer_data$Profession)
customer_data$Profession[customer_data$Profession=="Other"]<-"other"
table(customer_data$Profession)

addmargins(xtabs(~ Profession+Spending_Score,data=customer_data))
prop.table(xtabs(~ Profession+Spending_Score,data=customer_data))


tbl <-table(customer_data$Profession,customer_data$Spending_Score)
tbl
tbl<-tbl[-c(1,13),-1]                 # the contingency table
tbl


#what is the relation between Spending_Score and Profession?

chisq.test(tbl)
#Chi-squared approximation may be incorrect because you have a group with small frequency

customer_data$Profession[customer_data$Profession=="Annual_Income"]<-"other"
tbl <-table(customer_data$Profession,customer_data$Spending_Score)
tbl
tbl<-tbl[-c(1,12,13),-1]                 # the contingency table
tbl

chisq.test(tbl)
#Answer:
#As the p-value 2.2e-16=2.2 * 10 to power of -16= 0.00000000000000022 is less than the .05 significance 
#level, we reject the null hypothesis that
#the Spending_Score is independent of the Profession 
#and conclude that the Spending_Score and the Profession are statistically significantly associated at %5 
#significant level


#Q9.what is Family Size distribution?

summary(customer_data$Family_Size)


# Box plot of Family_Size by Spending_Score
boxplot(Family_Size ~ Spending_Score,data=customer_data, main="Family_Size",
        xlab="Spending_Score", ylab="Family_Size",col="red")
#Histogram
hist(customer_data$Family_Size, breaks = 5, main = "Family_Size",col="blue",xlab="Family_Size",ylab="Frequency")


summary(customer_data["Family_Size"])

#If you notice the maximum of Family_Size is 10 which is strange considering the Family_Size are 
#within the range of 1-9 let's try to find pattern of them if they exist

customer_data<-customer_data[which(customer_data["Family_Size"]>10),]
customer_data$Family_Size
summary(customer_data["Family_Size"])
nrow(customer_data)

#it looks like some of the Family Size are just scaled up by 10. let me make sure
count<-0
for (val in customer_data$Family_Size){
  if (val%%10 !=0) {count=count+1}
}


count#--> count = 0 means all the credit scores greater than 850 are scaled up by 10 



customer_data$Family_Size<-ifelse(customer_data$Family_Size>10, customer_data$Family_Size/10, customer_data$Family_Size)
summary(customer_data["Family_Size"])


#"pipe"-like operator, %>%, with which you may pipe a value forward into an expression or function call; something along the lines of x %>% f, rather than f(x)

customer_data%>%str

#Q10.Is there any relationship between Family_Size and Spending_Score?
#It is Bivariate analysis for continuous vs. categorical variables
# Bivariate Analysis is a very important part of analysis in real word as an example 
#when you should run A/B test
#For sure Group column is categorical column , if target is categorical ( for example buying the product or not) you should run chi-square test
#If target is continues (like order amount or time spend on web) you should run t-test for A/B test if you have two options or ANOVA if you have more than two options 

#Continuous Vs. Categorical  : For summaraization: group by categorical column an aggregate for numerical column
#                              For visualization: Group box plot, Group Histogram,Group density...
#                              For test of independence :1) if categorical column has only two levels :t-test ( if all assumption are met)
#                                                        2) if categorical column has more than two levels: ANOVA ( if all assumption are met)
#Bivariate analysis for Family Size Vs. Spending Score(Continuous Vs. Categorical)
# summarization:
#method #1
dim(customer_data)
agg1<- aggregate(Family_Size~ Spending_Score, customer_data , mean)
agg1
names(agg1) <- c("Spending_Score","mean of Family_Score")
agg1

agg2<- cbind(aggregate(Family_Size ~ Spending_Score, customer_data , min),
             aggregate(Family_Size ~ Spending_Score, customer_data , max)[,2],
             aggregate(Family_Size ~ Spending_Score, customer_data , mean)[,2])

names(agg2) <- c("Spending_Score","min_Family_Size","max_Family_Size","mean_Family_Size")
agg2



#Method #2
by(customer_data$Family_Size,customer_data$Spending_Score,FUN= fx)

#Method#3
tapply(customer_data$Family_Size,customer_data$Spending_Score,FUN= fx)
#Method#4
#load library
library(doBy)
summaryBy(Family_Size~ Spending_Score, customer_data,FUN= fx)
summaryBy(Family_Size~ Spending_Score, customer_data,FUN= c(mean,sd,var,min,max),na.rm=T)

#Method#5 you can use sql syntax with the help of sqldf()
#Note: As you can see the average( mean) of credit score for those who fully paid are different from those who charged off
#the question is that is that difference is significant in population or not
#for answering this qustion I will run t-test

#visulaization:
library(ggplot2)
#Group box plot with q plot( I will do that with ggplot as well)
qplot(Spending_Score~ Family_Size, customer_data = customer_data, 
      geom="boxplot", fill = Spending_Score)

#group histogram
# Changing histogram plot fill colors by Spending Score and usining semi-transparent fill
p<-ggplot(customer_data, aes(x=Family_Size, fill=Spending_Score, color=Spending_Score)) +
  geom_histogram(position="identity", alpha=0.5)
p
# Add mean lines
library(plyr)
mu <- ddply(customer_data, "Spending_Score", summarise, group.mean=mean(Family_Size,na.rm=T))
mu
p<-p+geom_vline(data=mu, aes(xintercept=group.mean, color=Spending_Score),
                linetype="dashed")
p

#Add density
p<-ggplot(customer_data, aes(x=Family_Size, fill=Spending_Score, color=Spending_Score)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.1)
p
# Add mean lines and Change the legend position
p+geom_vline(data=mu, aes(xintercept=group.mean, color=Spending_Score),
             linetype="dashed")+ theme(legend.position="top")+labs(title="Family_Size histogram plot",x="Family_Size", y = "Density")
#Group box plot
p<-ggplot(customer_data, aes(x=Spending_Score, y=Family_Size, fill=Spending_Score)) +
  geom_boxplot()
p
# Box plot with mean points
p<-p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
p

#Group box plot
p<-ggplot(customer_data, aes(x=Spending_Score, y=Family_Size, fill=Spending_Score)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=1)
p
# Box plot with mean points
p<-p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)
p

