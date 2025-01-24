#Simple random SAMPLING IN R
###### start #######
# explain about data 
#how import data7 with CSV file !!!
library(readr)
edit_deg <- read_csv("edit deg.csv")
View(edit_deg)
#how import data7 with CSV file !!!
## show all data in Console 
rep(edit_deg)
## Adress
getwd()

## Arithmetic Mean
# form : mean(x, ...)
mean(edit_deg$`pish girl`)
mean(edit_deg$`pish boy`)
mean(edit_deg$`first girl`)
mean(edit_deg$`first boy`)
mean(edit_deg$`second girl`)
mean(edit_deg$`second boy`)
mean(edit_deg$`last girl`)
mean(edit_deg$`last boy`)

# Correlation, Variance and Covariance (Matrices)

  var(x, y = NULL, na.rm = FALSE, use)

cov(x, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))

# "everything", "all.obs", "complete.obs"

# var()
var(edit_deg$`pish girl`, y = NULL, na.rm = FALSE)
var(edit_deg$`pish girl`, y = NULL, na.rm = TRUE)

var(edit_deg$`pish boy`)
var(edit_deg$`first girl`)
var(edit_deg$`first boy`)
var(edit_deg$`second girl`)
var(edit_deg$`second boy`)
var(edit_deg$`last girl`)
var(edit_deg$`last boy`)
 # cov 
## explain about pearson 

is.matrix(edit_deg$`pish girl`)
chang1= as.matrix(edit_deg$`pish girl`)
cov(chang1, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))
## ??
is.matrix(chang1)

## all of them 

chang2= as.matrix(edit_deg)
cov(chang2, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))




## Calculating Confidence Intervals 
# 1.Calculate the mean
# 2. Calculate the standard error of the mean
# 3.Find the t-score that corresponds to the confidence level
# 4.Calculate the margin of error and construct the confidence interval
#1 we did it 
# 2 
q <- length(x)
q
# Standard Deviation
#form : sd(x, na.rm = FALSE)
h<- sd(x)
h
er <- h/sqrt(q)
print(er)
# 3 
alpha = 0.05
degrees.freedom = q - 1

# The Student t Distribution
?qt
# qt(p, df, lower.tail = TRUE, log.p = FALSE)
qt(p=alpha/2, df=degrees.freedom,lower.tail=T)
q(p=0.03, df=degrees.freedom,lower.tail=F)
t.s = qnorm(p=alpha/2,lower.tail=T)
?qbinom
print(t.s)
# 4 
m.error <- t.s*er
m.error
sample.mean <- mean(x)
sample.mean
lower.bound <- sample.mean - m.error
upper.bound <- sample.mean + m.error
lower.bound 
upper.bound

# Calculate the mean and standard error
###eshtebah
l.model <- lm(`pish girl` ~ 1, edit_deg)
l.model
# Calculate the confidence interval
confint(l.model, level=0.95)

summary(edit_deg)

sum(edit_deg$`pish girl`, na.rm = FALSE) ## Sum of Vector Elements

# Random Samples and Permutations
#  sample(x, size, replace = FALSE, prob = NULL)
#         sample.int(n, size = n, replace = FALSE, prob = NULL,
           #    useHash = (!replace && is.null(prob) && size <= n/2 && n > 1e7))

?sample
sample(edit_deg$`pish girl`, size=10, replace = F)
sample(edit_deg$`pish boy`,10,T)
sample(edit_deg$`first girl`,20,F)

#(((((((( The Normal Distribution
rnorm(10)

set.seed(100)
sample(1:100,size = 5,T)
set.seed(100)
sample(1:100,size = 5,T)

#T student 
?rt
x<- sample(rt(500,2),20,F)

sample(rt(500,2),20,F)
mean(x)
var(x)
sample(rt(10,2),5,T)
sample(rt(10,2),5,F)
sample(rt(10,2),5,F)


## last part 
# chart

hist(edit_deg$`pish girl`,col = "steelblue" ,      freq = F,      breaks = 20)

hist(sample(edit_deg$`pish girl`,size=10),col = "yellow",prob=TRUE)
hist(sample(edit_deg$`pish girl`,size=10),col = "orange",prob=F)


boxplot(edit_deg$`first boy`)
boxplot(edit_deg$`pish girl`,edit_deg$`pish boy`)
boxplot(sample(edit_deg$`pish girl`,10,T))
boxplot(sample(edit_deg$`pish girl`,10,F))


data1 <- data.frame(edit_deg$`pish girl`,edit_deg$`pish boy`)
barplot(as.matrix(data1),col=terrain.colors(10),xlab = "kindergarten"
      , main = "bar plot" ,beside = T ,legend.text = T)

data2 <- data.frame(sample(edit_deg$`pish girl`),sample(edit_deg$`pish boy`))
barplot(as.matrix(data2),col=terrain.colors(5),
        main="sample barplot ", xlab = "girl and boy kindergarten ",
        beside = T,legen.text=T)

X=sample(1:6,100,p=7-(1:6),replace=T);boxplot(X,horizontal=T,bty=n)
#bedoon jaygozari F tamoom mishe  jaygozari 
#######  thank you for changing this world ##########
# source 
#https://bookdown.org/logan_kelly/r_practice/p09.html
N=500;n=50
y=sample(rnorm(N, mean = 10, sd =2),n,replace=F)
mu=mean(y)
f=n/N
v=var(y)*(1-f)/n
L=mu-qnorm(0.025,lower.tail=FALSE)*sqrt(v)
U=mu+qnorm(0.025,lower.tail=FALSE)*sqrt(v)
mu;v;L;U

###########################################################################


x=sample(rbinom(N, size=1, prob=0.30),n,replace=F)
p=mean(x)
v=(N-n)/(n-1)*p*(1-p)/N
L=p-qnorm(0.025,lower.tail=FALSE)*sqrt(v)
U=p+qnorm(0.025,lower.tail=FALSE)*sqrt(v)
p;v;L;U
library(UsingR)
data()
data(speed)
data2
data(
  
)
