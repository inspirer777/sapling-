###### Start: Simple Random Sampling in R ######

# Loading Required Libraries
library(readr)

# Importing Data from CSV
# Adjust the file path as needed
edit_deg <- read_csv("edit_deg.csv")

# View the imported data
View(edit_deg)

# Show all data in Console
rep(edit_deg)

# Check current working directory
getwd()

# Calculating Arithmetic Means for Various Columns
mean(edit_deg$`pish girl`)
mean(edit_deg$`pish boy`)
mean(edit_deg$`first girl`)
mean(edit_deg$`first boy`)
mean(edit_deg$`second girl`)
mean(edit_deg$`second boy`)
mean(edit_deg$`last girl`)
mean(edit_deg$`last boy`)

# Correlation, Variance, and Covariance
var(edit_deg$`pish girl`, na.rm = TRUE)
cov(edit_deg$`pish girl`, edit_deg$`first girl`, method = "pearson")

# Convert the data into matrix for covariance calculation
chang1 <- as.matrix(edit_deg$`pish girl`)
cov(chang1, method = "pearson")

# Covariance for entire dataset
chang2 <- as.matrix(edit_deg)
cov(chang2, method = "pearson")

# Confidence Interval Calculations

# Standard Deviation Calculation
q <- length(edit_deg$`pish girl`)
h <- sd(edit_deg$`pish girl`)
er <- h / sqrt(q)

# Degrees of freedom
alpha = 0.05
degrees.freedom = q - 1

# The Student t-distribution
t.s = qt(p = alpha / 2, df = degrees.freedom, lower.tail = TRUE)

# Margin of Error and Confidence Interval
m.error <- t.s * er
sample.mean <- mean(edit_deg$`pish girl`)
lower.bound <- sample.mean - m.error
upper.bound <- sample.mean + m.error

# Confidence Interval
lower.bound
upper.bound

# Linear Model for Confidence Interval Calculation
l.model <- lm(`pish girl` ~ 1, edit_deg)
confint(l.model, level = 0.95)

# Random Sampling
sample(edit_deg$`pish girl`, size = 10, replace = FALSE)
sample(edit_deg$`pish boy`, 10, TRUE)
sample(edit_deg$`first girl`, 20, FALSE)

# Normal Distribution and Random Sampling
rnorm(10)
set.seed(100)
sample(1:100, size = 5, TRUE)

# T-distribution Sample
x <- sample(rt(500, 2), 20, FALSE)

# Histograms and Boxplots
hist(edit_deg$`pish girl`, col = "steelblue", freq = FALSE, breaks = 20)
hist(sample(edit_deg$`pish girl`, size = 10), col = "yellow", prob = TRUE)
hist(sample(edit_deg$`pish girl`, size = 10), col = "orange", prob = FALSE)

boxplot(edit_deg$`first boy`)
boxplot(edit_deg$`pish girl`, edit_deg$`pish boy`)

# Barplot for Sample Data
data1 <- data.frame(edit_deg$`pish girl`, edit_deg$`pish boy`)
barplot(as.matrix(data1), col = terrain.colors(10), xlab = "kindergarten", main = "Bar Plot", beside = TRUE, legend.text = TRUE)

data2 <- data.frame(sample(edit_deg$`pish girl`), sample(edit_deg$`pish boy`))
barplot(as.matrix(data2), col = terrain.colors(5), main = "Sample Barplot", xlab = "Girl and Boy Kindergarten", beside = TRUE, legend.text = TRUE)

# Example of Sample from Binomial Distribution
N = 500; n = 50
y = sample(rnorm(N, mean = 10, sd = 2), n, replace = FALSE)
mu = mean(y)
f = n / N
v = var(y) * (1 - f) / n
L = mu - qnorm(0.025, lower.tail = FALSE) * sqrt(v)
U = mu + qnorm(0.025, lower.tail = FALSE) * sqrt(v)
mu; v; L; U

# Example of Sample from Binomial Distribution
x = sample(rbinom(N, size = 1, prob = 0.30), n, replace = FALSE)
p = mean(x)
v = (N - n) / (n - 1) * p * (1 - p) / N
L = p - qnorm(0.025, lower.tail = FALSE) * sqrt(v)
U = p + qnorm(0.025, lower.tail = FALSE) * sqrt(v)
p; v; L; U

###### End ######
