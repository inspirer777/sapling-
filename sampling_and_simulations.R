# Load necessary libraries
library(readr)

# Read the CSV file
edit_deg <- read_csv("edit deg.csv")
View(edit_deg)

# Display the data in the console
rep(edit_deg)

# Get the current working directory
getwd()

# Sampling 20 random rows from the dataset
s = edit_deg[sample(1:nrow(edit_deg), 20, replace = TRUE), ]
print(s)

# Calculate variance of 'pish girl'
var_sample <- var(sample(edit_deg$`pish girl`))
print(var_sample)

# Calculate mean of 'pish girl'
mean_pish_girl <- mean(edit_deg$`pish girl`)
print(mean_pish_girl)

# Example usage of choose function
ali = choose(50, 5)
print(ali)

# Generate random normal samples (replicate 10 times with 10000 samples)
samples <- replicate(10, rnorm(48))

# Calculate sample means
sampdist <- replicate(10, mean(sample(edit_deg$`second boy`, 10)))

# Plot the sample distribution
mean_sampdist <- mean(sampdist)
hist(sampdist, xlab = "Sample Distribution", main = "Sample Distribution of 'second boy'")

# Calculate the standard deviation for 'pish girl', 'pish boy', and 'year'
sd_pish_girl <- sd(edit_deg$`pish girl`)
sd_pish_boy <- sd(edit_deg$`pish boy`)
sd_year <- sd(edit_deg$year)

print(sd_pish_girl)
print(sd_pish_boy)
print(sd_year)

# Summary of the dataset
summary(edit_deg)

# Compute sum of 'pish girl' column, ignoring NA values
sum_pish_girl <- sum(edit_deg$`pish girl`, na.rm = FALSE)
print(sum_pish_girl)

# Compute sample means of the samples matrix
sample_avgs <- colMeans(samples)
print(is.vector(sample_avgs))  # Check if it is a vector

# Display first few sample averages
head(sample_avgs)

# Plot histogram of sample averages with theoretical normal curve
hist(sample_avgs, ylim = c(0, 1.4), col = "steelblue", freq = FALSE, breaks = 20)
curve(dnorm(x, sd = 1/sqrt(48)), col = "red", lwd = 2, add = TRUE)

# Set degrees of freedom for Chi-Square distribution
DF <- 3
Z <- replicate(10000, rnorm(DF))
X <- colSums(Z^2)

# Histogram of column sums of squares
hist(edit_deg$`pish girl`, freq = FALSE, col = "steelblue", breaks = 40, ylab = "Density", main = "Histogram of Column Sums")

# Add theoretical chi-square density curve
curve(rchisq(100, df = DF), type = 'l', lwd = 2, col = "red", add = TRUE)

# Calculate cumulative sum and plot the path for repeated coin tossing simulation
N <- 100
Y <- sample(0:1, N, replace = TRUE)
S <- cumsum(Y)
R <- S / (1:N)

plot(R, ylim = c(0.3, 0.7), type = "l", col = "steelblue", lwd = 2, xlab = "n", ylab = "R_n", main = "Converging Share of Heads in Repeated Coin Tossing")
lines(c(0, N), c(0.5, 0.5), col = "darkred", lty = 2, lwd = 1)

# Simulate binomial distributions with different sample sizes
sample.sizes <- c(5, 20, 75, 100)
set.seed(123)

for (n in sample.sizes) {
  samplemean <- rep(0, 10000)  # initialize vector of sample means
  stdsamplemean <- rep(0, 10000)  # initialize vector of standardized sample means
  
  for (i in 1:10000) {
    x <- rbinom(n, 1, 0.5)
    samplemean[i] <- mean(x)
    stdsamplemean[i] <- sqrt(n) * (mean(x) - 0.5) / 0.5
  }
  
  hist(stdsamplemean, col = "steelblue", freq = FALSE, breaks = 40, xlim = c(-3, 3), ylim = c(0, 0.8), xlab = paste("n =", n))
  curve(dnorm(x), lwd = 2, col = "darkred", add = TRUE)
}

# Spatially balanced sample plot
set.seed(1234567)
N <- 1000
n <- 100
p <- rep(n/N, N)
X <- cbind(runif(N), runif(N))
s <- sample(N, n)
plot(X[,1], X[,2])
points(X[s,1], X[s,2], pch = 19)

# Additional random sampling and matrix operations
rand <- rnorm(edit_deg$`pish girl`)
print(rand)

z <- matrix(edit_deg$`pish boy`)
v <- matrix(edit_deg$`pish boy`, 2, 3)
print(z)
print(v)

# Final sample from the 'pish girl' column with replacement
sample_pish_girl <- sample(edit_deg$`pish girl`, size = 1, replace = TRUE)
print(sample_pish_girl)

# Using choose() to calculate combinations
ali_comb = choose(50, 5)
print(ali_comb)
