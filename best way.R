library(readr)
edit_deg <- read_csv("edit deg.csv")
View(edit_deg)
rep(edit_deg)

getwd()
s=deg[sample(1:nrow(deg),20,replace = TRUE)]
s
var(sample(edit_deg$`pish girl`))
mean(edit_deg$`pish girl`)
?choose()
ali = choose(50,5)

samples <- replicate(edit_deg$`pish girl`, rnorm(48)) # 10 x 10000 sample matrix
sampdist <- replicate(edit_deg$`second boy`,mean(sample(deg$`66`, n ))) ## ???????????? ?????????? ???????? ????????????
mean(sampdist)
hist(sampdist,xlab = "?????? ?????????? ", main = "all of them ")
sd(edit_deg$`pish girl`)
sd(edit_deg$`pish boy`)
sd(edit_deg$year)
summary(edit_deg)
sample(edit_deg)
head(deg)
sum(edit_deg$`pish girl`, na.rm = FALSE) ## ?????????? ???????? ??????????

# compute sample means
sample.avgs <- colMeans(samples)
# check that 'sample.avgs' is a vector
is.vector(sample.avgs) 
#> [1] TRUE
?sample.avgs
# print the first few entries to the console
head(sample.avgs)
#> [1] -0.1045919  0.2264301  0.5308715 -0.2243476  0.2186909  0.2564663
# Plot the density histogram
hist(sample.avgs, 
     ylim = c(0, 1.4), 
     col = "steelblue" , 
     freq = F, 
     breaks = 20)

# overlay the theoretical distribution of sample averages on top of the histogram
curve(dnorm(x, sd = 1/sqrt(n)), 
      col = "red", 
      lwd = "2", 
      add = T)
# number of repetitions

# set degrees of freedom of a chi-Square Distribution
DF <- 3 

# sample 10000 column vectors à 3 N(0,1) R.V.S
Z <- replicate(reps, rnorm(DF)) 

# column sums of squares
X <- colSums(Z^2)
?colSums
colSums(edit_deg^2)
x<-c(edit_deg$year,edit_deg$`pish girl`)
rowMeans(x, na.rm = FALSE)
# histogram of column sums of squares
hist(edit_deg$`pish girl`, 
     freq = F, 
     col = "steelblue", 
     breaks = 40, 
     ylab = "Density", 
     main = "?????? ??????????????")

# add theoretical density ?????????? ????????
curve(rchisq(edit_deg$`pish boy`, df = DF), 
      type = 'l', 
      lwd = 2, 
      col = "red", 
      add = T)

# ee?????????? ???????????? ?????? ???? 
N <- 100
Y <- sample(0:1, N, replace = T)
Y <- sample(0:1, N, replace = FALSE)
Y
# Calculate R_n for 1:N
S <- cumsum(Y)
R <- S/(1:N)

# Plot the path.
plot(R, 
     ylim = c(0.3, 0.7), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     xlab = "n", 
     ylab = "R_n",
     main = "Converging Share of Heads in Repeated Coin Tossing")

# Add a dashed line for R_n = 0.5
lines(c(0, N), 
      c(0.5, 0.5), 
      col = "darkred", 
      lty = 2, 
      lwd = 1)
# subdivide the plot panel into a 2-by-2 array
par(mfrow = c(2, 2))
par
# set the number of repetitions and the sample sizes
reps <- 10000
sample.sizes <- c(5, 20, 75, 100)

# set seed for reproducibility
set.seed(123)

# outer loop (loop over the sample sizes)
for (n in sample.sizes) {
  
  samplemean <- rep(0, reps) #initialize the vector of sample means
  stdsamplemean <- rep(0, reps) #initialize the vector of standardized sample means
  
  # inner loop (loop over repetitions)   
  for (i in 1:reps) {
    x <- rbinom(n, 1, 0.5)
    samplemean[i] <- mean(x)
    stdsamplemean[i] <- sqrt(n)*(mean(x) - 0.5)/0.5
  }
  
  # plot histogram and overlay the N(0,1) density in every iteration    
  hist(stdsamplemean, 
       col = "steelblue", 
       freq = FALSE, 
       breaks = 40,
       xlim = c(-3, 3), 
       ylim = c(0, 0.8), 
       xlab = paste("n =", n), 
       main = "")
  
  curve(dnorm(x), 
        lwd = 2, 
        col = "darkred", 
        add = TRUE)
}  


#####################################################
set.seed(1234567);
N = 500;
n = 70;
p = rep(n/N,N);
X = cbind(runif(N),runif(N));
nrs = 10; # increase for more precision 
b1 = b2 = b3 = b4 = b5 = rep(0,nrs);

for(i in 1:nrs){
  # lpm1
  s = lpm1(p,X);
  b1[i] = sb(p,X,s);
  
  # lpm2
  s = lpm2(p,X);
  b2[i] = sb(p,X,s);
  
  # scps
  s = scps(p,X);
  b3[i] = sb(p,X,s);
  
  # lcube
  s = lcube(p,X,cbind(p));
  b4[i] = sb(p,X,s);  
  
  # srs
  s = sample(N,n);
  b5[i] = sb(p,X,s);
}
print(mean(b1));
print(mean(b2));
print(mean(b3));
print(mean(b4));
print(mean(b5));
# plot spatially balanced sample
set.seed(1234567);
N = 1000; # population size
n = 100; # sample size
p = rep(n/N,N); # inclusion probabilities
X = cbind(runif(N),runif(N)); # matrix of auxiliary variables
s = lpm1(p,X); # select sample 
plot(X[,1],X[,2]); # plot population
points(X[s,1],X[s,2], pch=19); # plot sample
?points
# check cpu time (for simulation)
# *********************************************************
set.seed(1234567);
N = 2000;
n = 100;
X = cbind(runif(N),runif(N));
p = rep(n/N,N);
system.time(for(i in 1:10){lpm1(p,X)});
system.time(for(i in 1:10){lpm2(p,X)});
?rep
N=384
rand<-rnorm(edit_deg$`pish girl`)
rand
sample(edit_deg$`pish girl`, size=1, replace =TRUE, prob = NULL)
is.vector(rand)
?is.vector
## matris 
z = matrix(edit_deg$`pish boy`)
z
v = matrix(edit_deg$`pish boy`,2,3)
v
?matrix
?choose()## ?????????? ????????????????
ali = choose(50,5)