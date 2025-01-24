# Required Libraries
library(survey)
library(SDaA)

# Loading Data
data(exfm1)
data(exfm2)
data(exfm3)
data(exfm4)
data(exfm5)

# Converting datasets into data frames
data_acs_pilot <- as.data.frame(exfm3)
data_acs_def <- as.data.frame(exfm4)
data_ace_pilot <- as.data.frame(exfm1)
data_ace_def <- as.data.frame(exfm2)
data_as <- as.data.frame(exfm5)

# Displaying the ACS Pilot Data
print(data_acs_pilot)

# Survey Analysis with VWB
sprs(data_acs_pilot, "VWB", 3000, 46.8, error = 20, pop = "fin")
print(data_acs_def)
sprs(data_ace_def, "VWB", "PLOT_AREA", "STRATA_AREA", .groups = "STRATA", error = 20, pop = "fin")

# Help for 'sprs' function
?sprs

# Sampling
sample(1:100, size = 1)
sample(1:100)

# Loading presidents dataset
data("presidents")
print(summary(presidents))

# Sampling from a range
sample(1:1223, 30)

# Importing Manipulate Library and Demonstrating Simple Random Sampling
require(manipulate)
SimpleRandom()

# Popu Data
data(popu)

# Example for Survey Analysis with SDaA library
head(otters)
otters$habitat <- factor(otters$habitat, labels = c("cliffs", "agricultural", "peat", "non-peat"))
head(otters)

# Adding N to otters dataset and performing Survey Analysis
otters$N <- 237
head(otters)

# Survey Design and Mean Calculation
mydesign <- svydesign(id = ~1, data = otters, fpc = ~N)
svymean(~holts, design = mydesign)
confint(svymean(~holts, design = mydesign))
svytotal(~holts, design = mydesign)

# Example for Student Sample
sample(deg$`48`, size = 1)
sample(deg$year, size = 1)
mean(sample(deg$`pish girl`, size = 4))

# Simulating Sampling
a <- rep(0, 50)
for (i in 1:100) {
  b <- sample(deg$`50`, size = 2)
  a[i] <- mean(b)
}
summary(deg)

# Swiss Municipalities Data Analysis
data(swissmunicipalities)
swiss = swissmunicipalities

# Creating a Matrix for Survey Analysis
X <- cbind(swiss$HApoly,
           swiss$Surfacesbois,
           swiss$P00BMTOT,
           swiss$P00BWTOT,
           swiss$POPTOT,
           swiss$Pop020,
           swiss$Pop2040,
           swiss$Pop4065,
           swiss$Pop65P,
           swiss$H00PTOT)

# Sampling with Inclusion Probabilities
pik <- inclusionprobabilities(swiss$POPTOT, 400)

# Stratified Sampling
sample <- balancedstratification(X, swiss$REG, pik, comment = TRUE)
