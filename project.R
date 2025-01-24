
data_acs_pilot

data(exfm1)
data(exfm2)
data(exfm3)
data(exfm4)
data(exfm5)

data_acs_pilot <- as.data.frame(exfm3)
data_acs_def <- as.data.frame(exfm4)
data_ace_pilot <- as.data.frame(exfm1)
data_ace_def <- as.data.frame(exfm2)
data_as <- as.data.frame(exfm5)

data_acs_pilot

sprs(data_acs_pilot, "VWB", 3000, 46.8,error = 20, pop = "fin")
data_acs_def
sprs(data_ace_def, "VWB", "PLOT_AREA", "STRATA_AREA",
     .groups = "STRATA" ,error = 20, pop = "fin")

?sprs
###video learn ###]
sample(1:100,size = 1)
sample(1:100)
presidents
data("presidents")
data("presidents")
summary(presidents)
###
sample(1:1223,30)
require(manipulate)
SimpleRandom()
data(popu)
### important examples 
library(SDaA)

head(otters)
otters$habitat <- factor(otters$habitat, labels = c("cliffs", "agricultural", 
                                                    "peat", "non-peat"))
head(otters)

otters$N <- 237
head(otters)
library(survey)
mydesign <- svydesign(id = ~1, data = otters, fpc = ~N)
svymean(~holts, design = mydesign)
confint(svymean(~holts, design = mydesign))
svytotal(~holts, design = mydesign)

########### student sample ###########
sample(deg$`48`,size = 1)
sample(deg$year,size = 1)
mean(sample(deg$`pish girl`,size = 4))
a<- rep(0,50)
for (i in 1:100) {
  b<- sample(deg$`50`,size = 2)
a[i]<- mean(b)  
}
summary(deg)

data(swissmunicipalities)
swiss=swissmunicipalities
X=cbind(swiss$HApoly,
        swiss$Surfacesbois,
        swiss$P00BMTOT,
        swiss$P00BWTOT,
        swiss$POPTOT,
        swiss$Pop020,
        swiss$Pop2040,
        swiss$Pop4065,
        swiss$Pop65P,
        swiss$H00PTOT )
pik=inclusionprobabilities(swiss$POPTOT,400)
?pik
sample=balancedstratification(X,swiss$REG,pik,comment=TRUE)