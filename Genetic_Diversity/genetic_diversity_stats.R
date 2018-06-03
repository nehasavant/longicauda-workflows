#### Hierstats genetic diveristy stats #####

# This tutorial has been adapted from https://popgen.nescent.org/StartSNP.html

install.packages("hierfstat")
install.packages("pegas")

library("adegenet")
library("hierfstat")
library("pegas")

setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/Genetic_Diversity")

gen<-read.genepop("batch_1.gen")
mydata <- genind2hierfstat(gen) #hierfstat object, you'll use this later

div <- summary(gen) #use the genid object here
div

names(div)
plot(div$Hobs, xlab="Loci number", ylab="Observed Heterozygosity", 
      main="Observed heterozygosity per locus")

plot(div$Hobs,div$Hexp, xlab="Hobs", ylab="Hexp", 
     main="Expected heterozygosity as a function of observed heterozygosity per locus")

bartlett.test(list(div$Hexp, div$Hobs)) # a test : H0: Hexp = Hobs
#p-value < 2.2e-16...so what does this mean? 

#These analyses give you basic statistics using hierfstat package. The function `basic.stats()` provides the observed heterozygosity (Ho), mean gene diversities within population (Hs, Fis, Fst).  
basicstat <- basic.stats(mydata, diploid = TRUE, digits = 2)  # Fst following Nei (1987) on genind object
basicstat 

#The function `boot.ppfis()` provides confidence interval for Fis. 
boot.ppfis(mydata) 

#The function `indpca()` does a PCA on the centered matrix of indiviual's allele frequencies.
x <- indpca(mydata) 
plot(x, cex = 0.7)
