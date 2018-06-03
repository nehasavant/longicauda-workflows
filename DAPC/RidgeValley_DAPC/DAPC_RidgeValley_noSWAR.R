#DAPC with Ridge&Valley samples only
install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)

#load data from Wickecheoke stream salamanders
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/RidgeValley_DAPC")

#set working directory session- set directory to Suburban sals- statistics DAPC
n1<-read.genepop("batch_1_RidgeValley_noSWAR.gen")
n1
str(n1)
pop(n1) #For some reason it is reading the popuations wrong. OLD009, UPP022, UPP021 and WICK015 are the populations names. These are the names of the final samples in each pop...

nameStrata(n2)

n1clust<- find.clusters(n1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
80# finding PCs
2# finding clusters

table(pop(n1), n1clust$grp)
table.value(table(pop(n1), n1clust$grp), col.lab=paste("inf", 1:5), row.lab=paste("ori", 1:5))

#Run first dapc on b1clust
dapc.n1<- dapc (n1, n1clust$grp)

#Running the DAPC
dapc.n2 <- dapc(n1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
3 #retain 6 DA
dapc.n2$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.r2$eig/(sum(dapc.n2$eig)))*100,2)
eig_percent
scatter(dapc.n2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.n2, n.pca=1:ncol(dapc.n2$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=3) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=4
dapc.n3 <- dapc(n1, var.contrib= TRUE, scale = FALSE, n.pca = 4, n.da = 3) #change PCs to the optimal
dapc.n3.scatter<- scatter(dapc.n3, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.n2scatter, position= "upperleft", ncol=2)
dapc.n3

#sum stats of a scores for your DAPC
summary(dapc(n1, n.da=6, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.n3$eig/(sum(dapc.n3$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

ridge=list("MUCK2", "MUCK1", "MUCK3", "SVEN", "WHIT", "WPOA")

#axes 1&2
scatter(dapc.n3, xax=1, yax=2, pch = 18:25, txt.leg = ridge, label = ridge, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#axes 2&3
scatter(dapc.n3, xax=2, yax=3, pch = 18:25, txt.leg = ridge, label = ridge, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft")

#axes 3&4
scatter(dapc.n3, xax=3, yax=4, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

