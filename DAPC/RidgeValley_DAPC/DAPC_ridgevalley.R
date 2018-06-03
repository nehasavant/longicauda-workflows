#DAPC with Ridge&Valley samples only
install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)

#load data from Wickecheoke stream salamanders
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/RidgeValley_DAPC")

#set working directory session- set directory to Suburban sals- statistics DAPC
r1<-read.genepop("batch_1_RidgeValley.gen")
r1
str(r1)
pop(r1) #For some reason it is reading the popuations wrong. OLD009, UPP022, UPP021 and WICK015 are the populations names. These are the names of the final samples in each pop...

nameStrata(r2)

r1clust<- find.clusters(r1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
100# finding PCs
3# finding clusters

table(pop(r1), r1clust$grp)
table.value(table(pop(r1), r1clust$grp), col.lab=paste("inf", 1:5), row.lab=paste("ori", 1:5))

#Run first dapc on b1clust
dapc.r1<- dapc (r1, b1clust$grp)

#Running the DAPC
dapc.r2 <- dapc(r1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
4 #retain 6 DA
dapc.r2$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.r2$eig/(sum(dapc.r2$eig)))*100,2)
eig_percent
scatter(dapc.r2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.r2, n.pca=1:ncol(dapc.r2$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=4) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=6
dapc.r3 <- dapc(r1, var.contrib= TRUE, scale = FALSE, n.pca = 6, n.da = 4) #change PCs to the optimal
dapc.r3.scatter<- scatter(dapc.r3, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.r2scatter, position= "upperleft", ncol=2)
dapc.r3

#sum stats of a scores for your DAPC
summary(dapc(r1, n.da=6, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.r3$eig/(sum(dapc.r3$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

ridge=list("MUCK2", "MUCK1", "MUCK3", "SVEN", "SWAR", "WHIT", "WPOA")

#axes 1&2
scatter(dapc.r3, xax=1, yax=2, pch = 18:25, txt.leg = ridge, label = ridge, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#axes 2&3
scatter(dapc.r3, xax=2, yax=3, pch = 18:25, txt.leg = ridge, label = ridge, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft")

#axes 3&4
scatter(dapc.r2, xax=3, yax=4, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

