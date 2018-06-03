####DAPC with all Piedmont samples  ####
install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)

#load data from Wickecheoke stream salamanders
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/DAPC/piedmont_DAPC")

#set working directory session- set directory to Suburban sals- statistics DAPC
p1<-read.genepop("batch_1_Piedmont.gen")
p1
str(p1)
pop(p1) #For some reason it is reading the popuations wrong. OLD009, UPP022, UPP021 and WICK015 are the populations names. These are the names of the final samples in each pop...

nameStrata(b2)

p1clust<- find.clusters(p1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
100# finding PCs
2# finding clusters

table(pop(p1), p1clust$grp)
table.value(table(pop(p1), p1clust$grp), col.lab=paste("inf", 1:14), row.lab=paste("ori", 1:14))

#Run first dapc on b1clust
dapc.p1<- dapc (p1, b1clust$grp)

#Running the DAPC
dapc.p2 <- dapc(p1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
6 #retain 6 DA
dapc.p2$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.p2$eig/(sum(dapc.p2$eig)))*100,2)
eig_percent
scatter(dapc.p2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.p2, n.pca=1:ncol(dapc.p2$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=6) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=7
dapc.p3 <- dapc(p1, var.contrib= TRUE, scale = FALSE, n.pca = 7, n.da = 6) #change PCs to the optimal
dapc.p3.scatter<- scatter(dapc.p3, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.b2scatter, position= "upperleft", ncol=2)
dapc.p3

#sum stats of a scores for your DAPC
summary(dapc(p1, n.da=6, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.p3$eig/(sum(dapc.p3$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

piedmont=list("CAPO", "COOK", "HAKI", "HARI", "LNIS", "LOCK", "WICK", "WARF")

#axes 1&2
scatter(dapc.p3, xax=1, yax=2, pch = 18:25, txt.leg = piedmont, label = piedmont, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#axes 2&3
scatter(dapc.p3, xax=2, yax=3, pch = 18:25, txt.leg = piedmont, label = piedmont, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft")
#axes 3&4
scatter(dapc.p2, xax=3, yax=4, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#### DAPC of Piedmont streams without COOK ####

#set working directory session- set directory to Suburban sals- statistics DAPC
p1<-read.genepop("batch_1_Piedmont_noCOOK.gen")
p1
str(p1)
pop(p1) #For some reason it is reading the popuations wrong. OLD009, UPP022, UPP021 and WICK015 are the populations names. These are the names of the final samples in each pop...

nameStrata(b2)

p1clust<- find.clusters(p1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
85# finding PCs
2# finding clusters

table(pop(p1), p1clust$grp)
table.value(table(pop(p1), p1clust$grp), col.lab=paste("inf", 1:14), row.lab=paste("ori", 1:14))

#Run first dapc on b1clust
dapc.p1<- dapc (p1, p1clust$grp)

#Running the DAPC
dapc.p2 <- dapc(p1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
5 #retain 5 DA
dapc.p2$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.p2$eig/(sum(dapc.p2$eig)))*100,2)
eig_percent
scatter(dapc.p2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.p2, n.pca=1:ncol(dapc.p2$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=6) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=7
dapc.p3 <- dapc(p1, var.contrib= TRUE, scale = FALSE, n.pca = 7, n.da = 6) #change PCs to the optimal
dapc.p3.scatter<- scatter(dapc.p3, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.b2scatter, position= "upperleft", ncol=2)
dapc.p3

#sum stats of a scores for your DAPC
summary(dapc(p1, n.da=6, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.p3$eig/(sum(dapc.p3$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

piedmont=list("CAPO", "HAKI", "HARI", "LNIS", "LOCK", "WICK", "WARF")

#axes 1&2
scatter(dapc.p3, xax=1, yax=2, pch = 18:25, txt.leg = piedmont, label = piedmont, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#axes 2&3
scatter(dapc.p3, xax=2, yax=3, pch = 18:25, txt.leg = piedmont, label = piedmont, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft")

#axes 3&4
scatter(dapc.p2, xax=3, yax=4, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")
