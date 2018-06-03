#NS - All long-tail salamanders WITHOUT SWARTSWOOD

install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)

#load data from Wickecheoke stream salamanders
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/all_DAPC/")

#set working directory session- set direßctory to Suburban sals- statistics DAPC
c1<-read.genepop("batch_1_noSWAR.gen")
c1
str(c1)
pop(c1)

c1clust<- find.clusters(c1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
150 #Retaining ~80%
2 #the lowest BIC

c2clust<- find.clusters(c1, max.n.clust=40, n.pca=NULL)
150
3

popname1=list("CAPO-s", "COOK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "MUCK2-p", "MUCK1-p", "MUCK3-p", "WICK-s", "SVEN-p", "SWAR-p", "WARF-s", "WHIT-s", "WPOA-p")

table(pop(c1), c2clust$grp)
table.value(table(pop(c1), c1clust$grp), col.lab=paste("cluster", 1:3), row.lab=paste(popname1))

#Run first dapc on b1clust
dapc.c1<- dapc (c1, c2clust$grp) #retained 3 DAs
scatter(dapc.c1,cell = 0, pch = 18:23, cstar = 0, label = popname1, mstree = FALSE, lwd = 2, lty = 2)

#Running the DAPC
dapc.c1 <- dapc(c1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
#retain 10 DA
dapc.c1$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.c1$eig/(sum(dapc.c1$eig)))*100,2)
eig_percent #90.12  9.88
scatter(dapc.c1, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.c1, n.pca=1:ncol(dapc.c1$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=10) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=7
dapc.c2 <- dapc(c1, var.contrib = TRUE, scale = FALSE, n.pca = 7, n.da = 10) #change PCs to the optimal
scatter(dapc.c2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

dapc.c1.scatter<- scatter(dapc.c2, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="bottomright", ncol=2)
legend(dapc.c1.scatter, position= "upperleft", ncol=2)
dapc.b2

#Loading plot
loadingplot(dapc.c2$var.contr, axis=2, thres=0.07, lab.jitter=1) #there are no alleles that contribute disproporitonately to the DAPC. 

#compoplot (STRUCTURE-like plot). Not sure how to use this properly.
compoplot(dapc.c2, posi="bottomright", txt.leg=paste("Cluster", 1:14), lab="", ncol=1, xlab="inidividuals")

#Second compoplot 
popname1=list("CAPO-s", "COOK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "MUCK2-p", "MUCK1-p", "MUCK3-p", "WICK-s", "SVEN-p", "WARF-s", "WHIT-s", "WPOA-p")
streams=list("CAPO-s", "COOK-s", "WICK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "WARF-s")

compoplot(dapc.c2, cleg=0.4, posi=list(x=0, y=-0), lab=popname1, only.grp=streams)

#sum stats of a scores for your DAPC
summary(dapc(c1, n.da=10, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.c2$eig/(sum(dapc.c2$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

#axes 1&2
popname_noSWAR=list("CAPO-s", "COOK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "MUCK2-p", "MUCK1-p", "MUCK3-p", "WICK-s", "SVEN-p", "WARF-s", "WHIT-s", "WPOA-p")

scatter(dapc.c2, xax=1, yax=2, pch = 18:25, txt.leg = popname_noSWAR, label=popname_noSWAR, cleg=0.7, legend=TRUE, mstree = FALSE, scree.da=TRUE, ratio.da= 0.179, posi.da = "bottomleft", clabel=0.7, cellipse=TRUE, posi.leg="bottomright", cex=2)

#axes 2&3
scatter(dapc.c2, xax=2, yax=3, pch = 18:25, txt.leg = popname_noSWAR, label=popname_noSWAR, cleg=0.7, legend=TRUE, mstree = FALSE, scree.da=TRUE, ratio.da= 0.179, posi.da = "bottomright", clabel=0.6, cellipse=TRUE, posi.leg="topleft")

#axes 3&4
scatter(dapc.c2, xax=3, yax=4, pch = 18:25, txt.leg = popname_noSWAR, cstar=0, cleg=0.7, legend=TRUE, mstree = FALSE, scree.da=TRUE, ratio.da= 0.179, posi.da = "bottomright", clabel=0.0001, cellipse=TRUE, posi.leg="topleft")


