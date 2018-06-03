#NS - ALL long-tail salamanders

install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)
library(RColorBrewer)

#load data from all salamanders
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/all_DAPC")

#set working directory session- set direßctory to Suburban sals- statistics DAPC
a1<-read.genepop("batch_1.gen")
a1
str(a1)
pop(a1)

a1clust<- find.clusters(a1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
175 #It looks like at least 90% of the variance is explained by retaining 200 PCs, could even go down to 175 if need be. 
3 #the lowest BIC is likely 3

popname1=list("CAPO-s", "COOK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "MUCK2-p", "MUCK1-p", "MUCK3-p", "WICK-s", "SVEN-p", "SWAR-p", "WARF-s", "WHIT-s", "WPOA-p")

table(pop(a1), a1clust$grp)
table.value(table(pop(a1), a1clust$grp), col.lab=paste("cluster", 1:3), row.lab=paste(popname1))

#Run first dapc on b1clust
dapc.a1<- dapc(a1, a1clust$grp)

#Running the DAPC
dapc.a1 <- dapc(a1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
#retain 10 DA
dapc.a1$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.a1$eig/(sum(dapc.a1$eig)))*100,2)
eig_percent #90.12  9.88
scatter(dapc.a1, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.a1, n.pca=1:ncol(dapc.a1$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=10) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=7
dapc.a2 <- dapc(all, var.contrib = TRUE, scale = FALSE, n.pca = 7, n.da = 10) #change PCs to the optimal
scatter(dapc.a2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

dapc.a2scatter<- scatter(dapc.a2, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.a2scatter, position= "upperleft", ncol=2)
dapc.a2

#Loading plot
loadingplot(dapc.a2$var.contr, axis=2, thres=0.07, lab.jitter=1) #there are no alleles that contribute disproporitonately to the DAPC. 

#compoplot (STRUCTURE-like plot). Not sure how to use this properly.
compoplot(dapc.a2, posi="bottomright", txt.leg=paste("Cluster", 1:14), lab="", ncol=1, xlab="inidividuals")

#Second compoplot 
popname1=list("CAPO-s", "COOK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "MUCK2-p", "MUCK1-p", "MUCK3-p", "WICK-s", "SVEN-p", "SWAR-p", "WARF-s", "WHIT-s", "WPOA-p")
streams=list("CAPO-s", "COOK-s", "WICK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "WARF-s")

compoplot(dapc.a2, cleg=0.4, posi=list(x=0, y=-0), lab=popname1, only.grp=streams)

#sum stats of a scores for your DAPC
summary(dapc(a1, n.da=5, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.a2$eig/(sum(dapc.a2$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

#axes 1&2
popname1=list("CAPO-s", "COOK-s", "HAKI-s", "HARI-s", "LNIS-s", "LOCK-s", "MUCK2-p", "MUCK1-p", "MUCK3-p", "WICK-s", "SVEN-p", "SWAR-p", "WARF-s", "WHIT-s", "WPOA-p")

display.brewer.all()
spec <- brewer.pal(11, "Spectral") #col = spec,
ryg <- brewer.pal(11, "RdYlGn") #col = ryg,

scatter(dapc.a2, xax=1, yax=2, pch = 18:25, txt.leg = popname1, label=popname1,  cex.lab = 2, cleg=0.6, mstree = FALSE, legend = TRUE,  scree.da=TRUE, ratio.da= 0.17, posi.da = "bottomleft", clabel=0.85, cellipse=TRUE, posi.leg="topleft", cex =1)

scatter(dapc.a2, xax=1, yax=2, pch = 18:25, txt.leg = popname1, label=popname1,  cex.lab = 2, mstree = FALSE,  scree.da=TRUE, ratio.da= 0.17, posi.da = "bottomleft", clabel=0.85, cellipse=TRUE, posi.leg="topleft", cex =1)

title("DAPC with All Individuals") #Set Title

#axes 2&3
scatter(dapc.a2, xax=2, yax=3, pch = 18:25, txt.leg = popname1, label=popname1, cleg=0.75, legend=TRUE, mstree = FALSE, scree.da=TRUE, ratio.da= 0.179, posi.da = "bottomright", clabel=0.6, cellipse=TRUE, posi.leg="topleft")

#axes 3&4
scatter(dapc.a2, xax=3, yax=4, pch = 18:25, txt.leg = popname, cstar=0, cleg=0.7, legend=TRUE, mstree = FALSE, scree.da=TRUE, ratio.da= 0.179, posi.da = "bottomright", clabel=0.0001, cellipse=TRUE, posi.leg="topleft")


