#DAPC with Pond-complex only 
install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)

#load data from Wickecheoke stream salamanders
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/pond_complex_DAPC/")

#set working directory session- set directory to Suburban sals- statistics DAPC
x1<-read.genepop("batch_1_pondcomplex.gen")
x1
str(x1)
pop(x1) 

nameStrata(n2)

x1clust<- find.clusters(x1, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
80# finding PCs
2# finding clusters

table(pop(x1), x1clust$grp)
table.value(table(pop(x1), x1clust$grp), col.lab=paste("inf", 1:5), row.lab=paste("ori", 1:5))

#Run first dapc on b1clust
dapc.x1<- dapc (x1, x1clust$grp)

#Running the DAPC
dapc.x2 <- dapc(x1, var.contrib = TRUE, scale = FALSE, n.pca = 60, n.da = NULL) #run the DAPC
3 #retain 3 DA
dapc.x2$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.x2$eig/(sum(dapc.x2$eig)))*100,2)
eig_percent
scatter(dapc.x2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.x2, n.pca=1:ncol(dapc.x2$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=3) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=7
dapc.x3 <- dapc(x1, var.contrib= TRUE, scale = FALSE, n.pca = 7, n.da = 3) #change PCs to the optimal
dapc.x3.scatter<- scatter(dapc.x3, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.x2scatter, position= "upperleft", ncol=2)
dapc.r3

#sum stats of a scores for your DAPC
summary(dapc(x1, n.da=6, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.x3$eig/(sum(dapc.x3$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

complex=list("MUCK2", "MUCK1", "MUCK3", "WHIT", "WPOA")

#axes 1&2
scatter(dapc.x3, xax=1, yax=2, pch = 18:25, txt.leg = complex, label = complex, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#axes 2&3
scatter(dapc.x3, xax=2, yax=3, pch = 18:25,  txt.leg = complex, label = complex, mstree = FALSE, scree.da=TRUE, posi.da = "bottomright", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft")

#axes 3&4
scatter(dapc.x3, xax=3, yax=4, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

