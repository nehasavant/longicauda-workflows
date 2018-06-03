 #NS - Wickecheoke salamanders

install.packages("adegenet", dep=TRUE)
library(poppr)
library(adegenet)
library(ade4)

#load data from Wickecheoke stream salamanders
setwd("~/RADseq/DAPC_wick")

#set working directory session- set directory to Suburban sals- statistics DAPC
b2<-read.genepop("batch_1.gen")
b2
str(b2)
pop(b2) #For some reason it is reading the popuations wrong. OLD009, UPP022, UPP021 and WICK015 are the populations names. These are the names of the final samples in each pop...

nameStrata(b2)

b1clust<- find.clusters(b2, max.n.clust=40, n.pca=NULL) #finding clusters, first set to max PCs and retain lowest point of BIC
40# finding PCs
#2# finding clusters

table(pop(b1), b1clust$grp)
table.value(table(pop(b1), b1clust$grp), col.lab=paste("inf", 1:14), row.lab=paste("ori", 1:14))

#Run first dapc on b1clust
dapc.b1<- dapc (b2, b1clust$grp)

#Running the DAPC
dapc.b2 <- dapc(b2, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = NULL) #run the DAPC
4#retain 11 DA
dapc.b2$eig
#Figure out eigenvalue percent of variability in each discrimnant function
eig_percent <- round((dapc.b2$eig/(sum(dapc.b2$eig)))*100,2)
eig_percent
scatter(dapc.b2, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.b2, n.pca=1:ncol(dapc.b2$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=4) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=5
dapc.b3 <- dapc(b2, var.contrib= TRUE, scale = FALSE, n.pca = 5, n.da = 5) #change PCs to the optimal
dapc.b3scatter<- scatter(dapc.b3, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topleft", ncol=2)
legend(dapc.b2scatter, position= "upperleft", ncol=2)
dapc.b2

#sum stats of a scores for your DAPC
summary(dapc(b2, n.da=5, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores

#proportion of variable explained by PC
eig_percent <- round((dapc.b2$eig/(sum(dapc.b2$eig)))*100,2)
# what are the percent variance explained by PC1 and PC2?
eig_percent[1:7]

wick=list("UPPER", "MID A", "MID B", "LOWER")

#axes 1&2
scatter(dapc.b3, xax=1, yax=2, pch = 18:25, cstar = 0, txt.leg = wick, label = wick, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")

#axes 2&3
scatter(dapc.b3, xax=2, yax=3, pch = 18:25, cstar = 0, txt.leg = wick, label = wick, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")
#axes 3&4
scatter(dapc.b2, xax=3, yax=4, pch = 18:25, cstar = 0, mstree = FALSE, scree.da=TRUE, posi.da = "bottomleft", ratio.da= 0.179, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, legend=TRUE, posi.leg="topright")


####JUST on PRESERVE MIANUS POPS#########
#set working directory session- set directory
b1<-read.genepop("batch_1a.gen")
b1
str(b1)
pop(b1)

#rescale data by dealing with NA)
x.b1.EB <- scaleGen(b1, NA.method= c("zero"), scale=FALSE)
View(x.b1.DF)

b1clust<- find.clusters(b1, max.n.clust=30) #finding clusters
125# finding PCs
2# finding clusters

#Run first dapc
dapc.b1<- dapc (b1, b1clust$grp)
dapc.b1

#Running the DAPC
dapc.b1 <- dapc(b1, var.contrib = TRUE, scale = FALSE, n.pca = 40, n.da = 2) #run the DAPC
scatter(dapc.b1, cell = 0, pch = 18:23, cstar = 0, mstree = FALSE, lwd = 2, lty = 2) #run the scatter of the DAPC

#calculating the optimum PC number to rerun DAPC
optim.a.score(dapc.b1, n.pca=1:ncol(dapc.b1$tab), smart=TRUE, n=10, plot=TRUE, n.sim=10, n.da=2) #calculating optimal number of PCs

#rerun DAPC with optimum PCs=7
dapc.b1 <- dapc(b1, var.contrib = TRUE, scale = FALSE, n.pca = 7, n.da = 2) #change PCs to the optimal
scatter(dapc.b1, pch = 18:27, cstar = 0, mstree = FALSE, scree.da=FALSE, legend=TRUE, cex=1.0, cex.lab=0.5, cex.main=0.5, cellipse=TRUE, posi.leg="bottomright")

#sum stats of a scores for your DAPC
summary(dapc(b1, n.da=2, n.pca=7))$assign.per.pop #summary stats for your PC presenting a-scores
