#Making the ADMIXTURE plots for all samples
#Neha Savant
# April 23, 2018

#set the working directory
setwd("~/Dropbox/Columbia_New_York/Salamanders/Downstream_Analyses/all_ADMIXTURE/")

# install dependencies and devtools
install.packages(c("Cairo","ggplot2","gridExtra","gtable","tidyr","devtools"),dependencies=T)

# install pophelper package from GitHub
devtools::install_github('royfrancis/pophelper')

# load library for use
library(pophelper)

tbl=read.table("batch_1_all.3.Q")
barplot(t(as.matrix(tbl)), col=rainbow(3),
        xlab="Individual #", ylab="Ancestry", border=NA)

names=dataframe(labs=c("CAPO", "COOK", "HAKI", "HARI", "LNIS", "LOCK", "MUCK2", "MUCK1", "MUCK3", "WICK", "SVEN", "SWAR", "WHIT", "WPOA"))

x <- readQ("batch_1_all.3.Q")
plotQ(x, returnplot=T, titlelab="ADMIXTURE for all samples", titlesize=5, showlegend=TRUE, ordergrp=TRUE, grplab=data.frame(labs=c("CAPO", "COOK", "HAKI", "HARI", "LNIS", "LOCK", "MUCK2", "MUCK1", "MUCK3", "WICK", "SVEN", "SWAR", "WHIT", "WPOA"),stringsAsFactors=F))
