args = commandArgs(trailingOnly=TRUE) 
library(qqman) 
reportList <- list.files(pattern="RAiSD_Report.CEW_2002_sweep.CM") 
data <- data.frame(pos=c(), value=c(), chr=c()) 
for (i in 1:length(reportList)) { 
  d <- read.table(reportList[i], header=F, skip=0)[,1:7] 
  tmp.dat <- data.frame(pos=d[,1], value=d[,7]*1, chr=rep(i, length(d[,1]))) 
  data <- rbind(data, tmp.dat)} 
png("CEW_2002_sweep.png", width = 1600, height = 800, units = "px") 
snp <- 1:dim(data)[1] 
mydf_2002 <- data.frame(snp, data) 
thres<-as.numeric(0.9995) 
topQ<-thres*100 
threshold <- quantile(x=data$value, probs = thres) 
title_msg <- paste("Manhattan plot for 2002 trap", "threshold=",threshold," (", topQ,"%)") 
manhattan(mydf_2002, chr="chr", bp="pos", cex = 0.5, p="value", snp="snp", logp=F,  ylab=bquote(mu ~ "statistic"), cex.axis=2, cex.lab=2, genomewideline = FALSE, suggestiveline = FALSE,pos=0, col=c("darkred", "black"), ylim=c(0.0, 300)) 
title(title_msg) 
abline(h=threshold, col="red", lw=1, lty=2) 
dev.off()


args = commandArgs(trailingOnly=TRUE) 
library(qqman) 
reportList <- list.files(pattern="RAiSD_Report.CEW_2012_sweep.CM") 
data <- data.frame(pos=c(), value=c(), chr=c()) 
for (i in 1:length(reportList)) { 
  d <- read.table(reportList[i], header=F, skip=0)[,1:7] 
  tmp.dat <- data.frame(pos=d[,1], value=d[,7]*1, chr=rep(i, length(d[,1]))) 
  data <- rbind(data, tmp.dat)} 
png("CEW_2012_sweep.png", width = 1600, height = 800, units = "px") 
snp <- 1:dim(data)[1] 
mydf_2012 <- data.frame(snp, data) 
thres<-as.numeric(0.9995) 
topQ<-thres*100 
threshold <- quantile(x=data$value, probs = thres) 
title_msg <- paste("Manhattan plot for 2012 trap", "threshold=",threshold," (", topQ,"%)") 
manhattan(mydf_2012, chr="chr", bp="pos", cex = 0.5, p="value", snp="snp", logp=F,  ylab=bquote(mu ~ "statistic"), cex.axis=2, cex.lab=2, genomewideline = FALSE, suggestiveline = FALSE,pos=0, col=c("darkred", "black"), ylim=c(0.0, 300)) 
title(title_msg) 
abline(h=threshold, col="red", lw=1, lty=2) 
dev.off()


args = commandArgs(trailingOnly=TRUE) 
library(qqman) 
reportList <- list.files(pattern="RAiSD_Report.CEW_2017_sweep.CM") 
data <- data.frame(pos=c(), value=c(), chr=c()) 
for (i in 1:length(reportList)) { 
  d <- read.table(reportList[i], header=F, skip=0)[,1:7] 
  tmp.dat <- data.frame(pos=d[,1], value=d[,7]*1, chr=rep(i, length(d[,1]))) 
  data <- rbind(data, tmp.dat)} 
png("CEW_2017_sweep.png", width = 1600, height = 800, units = "px") 
snp <- 1:dim(data)[1] 
mydf_2017 <- data.frame(snp, data) 
thres<-as.numeric(0.9995) 
topQ<-thres*100 
threshold <- quantile(x=data$value, probs = thres) 
title_msg <- paste("Manhattan plot for 2017 trap", "threshold=",threshold," (", topQ,"%)") 
manhattan(mydf_2017, chr="chr", bp="pos", cex = 0.5, p="value", snp="snp", logp=F,  ylab=bquote(mu ~ "statistic"), cex.axis=2, cex.lab=2, genomewideline = FALSE, suggestiveline = FALSE,pos=0, col=c("darkred", "black"), ylim=c(0.0, 300)) 
title(title_msg) 
abline(h=threshold, col="red", lw=1, lty=2) 
dev.off()