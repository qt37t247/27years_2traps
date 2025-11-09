args = commandArgs(trailingOnly=TRUE) 
library(qqman) 
reportList <- list.files(pattern="RAiSD_Report.CEW_light_sweep.CM") 
data <- data.frame(pos=c(), value=c(), chr=c()) 
for (i in 1:length(reportList)) { 
  d <- read.table(reportList[i], header=F, skip=0)[,1:7] 
  tmp.dat <- data.frame(pos=d[,1], value=d[,7]*1, chr=rep(i, length(d[,1]))) 
  data <- rbind(data, tmp.dat)} 
png("CEW_light_sweep.png", width = 1600, height = 800, units = "px") 
snp <- 1:dim(data)[1] 
mydf_light <- data.frame(snp, data) 
thres<-as.numeric(0.9995) 
topQ<-thres*100 
threshold <- quantile(x=data$value, probs = thres) 
title_msg <- paste("Manhattan plot for light trap", "threshold=",threshold," (", topQ,"%)") 
manhattan(mydf_light, chr="chr", bp="pos", cex = 0.5, p="value", snp="snp", logp=F,  ylab=bquote(mu ~ "statistic"), cex.axis=2, cex.lab=2, genomewideline = FALSE, suggestiveline = FALSE,pos=0, col=c("darkred", "black"), ylim=c(0.0, 500)) 
title(title_msg) 
abline(h=threshold, col="red", lw=1, lty=2) 
dev.off()

write.csv(mydf_light, "light_all.csv")

reportList <- list.files(pattern="RAiSD_Report.CEW_phero_sweep.CM") 
data <- data.frame(pos=c(), value=c(), chr=c()) 
for (i in 1:length(reportList)) { 
  d <- read.table(reportList[i], header=F, skip=0)[,1:7] 
  tmp.dat <- data.frame(pos=d[,1], value=d[,7]*1, chr=rep(i, length(d[,1]))) 
  data <- rbind(data, tmp.dat)} 
png("CEW_phero_sweep.png", width = 1600, height = 800, units = "px") 
snp <- 1:dim(data)[1] 
mydf_phero <- data.frame(snp, data) 
thres<-as.numeric(0.9995) 
topQ<-thres*100 
threshold <- quantile(x=data$value, probs = thres) 
title_msg <- paste("Manhattan plot for pheromone trap", "threshold=",threshold," (", topQ,"%)") 
manhattan(mydf_phero, chr="chr", bp="pos", cex = 0.5, p="value", snp="snp", logp=F,  ylab=bquote(mu ~ "statistic"), cex.axis=2, cex.lab=2, genomewideline = FALSE, suggestiveline = FALSE,pos=0, col=c("darkred", "black"), ylim=c(0.0, 500)) 
title(title_msg) 
abline(h=threshold, col="red", lw=1, lty=2) 
dev.off()

write.csv(mydf_phero, "phero_all.csv")

light_out <- mydf_light[mydf_light$value>39.8, ]
phero_out <- mydf_phero[mydf_phero$value>49.2, ]

write.csv(light_out, "light_out.csv")
write.csv(phero_out, "phero_out.csv")

## Attempt to plot different sites
# ## finding ranges
# light_out_range <- light_out
# light_out_range$start <- light_out_range$pos - 50000
# light_out_range$end <- light_out_range$pos + 50000
# 
# light_ranges <- data.frame(start=numeric(), end=numeric(), chr=factor()) 
# 
# for (j in unique(light_out_range$chr)) {
#   
#   df <- light_out_range[light_out_range$chr==j, ]
#   df <- df[order(df$start), ]
#   
#   merged_ranges <- list()
#   current_start <- df$start[1]
#   current_end <- df$end[1]
#   
#   for (i in 2:nrow(df)) {
#     next_start <- df$start[i]
#     next_end <- df$end[i]
#     
#     if (next_start <= current_end + 1) {
#       # Ranges overlap or are adjacent → merge
#       current_end <- max(current_end, next_end)
#     } else {
#       # No overlap → store current range and move to the next
#       merged_ranges[[length(merged_ranges) + 1]] <- c(current_start, current_end)
#       current_start <- next_start
#       current_end <- next_end
#     }
#     
#     merged_ranges[[length(merged_ranges) + 1]] <- c(current_start, current_end)
#     
#     covered_ranges <- do.call(rbind, merged_ranges)
#     colnames(covered_ranges) <- c("start", "end")
#     covered_ranges <- as.data.frame(covered_ranges)
#     covered_ranges$chr <- j
#     
#   }
#   
#   light_ranges <- rbind(light_ranges, covered_ranges)
#   
# }
# 
# write.csv(light_ranges, "light_ranges.csv")
# 
# 
# 
# phero_out_range <- phero_out
# phero_out_range$start <- phero_out_range$pos - 50000
# phero_out_range$end <- phero_out_range$pos + 50000
# 
# phero_ranges <- data.frame(start=numeric(), end=numeric(), chr=factor()) 
# 
# for (j in unique(phero_out_range$chr)) {
#   
#   df <- phero_out_range[phero_out_range$chr==j, ]
#   df <- df[order(df$start), ]
#   
#   merged_ranges <- list()
#   current_start <- df$start[1]
#   current_end <- df$end[1]
#   
#   for (i in 2:nrow(df)) {
#     next_start <- df$start[i]
#     next_end <- df$end[i]
#     
#     if (next_start <= current_end + 1) {
#       # Ranges overlap or are adjacent → merge
#       current_end <- max(current_end, next_end)
#     } else {
#       # No overlap → store current range and move to the next
#       merged_ranges[[length(merged_ranges) + 1]] <- c(current_start, current_end)
#       current_start <- next_start
#       current_end <- next_end
#     }
#     
#     merged_ranges[[length(merged_ranges) + 1]] <- c(current_start, current_end)
#     
#     covered_ranges <- do.call(rbind, merged_ranges)
#     colnames(covered_ranges) <- c("start", "end")
#     covered_ranges <- as.data.frame(covered_ranges)
#     covered_ranges$chr <- j
#     
#   }
#   
#   phero_ranges <- rbind(phero_ranges, covered_ranges)
#   
# }
# 
# write.csv(phero_ranges, "phero_ranges.csv")

### Manually delete replicates to create new ranges in csv

# library(scales)
# 
# light_ranges <- read.csv("light_ranges.csv")
# phero_ranges <- read.csv("phero_ranges.csv")

# pdf("chrs.pdf", height = 3)
# 
# for (i in 1:31){
#   
#   light_rangez <- light_ranges[light_ranges$chr==i,]
#   phero_rangez <- phero_ranges[phero_ranges$chr==i,]
#   plot(1, type="n", xlab="", ylab="", xlim=c(0, max(phero_rangez$end)), ylim=c(0, 1), main = paste("chr",i))
#   segments(x0=light_rangez$start, y0=0.4, x1=light_rangez$end, y1=0.4, lwd = 4, col = alpha("orange", 0.4))
#   segments(x0=phero_rangez$start, y0=0.6, x1=phero_rangez$end, y1=0.6, lwd = 4, col = alpha("blue", 0.4))
#   
# }
# 
# dev.off()

## Identify genes within the range of selected genomic region.
library(biomaRt)

write.csv(listDatasets(useMart(biomart = "metazoa_mart", host = "https://metazoa.ensembl.org")), "list.csv")

ensembl <- useMart(biomart = "metazoa_mart", host = "https://metazoa.ensembl.org", dataset = "hzgca022581195v1rs_eg_gene")

gene_data <- getBM(
  attributes = c('ensembl_gene_id', 'description', 'external_gene_name', 'go_id', 'chromosome_name', 'start_position', 'end_position'),
  mart = ensembl
)


light_out$chr <- as.factor(light_out$chr)
levels(light_out$chr) <- sort(unique(gene_data$chromosome_name))[1:31]

phero_out$chr <- as.factor(phero_out$chr)
levels(phero_out$chr) <- sort(unique(gene_data$chromosome_name))[1:31]

find_genes <- function(chromosome, position) {
  result <- getBM(
    attributes = c('ensembl_gene_id', 'description', 'geneid', 'go_id', 'chromosome_name', 'start_position', 'end_position'),
    filters = c('chromosome_name', 'start', 'end'),
    values = list(chromosome, position, position),
    mart = ensembl
  )
  return(result)
}


genes_out_light <- data.frame()

for (i in seq_len(nrow(light_out))) {
  chromosome <- light_out$chr[i]
  position <- light_out$pos[i]
  
  # Find genes for this range
  genes <- find_genes(chromosome, position)
  genes_out_light <- rbind(genes_out_light, genes)
}

write.csv(genes_out_light, "genes_out_light.csv")


genes_out_phero <- data.frame()

for (i in seq_len(nrow(phero_out))) {
  chromosome <- phero_out$chr[i]
  position <- phero_out$pos[i]
  
  # Find genes for this range
  genes <- find_genes(chromosome, position)
  genes_out_phero <- rbind(genes_out_phero, genes)
}

write.csv(genes_out_phero, "genes_out_phero.csv")
