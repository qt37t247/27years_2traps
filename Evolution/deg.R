# Load libraries
library(DESeq2)
library(tximport)
library(ggplot2)
library(ggrepel)
library(dplyr)

# ================= CONFIGURATION =================
# 1. Load Metadata
coldata <- read.csv("metadata.csv", row.names = 1)

# 2. Locate Salmon Output Files
# Ensure the folder names match the SRA IDs in your metadata
files <- file.path(paste0(rownames(coldata), "_quant"), "quant.sf")
names(files) <- rownames(coldata)

# Check if files exist
if(!all(file.exists(files))) stop("Some quantification files are missing!")

# 3. Import Data
# Note: For gene-level analysis, you usually need a 'tx2gene' data frame.
# For this script, we will set txOut=TRUE to analyze at the transcript level 
# to keep it generic. If you have a tx2gene map, set txOut=FALSE and add tx2gene=tx2gene_df.
txi <- tximport(files, type = "salmon", txOut = TRUE)

# ================= DESEQ2 ANALYSIS =================

# Create DESeqDataSet
# Ensure 'condition' matches the column name in your metadata.csv
dds <- DESeqDataSetFromTximport(txi, colData = coldata, design = ~ condition)

# Run Differential Expression
dds <- estimateSizeFactors(dds)
dispersions(dds) <- 0.1
dds <- nbinomWaldTest(dds)

# Get Results (Contrast: Treatment vs Control)
# Change "Treatment" and "Control" to match your metadata values
res <- results(dds, contrast=c("condition", "Head_F", "Thorax_F"))

# Shrink LFC for better visualization (Optional but recommended)
#resLFC <- lfcShrink(dds, coef=resultsNames(dds)[2], type="apeglm")

# Convert to Data Frame
res_df <- as.data.frame(res)
res_df$gene <- rownames(res_df)

# ================= VOLCANO PLOT =================

# Define thresholds for coloring
log2fc_thresh <- 1
pval_thresh <- 0.05

# Categorize genes
res_df <- res_df %>%
  mutate(diffexpressed = case_when(
    log2FoldChange > log2fc_thresh & padj < pval_thresh ~ "UP",
    log2FoldChange < -log2fc_thresh & padj < pval_thresh ~ "DOWN",
    TRUE ~ "NO"
  ))

write.csv(res_df, "Head_F_Thorax_F.csv")

# Create the plot
p <- ggplot(res_df, aes(x = log2FoldChange, y = -log10(padj), col = diffexpressed, label = gene)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_color_manual(values = c("DOWN" = "blue", "NO" = "grey", "UP" = "red")) +
  geom_vline(xintercept = c(-log2fc_thresh, log2fc_thresh), col = "black", linetype = "dashed") +
  geom_hline(yintercept = -log10(pval_thresh), col = "black", linetype = "dashed") +
  labs(title = "Volcano Plot: Head_M vs Head_F",
       x = "Log2 Fold Change",
       y = "-Log10 Adjusted P-value") 

# Add labels to top significant genes (Top 10 by p-value)
top_genes <- head(res_df[order(res_df$padj), ], 10)
p <- p + geom_text_repel(data = top_genes, aes(label = gene), max.overlaps = 20)

# Save plot
ggsave("Head_F_Thorax_F.png", plot = p, width = 8, height = 6, dpi = 300)

print("Analysis Complete. Plot saved as volcano_plot.png")
