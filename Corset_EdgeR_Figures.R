## Check for packages and install if missing
packages <-  c("tidyverse", "reshape2", "edgeR",
               "scales", "plyr", "RColorBrewer",
               "corrplot", "cba", "grid", "gridExtra",
               "biomaRt", "htmlTable", "statmod",
               "DESeq2", "genefilter", "qvalue",
               "rgl", "R2HTML", "mixOmics", "GGally",
               "cluster", "geneplotter", "spatstat",
               "VennDiagram", "viridis", "DESeq2")
for( package in packages ){
  if(!require( package, character.only = TRUE )){
    install.packages( package )
    require( package, character.only = TRUE )
  }
}


## Load libraries
library(plyr)
library(GGally)
library(scales)
library(RColorBrewer)
library(corrplot)
library(statmod)
library(mixOmics)
library(spatstat)
library(gplots)
library(cluster)
library(geneplotter)
library(VennDiagram)
library(viridis)
library(gridExtra)
library(tidyverse)
library(DESeq2)
library(edgeR)
library(ggpubr)


## Analyse Corset gene clusters
#import the data and name groups
count_data <- read.table("counts.txt", header = TRUE)
colnames(count_data) <- c("2-cell_A","2-cell_B","2-cell_D",
                          "4-cell_A","4-cell_B","4-cell_C","4-cell_D",
                          "8-cell_B","8-cell_C","8-cell_D")

#group samples by developmental stage
group <- factor(c(1,1,1,2,2,2,2,3,3,3))


## Run edgeR
y <- DGEList(counts=count_data,
             group=group, remove.zeros = TRUE)
y <- calcNormFactors(y)
design <- model.matrix(~group)
y <- estimateDisp(y, design)


## Apply a general linear model (GLM)
fit <- glmFit(y, design)

#run likelihood ratio tests for each dev stage
lrt.2vs1 <- glmLRT(fit, coef=2)
lrt.3vs1 <- glmLRT(fit, coef=3)
lrt.3vs2 <- glmLRT(fit, contrast=c(0,-1,1))
lrt <- glmLRT(fit, coef=2:3)
topTags(lrt)

#summarise DE genes
summary(decideTests(lrt.2vs1))
summary(decideTests(lrt.3vs1))
summary(decideTests(lrt.3vs2))

#write output to file (inspect to identify bias in samples)
output2v1 <- order(lrt.2vs1$table$PValue)
signs_of_contamination <- cpm(y)[output2v1[1:1000],]
write.table(signs_of_contamination, file = "top1000")


## Calculate DE with the GLM Quasi-Likelihood approach
fit2 <- glmQLFit(y, design)

#calculate DE using F-tests (ANOVA)
qlftest <- glmQLFTest(fit2, coef=2:3)
result_glmQLFTest <- glmQLFTest(fit2, coef=ncol(fit2$design))

#examine the top DE genes and compare with the lrt method
topTags(result_glmQLFTest)
is.de.all <- decideTestsDGE(result_glmQLFTest)


#summarise DE genes across all 3 groups and write to file
summary(is.de.all)
allTags <- topTags(result_glmQLFTest,n=dim(result_glmQLFTest$table)[1])[[1]]
DEgenes=rownames(allTags)[allTags$FDR<0.05]
write.table(DEgenes,"DEgenes_all.txt",
            quote=F,row.names=F,col.names=F)
export_gene_counts <- qlftest$table
write.table(export_gene_counts,"export_DE_results_for_all_genes.txt",
            quote=F)
fitted_counts <- qlftest$fitted.values
write.table(fitted_counts,"fitted_counts.txt",
            quote=F)

#calculate DE genes for 2-cell vs 4-cell groups
qlftest_2vs1 <- glmQLFTest(fit2, coef=2)
is.de.2vs1 <- decideTestsDGE(qlftest_2vs1)
summary(is.de.2vs1)
qlftest_2vs1_allTags <- topTags(qlftest_2vs1,n=dim(qlftest_2vs1$table)[1])[[1]]
qlftest_2vs1_DEgenes=rownames(qlftest_2vs1_allTags)[qlftest_2vs1_allTags$FDR<0.05]
write.table(qlftest_2vs1_DEgenes,"qlftest_2vs1_DEgenes_all.txt",
            quote=F,row.names=F,col.names=F)

#calculate DE genes for 4-cell vs 8-cell groups
qlftest_3vs2 <- glmQLFTest(fit2, contrast=c(0,-1,1))
is.de.3vs2 <- decideTestsDGE(qlftest_3vs2)
summary(is.de.3vs2)
qlftest_3vs2_allTags <- topTags(qlftest_3vs2,n=dim(qlftest_3vs2$table)[1])[[1]]
qlftest_3vs2_allTags_DEgenes=rownames(qlftest_3vs2_allTags)[qlftest_3vs2_allTags$FDR<0.05]
write.table(qlftest_3vs2_allTags_DEgenes,"qlftest_3vs2_DEgenes_all.txt",
            quote=F,row.names=F,col.names=F)

#calculate DE genes for 2-cell vs 8-cell groups
qlftest_3vs1 <- glmQLFTest(fit2, coef=3)
topTags(qlftest_3vs1)
is.de.3vs1 <- decideTestsDGE(qlftest_3vs1)
summary(is.de.3vs1)
qlftest_3vs1_allTags <- topTags(qlftest_3vs1,n=dim(qlftest_3vs1$table)[1])[[1]]
qlftest_3vs1_DEgenes=rownames(qlftest_3vs1_allTags)[qlftest_3vs1_allTags$FDR<0.05]
write.table(qlftest_3vs1_DEgenes,"qlftest_3vs1_DEgenes_all.txt",
            quote=F,row.names=F,col.names=F)


## Perform TREAT analysis (retain genes with logFC >1)
#between 2-cell and 4-cell
tr1vs2 <- glmTreat(fit, coef=2, lfc=1)
topTags(tr1vs2)
tr1vs2_allTags <- topTags(tr1vs2,n=dim(tr1vs2$table)[1])[[1]]
deClusters_tr1vs2=rownames(tr1vs2_allTags)[tr1vs2_allTags$FDR<0.05]
write.table(deClusters_tr1vs2,"deClusters_tr1vs2_PValue05.txt",
            quote=F,row.names=F,col.names=F)

#between 2-cell and 8-cell
tr1vs3 <- glmTreat(fit, coef=3, lfc=1)
topTags(tr1vs3)
tr1vs3_allTags <- topTags(tr1vs3,n=dim(tr1vs3$table)[1])[[1]]
deClusters_tr1vs3=rownames(tr1vs3_allTags)[tr1vs3_allTags$FDR<0.05]
write.table(deClusters_tr1vs3,"deClusters_tr1vs3_PValue05.txt",
            quote=F,row.names=F,col.names=F)

#between 4-cell and 8cell
tr2vs3 <- glmTreat(fit, contrast=c(0,-1,1), lfc=1)
topTags(tr2vs3)
tr2vs3_allTags <- topTags(tr2vs3,n=dim(tr2vs3$table)[1])[[1]]
deClusters_tr2vs3=rownames(tr2vs3_allTags)[tr2vs3_allTags$FDR<0.05]
write.table(deClusters_tr2vs3,"deClusters_tr2vs3_PValue05.txt",
            quote=F,row.names=F,col.names=F)

#summary of DE genes based on P value
summary(tr1vs2_allTags$PValue<0.01)
summary(tr2vs3_allTags$PValue<0.01)

exact12 <- exactTest(y, pair = c(1,2))
results_edgeR <- topTags(exact12, n = nrow(data_clean), sort.by = "none")
exact23 <- exactTest(y, pair = c(2,3))
results_edgeR_2vs3 <- topTags(exact23, n = nrow(data_clean), sort.by = "none")


## Visualize the results
#smear plots and volcano plots to visualize DE
png(filename = "Figure_8.png", width = 18, height = 16,
    units = "cm", res = 300)
#set the number of plots per page
par(mfrow=c(2,2))
#set margins
par(mar = c(4.5, 4, 2, 2)+0.1)
par(oma = c(0,2.5,0,0)+0.1)
#plot A
plotMD(qlftest_2vs1, status=is.de.2vs1, values=c(1,-1),
       col=c("red","blue"), cex = 0.4, main = " ") +
  mtext(c("A"), cex = 1.75, side=3, at=c(-7)) +
  legend("topright",
         as.character(c("Not significant",
                        "Upregulated",
                        "Downregulated")),
         col=c("black","red","blue"),
         pch=20, pt.cex = 0.9, cex = 0.9)

#plot B
tab = data.frame(logFC = et12$table[, 1],
                 negLogPval = -log10(et12$table[, 3]))
plot(tab, pch = 16, cex = 0.5, xlab = expression(log[2]~fold~change),
     ylab = expression(-log[10]~pvalue), xlim=c(-8,12))
lfc = 2
pval = 0.05
signGenes = (abs(tab$logFC) > lfc & tab$negLogPval > -log10(pval))
points(tab[signGenes, ], pch = 16, cex = 0.5, col = "red")
abline(h = -log10(pval), col = "green3", lty = 2)
abline(v = c(-lfc, lfc), col = "blue", lty = 2)
mtext(paste("pval =", pval), side = 2, at = -log10(pval), cex = 0.7, line = 0.5, las = 1)
mtext(c(paste("-", lfc, "fold"), paste("+", lfc, "fold")), side = 1, at = c(-lfc, lfc),
      cex = 0.7, line = 0.5, las=2) +
  mtext(c("B"), cex = 1.75, side=3, at=c(-11))

#plot C
plotMD(qlftest_3vs2, status=is.de.3vs2, values=c(1,-1),
       col=c("red","blue"), cex = 0.4, main = " ") +
  mtext(c("C"), cex = 1.75, side=3, at=c(-7)) +
  legend("topright",
         as.character(c("Not significant",
                        "Upregulated",
                        "Downregulated")),
         col=c("black","red","blue"),
         pch=20, pt.cex = 0.9, cex = 0.9)

#plot D
tab = data.frame(logFC = et32$table[, 1],
                 negLogPval = -log10(et32$table[, 3]))
plot(tab, pch = 16, cex = 0.5, xlab = expression(log[2]~fold~change),
     ylab = expression(-log[10]~pvalue), xlim=c(-8,12))
lfc = 2
pval = 0.05
signGenes = (abs(tab$logFC) > lfc & tab$negLogPval > -log10(pval))
points(tab[signGenes, ], pch = 16, cex = 0.5, col = "red")
abline(h = -log10(pval), col = "green3", lty = 2)
abline(v = c(-lfc, lfc), col = "blue", lty = 2)
mtext(paste("pval =", pval), side = 2, at = -log10(pval), cex = 0.7, line = 0.5, las = 1)
mtext(c(paste("-", lfc, "fold"), paste("+", lfc, "fold")), side = 1, at = c(-lfc, lfc),
      cex = 0.7, line = 0.5, las=2) +
  mtext(c("D"), cex = 1.75, side=3, at=c(-11))

#add labels for groups
mtext(c("2- to 4-cell stage"), cex = 1.25, side = 2,
      outer = TRUE, line = 1, adj = c(0.865))
mtext(c("4- to 8-cell stage"), cex = 1.25, side = 2,
      outer = TRUE, line = 1, adj = c(0.22))

dev.off()


## Plotting PCA (raw data)
logcpm <- cpm(y, prior.count=2, log=TRUE)
pca <- prcomp(t(logcpm), scale. = TRUE)
plot(pca$x[, 1], pca$x[, 2], pch = ".", xlab = "PC1", ylab = "PC2", xlim = c(-600,600), ylim = c(-600,600),  main = "Principle Component Analysis Plot")
text(pca$x[, 1], pca$x[, 2], labels = colnames(logcpm), col=as.numeric(y$samples$group))

## Plot MDS and corrplot (raw data; Figure 7)
pseudoCounts <- log2(y$counts+20)
colnames(pseudoCounts) <- c("2-cell_A","2-cell_B","2-cell_D",
                            "4-cell_A","4-cell_B","4-cell_C","4-cell_D",
                            "8-cell_B","8-cell_C","8-cell_D")
col <- c("#00C17B","#00C17B","#00C17B",
         "#236790","#236790","#236790","#236790",
         "#a37aa0","#a37aa0","#a37aa0")

par(mfrow=c(1,2))
par(oma=c(0,0,0,2))
par(mar=c(4.5, 4.5, 4.5, 0.1)+0.1)
plotMDS(pseudoCounts, top = 375, col=col, cex = 0.9,
        xlim = c(-4,3), ylim = c(-2,2), cex.axis = 0.8,
        ylab = "", xlab = "")
legend("topleft",
       as.character(c("2-cell embryos","4-cell embryos","8-cell embryos")),
       col=c("#00C17B","#236790","#a37aa0"),
       pch=15, pt.cex = 1.4, cex = 0.95)
title(ylab="Leading logFC dim2", line=2.5, cex.lab = 0.8)
title(xlab="Leading logFC dim1", line=2.5, cex.lab = 0.8)
mtext(c("A"), cex = 1.75, side=3, at=c(-5), padj = c(-1))

#plot correlations
par(mar=c(1.5, 1, 0, 2.5)+0.1)
par(cex=0.775)
corrplot.mixed(cor_matrix_spearman, tl.col = "black",
               order = "hclust", hclust.method = "ward.D",
               tl.srt = 45, upper = "shade", cl.cex = 0.95,
               lower = "number", cl.lim = c(0,1),
               tl.pos = "lt",diag = "l")

mtext(c("B"), cex = 1.75, side=3, at=c(-1), padj = c(3.3))

dev.off()

## Cleaning the data (removing low-count gene clusters)
cpm_log <- cpm(count_data, log = TRUE)
median_log2_cpm <- apply(cpm_log, 1, median)
hist(median_log2_cpm)
expr_cutoff <- 2
abline(v = expr_cutoff, col = "red", lwd = 3)
sum(median_log2_cpm > expr_cutoff)
data_clean <- count_data[median_log2_cpm > expr_cutoff, ]

w <- DGEList(counts=data_clean, group=group)
w <- calcNormFactors(w)

keep <- rowSums(cpm(w) > 2) >= 2
w <- w[keep, , keep.lib.sizes=FALSE]
w <- calcNormFactors(w)
keep2 <- rowSums(w$counts) > 100
w <- w[keep2, , keep.lib.sizes=FALSE]
w <- calcNormFactors(w)

## Reanalysis with cleaned data
w <- estimateDisp(w, design)
fit_clean <- glmQLFit(w, design)
plotQLDisp(fit_clean)

res <- glmQLFTest(fit_clean)
tags <- topTags(res)
tags

lrt.2vs1 <- glmLRT(fit_clean, coef=2)
lrt.3vs1 <- glmLRT(fit_clean, coef=3)
lrt.3vs2 <- glmLRT(fit_clean, contrast=c(0,-1,1))
summary(decideTests(lrt.2vs1))
summary(decideTests(lrt.3vs1))
summary(decideTests(lrt.3vs2))



## Plot MDS
png(filename = "Figure_7.png", width = 14, height = 12, units = "cm", res = 300)
par(mar=c(4.5,4.5,0.5,1))
col <- c("#00C17B","#00C17B","#00C17B",
         "#236790","#236790","#236790","#236790",
         "#4D0055","#4D0055","#4D0055")
plotMDS(w, top = 2500, col=col, cex = 1, xlim = c(-4,3), ylim = c(-2,2))
legend("topleft",
       as.character(c("2-cell embryos",
                      "4-cell embryos",
                      "8-cell embryos")),
       col=c("#00C17B","#236790","#4D0055"),
       pch=15, pt.cex = 2)
dev.off()


MDS <- plotMDS(y, top = 500, col=col)
distance_matrix_top500 <- MDS$distance.matrix
distance_matrix_adj <- distance_matrix_top500*0.1
distance_matrix_adj



plotMDS(y, top = 1000, col=col, cex = 1)
legend("topleft",
       as.character(c("2-cell embryos",
                      "4-cell embryos",
                      "8-cell embryos")), col=1:3, pch=20)



## Practice grid (all)
png(file = "Figure_xx.png", width = 2000, height = 2000, units = "px", res = 300)
par(mar = c(4, 4, 2, 2), oma = c(1, 1, 1, 1))
par(cex = 0.9)
par(mfrow = c(3, 4))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
for (i in 1:10) {
  plotMD(y, column = i)
  abline(h=0, col="red", lty=2, lwd=2)
  mtext(letters[i], side = 3, line = -1, adj = 0.5, cex = 1.5)
}
dev.off()

## 2-cell grid
png(file = "2-cell.png", width = 6, height = 18, units = "cm", res = 300)
par(mar = c(2, 2, 1, 1))
par(cex = 0.6)
par(mfrow = c(3, 1))
for (i in 1:3) {
  plotMD(y, column=1)
  abline(h=0, col="red", lty=2, lwd=2)
  plotMD(y, column=2)
  abline(h=0, col="red", lty=2, lwd=2)
  plotMD(y, column=3)
  abline(h=0, col="red", lty=2, lwd=2)
}
dev.off()

## 4-cell grid
png(file = "4-cell.png", width = 6, height = 18, units = "cm", res = 300)
par(mar = c(2, 2, 1, 1))
par(cex = 0.6)
par(mfrow = c(3, 1))
for (i in 1:3) {
  plotMD(y, column=4)
  abline(h=0, col="red", lty=2, lwd=2)
  plotMD(y, column=5)
  abline(h=0, col="red", lty=2, lwd=2)
  plotMD(y, column=6)
  abline(h=0, col="red", lty=2, lwd=2)
}
dev.off()

## 8-cell grid
png(file = "8-cell.png", width = 6, height = 18, units = "cm", res = 300)
par(mar = c(2, 2, 1, 1))
par(cex = 0.6)
par(mfrow = c(3, 1))
for (i in 1:3) {
  plotMD(y, column=8)
  abline(h=0, col="red", lty=2, lwd=2)
  plotMD(y, column=9)
  abline(h=0, col="red", lty=2, lwd=2)
  plotMD(y, column=10)
  abline(h=0, col="red", lty=2, lwd=2)
}
dev.off()

## Plot grid all together
png(file = "Figure_xx.png", width = 1000, height = 4000, units = "px", res = 300)
par(oma=c(0,0,1,0))
par(mar = c(2, 2.5, 2, 1))
par(cex = 0.6)
par(mfrow = c(10, 1))
plotMD(y, column=1)
abline(h=0, col="red", lty=2, lwd=2)
mtext("A", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=2)
abline(h=0, col="red", lty=2, lwd=2)
mtext("B", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=3)
abline(h=0, col="red", lty=2, lwd=2)
mtext("C", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=4)
abline(h=0, col="red", lty=2, lwd=2)
mtext("D", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=5)
abline(h=0, col="red", lty=2, lwd=2)
mtext("E", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=6)
abline(h=0, col="red", lty=2, lwd=2)
mtext("F", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=7)
abline(h=0, col="red", lty=2, lwd=2)
mtext("G", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=8)
abline(h=0, col="red", lty=2, lwd=2)
mtext("H", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=9)
abline(h=0, col="red", lty=2, lwd=2)
mtext("I", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)
plotMD(y, column=10)
abline(h=0, col="red", lty=2, lwd=2)
mtext("J", side = 3, line = -1, adj = -0.1, cex = 1.5, padj = -1.2)

dev.off()


## BCV plot (robust)
y <- estimateDisp(y, design, robust=TRUE)
plotBCV(y)

## PFH topconfects
install.packages("devtools")
devtools::install_github("pfh/topconfects")
library(topconfects)
fit2_confects <- edger_confects(fit = fit2, coef = 2, contrast = NULL, effect = NULL,
               fdr = 0.05, step = 0.01, null = "worst.case")
confects_plot(fit2_confects)

fit3_confects <- edger_confects(fit = fit2, coef = 3, contrast = NULL, effect = NULL,
                                fdr = 0.05, step = 0.01, null = "worst.case")

png(filename = "Figure_9.png", width = 600, height = 750, res = 96, units = "px")
pdf(file = "Figure_9.pdf", width = 6, height = 8, pointsize = 14)
con2 <- confects_plot(fit2_confects) +
  ylab("2-cell to 4-cell stage") + 
  theme(plot.margin = unit(c(1.5,1,0.1,1), "lines"))
con3 <- confects_plot(fit3_confects) +
  ylab("4-cell to 8-cell stage") + 
  theme(plot.margin = unit(c(1.5,1,0.1,1), "lines"))

ggarrange(con2, con3, ncol=2, labels = c("A", "B"),
          common.legend = TRUE, legend = "bottom", 
          font.label = list(size = 20))

dev.off()


efit <- glmQLFit(y, design, robust=TRUE)
etop <- glmQLFTest(efit, coef=3) %>% topTags(n=Inf)
plotMD(efit, legend="bottomright", status=paste0(
  ifelse(rownames(efit) %in% econfects$table$name[1:40], "confects ", ""),
  ifelse(rownames(efit) %in% rownames(etop)[1:40], "topTags ","")))

efit$coefficients
effect <- effect_shift_log2(c(1,2,3), c(8,9,10))

nonlinear <- edger_confects(efit, effect=effect)



## Boxplots and hierarchical clustering of log cpm normalized reads
pseudoCounts <- log2(data_clean+20)
colnames(pseudoCounts) <- c("2-cell_A","2-cell_B","2-cell_D",
                            "4-cell_A","4-cell_B","4-cell_C","4-cell_D",
                            "8-cell_B","8-cell_C","8-cell_D")

pdf(file = "Figure_2.pdf", width = 8, height = 5, pointsize = 12)
par(mfrow=c(1,2))
boxplot(pseudoCounts, las = 2, col = RSC,
        at =c(1,2,3,5,6,7,8,10,11,12),
        ylab = "Library size per sample (log cpm)") +
  mtext(c("A"), cex = 1.75, side=3, at=c(0))
normalized.counts=cpm(pseudoCounts)
transposed=t(normalized.counts)
distance=dist(transposed)
clusters=hclust(distance)
plot(clusters, ylab = "",
     xlab = "", sub = "", frame.plot = TRUE,
     axes = FALSE, main = "") +
  mtext(c("B"), cex = 1.75, side=3, at=c(0.5))
title(ylab = "Hierarchical clustering (log cpm)", line = 1.5)
dev.off()



png(file = "Figure_2.png", width = 20, height = 10, units = "cm", res = 300)
par(mfrow=c(1,2))
boxplot(pseudoCounts, las = 2, col = RSC,
        at =c(1,2,3,5,6,7,8,10,11,12),
        ylab = "Average read count per sample (log cpm)") +
  mtext(c("A"), cex = 1.75, side=3, at=c(0))
normalized.counts=cpm(pseudoCounts)
transposed=t(normalized.counts)
distance=dist(transposed)
clusters=hclust(distance)
plot(clusters, ylab = "",
     xlab = "", sub = "", frame.plot = TRUE,
     axes = FALSE, main = "") +
  mtext(c("B"), cex = 1.75, side=3, at=c(0.5))
title(ylab = "Hierarchical clustering (log cpm)", line = 1.5)
dev.off()


## Correlation matrix
cor_matrix_spearman <- cor(data_clean, method = "spearman", use = "complete.obs")

#plot correlations
corrplot.mixed(cor_matrix_spearman, tl.col = "black",
               order = "hclust", hclust.method = "ward.D",
               tl.srt = 45, upper = "shade", cl.cex = 1,
               lower = "number", cl.lim = c(0,1),
               tl.pos = "lt",diag = "l", mar = c(0,0,0,2))
dev.off()



## QLFtest
qlrt <- glmQLFTest(fit, coef=2:3)
SpM_genes <- qlrt$table[,1:4]
names(SpM_genes) <- c("2-cell","4-cell","8-cell")
count_data <- read.table("counts.txt", header = TRUE)
group <- factor(c(1,1,1,2,2,2,2,3,3,3))

y <- DGEList(counts=data_clean,group=group)
y <- calcNormFactors(y)
design <- model.matrix(~group)
y <- estimateDisp(y, design)
fit <- glmFit(y, design)
lrt <- glmLRT(fit, coef=2:3)

fit <- glmFit(y, design)
lrt.2vs1 <- glmLRT(fit, coef=2)
lrt.3vs1 <- glmLRT(fit, coef=3)
lrt.3vs2 <- glmLRT(fit, contrast=c(0,-1,1))

topTags(qlrt)
lrt_allTags <- topTags(qlrt,n=dim(lrt$table)[1])[[1]]
deClusters=rownames(lrt_allTags)[lrt_allTags$FDR<0.05]
write.table(deClusters,"deClusters_PValue05.txt",quote=F,row.names=F,col.names=F)
write.table(lrt_allTags, "all_tags_clean.txt", quote=F)

row_sub = apply(pseudoCounts, 1, function(row) all(row !=0 ))
fewer_pseudocounts <- pseudoCounts[row_sub,]

FST <- SpM_genes["", ]
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 1, 0, 0))
sum(lrt_allTags$FDR < 0.05)
summary(decideTestsDGE(qlrt))
try <- qlrt$table$logFC.group2

plotSmear(lrt.2vs1,de.tags = rownames(lrt_allTags)[lrt_allTags$FDR < .05])
abline(h = c(-2, 2), col = "blue")

meanVarPlot <- plotMeanVar( y ,
                            show.raw.vars=TRUE ,
                            show.tagwise.vars=TRUE ,
                            show.binned.common.disp.vars=FALSE ,
                            show.ave.raw.vars=FALSE , NBline = TRUE ,
                            nbins = 100 , pch = 16 ,
                            xlab ="Mean Expression (Log10 Scale)" ,
                            ylab = "Variance (Log10 Scale)")

old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)

pca <- prcomp(t(cpm_log), scale. = TRUE)
plot(pca$x[, 1], pca$x[, 2], pch = ".", xlab = "PC1", ylab = "PC2", xlim = c(-600,600), ylim = c(-600,600),  main = "Principle Component Analysis Plot")
text(pca$x[, 1], pca$x[, 2], labels = colnames(cpm_log), col=as.numeric(y$samples$group))

plotMDS.DGEList(y, main = "MDS Plot for Count Data", labels = colnames(y$counts))
plotMDS(y, method="bcv", col=as.numeric(y$samples$group))
m1 <-estimateCommonDisp(y, verbose=T)
m1 <- estimateTagwiseDisp(m1)
exact12 <- exactTest(y, pair = c(1,2))
results_edgeR <- topTags(exact12, n = nrow(data_clean), sort.by = "none")
exact23 <- exactTest(y, pair = c(2,3))
results_edgeR_2vs3 <- topTags(exact23, n = nrow(data_clean), sort.by = "none")

sum(results_edgeR_2vs3$table$FDR < 0.05)
head(results_edgeR$table)
sum(results_edgeR$table$FDR < .05)
plotSmear(qlftest_3vs2, de.tags = rownames(qlftest_3vs2_allTags)[qlftest_3vs2_allTags$FDR < .05])
abline(h = c(-2, 2), col = "blue")
plotSmear(exact12, de.tags = rownames(results_edgeR)[results_edgeR$table$PValue <=0.01])


## Heatmap
sampleDists <- as.matrix(78/dist(t(pseudoCounts)))
sampleDists

cimColor <- viridis_pal(option = "viridis")(32)
RSC <- c("#00C17B","#00C17B","#00C17B",
                "#236790","#236790","#236790","#236790",
                "#a37aa0","#a37aa0","#a37aa0")
CSC <- c("#00C17B","#00C17B","#00C17B",
                "#236790","#236790","#236790","#236790",
                "#a37aa0","#a37aa0","#a37aa0")

cim(sampleDists, color=cimColor, cluster = "both", keysize.label = 1,
    keysize = c(1,1), symkey=FALSE, margins = c(5, 7),
    clust.method = c("complete","complete"), center = TRUE, row.sideColors = RSC,
    col.sideColors = CSC,
    legend = list(legend = c("2-cell","4-cell","8-cell"),
                             col = c("#00C17B","#236790","#a37aa0"),
                             title = "Developmental stage    ",
                  cex = 0.9))


select = order(rowMeans(fewer_pseudocounts), decreasing=TRUE)[1:1000]
highexprgenes_counts <- fewer_pseudocounts[select,]
colnames(highexprgenes_counts) <- c("2-cell_A", "2-cell_B", "2-cell_D", "4-cell_A", "4-cell_B", "4-cell_C", "4-cell_D", "8-cell_B", "8-cell_C", "8-cell_D")
cim(t(highexprgenes_counts), color=cimColor, cluster = "both",
    symkey=FALSE, margins = c(8, 6), legend = NULL, center = TRUE)

heatmap.2(as.matrix(highexprgenes_counts), col=redgreen(11),
          Rowv=FALSE, Colv=FALSE, dendrogram=c("none"))

dev.off()


## Smearplots again
plotSmear(qlf.2vs1, de.tags=rownames(qlrt)[qlf.2vs1$table$PValue < .05],
          pair=c("1","2"), cex = .5 , xlab="Log Concentration", ylab="Log Fold-Change")

boxplot(as.numeric(qlftest$fitted.values["Cluster-100067.0", ]) ~ group,
        ylab = "Read count",
        cex.main = 2, names=c("2-cell","4-cell","8-cell"))




## New barplot / boxplot
x_axis_names <- c("2-cell_A","2-cell_B","2-cell_D",
                  "4-cell_A","4-cell_B","4-cell_C","4-cell_D",
                  "8-cell_B","8-cell_C","8-cell_D")


fit2["Cluster-203927.1", ]$coefficients[1:3]
pseudoCounts2 <- asinh(fit2$fitted.values+1)
pseudoCounts2["Cluster-203927.1", ]
fit2$coefficients


keep <- lrt_allTags$FDR <= 0.05 & lrt_allTags$logCPM > 2
lrt_keep <- lrt_allTags[keep,]


## Plot correlations as a heatmap
col2 <- viridis_pal()(12)
heatmap.2(x = cor_matrix_spearman, col = col2, symm = TRUE,
        RowSideColors = RSC, ColSideColors = CSC,
        margin = c(6,6), trace = c("none"), revC = TRUE)

## Plot Spearman correlations as a matrix
library(GGally)
png(filename="GGally_chart.png", height = 18, width = 18, units = "cm", res = 300)
ggpairs(fewer_pseudocounts) +
  theme_minimal()
dev.off()

## Re-running EdgeR using the 'cds' method
par(mar=c(4.5,4.5,4.5,4.5))
cds <- y[rowSums(1e+06 * y$counts/expandAsMatrix(y$samples$lib.size, dim(y)) > 1) >= 3, ]
dim(cds)
cds <- calcNormFactors(cds)
cds$samples
cds$samples$lib.size * cds$samples$norm.factors
apply(cds$counts, 2, sum)
keep <- rowSums(cpm(cds) > 1) >= 3
cds <- cds[keep, ]
dim(cds)
plotMDS.DGEList(cds, labels = colnames(cds$counts), top = 2000)
cds <- estimateCommonDisp(cds)
names(cds)
cds$common.dispersion
cds <- estimateTagwiseDisp( cds , prior.n = 10 )

## Plotting mean-variance (how well the NB model fits)
meanVarPlot <- plotMeanVar( cds , show.raw.vars=TRUE ,
                            show.tagwise.vars=TRUE ,
                            show.binned.common.disp.vars=FALSE ,
                            show.ave.raw.vars=FALSE ,
                            dispersion.method = "qcml" , NBline = TRUE ,
                            nbins = 100 ,
                            pch = 16 ,
                            xlab ="Mean Expression (Log10 Scale)" ,
                            ylab = "Variance (Log10 Scale)" ,
                            main = "Mean-Variance Plot" )

## Further reducing the number of 'noisy' genes
cpm_log <- cpm(cds, log = TRUE)
median_log2_cpm <- apply(cpm_log, 1, median)
hist(median_log2_cpm)
expr_cutoff <- -1
abline(v = expr_cutoff, col = "red", lwd = 3)
sum(median_log2_cpm > expr_cutoff)
data_clean <- cds[median_log2_cpm > expr_cutoff, ]
pca <- prcomp(t(cpm_log), scale. = TRUE)
plot(pca$x[, 1], pca$x[, 2], pch = ".", xlab = "PC1", ylab = "PC2")
text(pca$x[, 1], pca$x[, 2], labels = colnames(cpm_log))

par(mar=c(6, 5 ,1 ,0))
opt <- options(scipen = 10)
options(opt)
barplot(y$samples$lib.size, names.arg = y$samples$group, las = 2, ylim = c(0, 40000000))

DEgenes_2_vs_1=rownames(qlf.2vs1_allTags)[qlf.2vs1_allTags$FDR<0.05]
DEgenes_3_vs_2=rownames(qlf.3vs2_allTags)[qlf.3vs2_allTags$FDR<0.05]
DEgenes_3_vs_1=rownames(qlf.3vs1_allTags)[qlf.3vs1_allTags$FDR<0.05]

plotBCV(cds)
et12 <- exactTest(cds, pair=c(1,2))
et32 <- exactTest(cds, pair=c(2,3))
et13 <- exactTest(cds, pair=c(1,3))
topTags(et12, n=10)
topTags(et32, n=10)
topTags(et13, n=10)
de1 <- decideTestsDGE(et12, adjust.method="BH", p.value=0.05)
de2 <- decideTestsDGE(et32, adjust.method="BH", p.value=0.05)
de3 <- decideTestsDGE(et13, adjust.method="BH", p.value=0.05)
summary(de1)
summary(de2)
summary(de3)
de1tags12 <- rownames(cds)[as.logical(de1)] 
de1tags13 <- rownames(cds)[as.logical(de3)] 
plotSmear(et12, de.tags=de1tags12)
abline(h = c(-2, 2), col = "blue")
plotSmear(et13, de.tags=de1tags13)
abline(h = c(-2, 2), col = "blue")

boxplot(as.numeric(count_data["Cluster-203927.1", ]) ~ group,
        ylab = "Read count", main = "AP-1 | Mus musculus")



## Plot a Venn diagram to illustrate DE genes common to both stages
total <- venn.diagram(x = list(" " = DEgenes_2_vs_1,
                            " " = DEgenes_3_vs_1),
                   fill = c("#00C17B","#4D0055"),
                   filename = NULL, fontfamily = "sans", rotation.degree = 180,
                   cex=1.7, scaled = FALSE)
#grid.draw(total)
#grid.text(c("2- to 4-cell stage \n     (n=3428)"), x=0.03, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))
#grid.text(c("4- to 8-cell stage \n      (n=1150)"), x=0.55, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))

#upregulated genes
DEgenes_2_vs_1_upreg=rownames(qlf.2vs1_allTags)[qlf.2vs1_allTags$logFC>0 & qlf.2vs1_allTags$FDR<0.05]
DEgenes_3_vs_1_upreg=rownames(qlf.3vs1_allTags)[qlf.3vs1_allTags$logFC>0 & qlf.3vs1_allTags$FDR<0.05]
length(DEgenes_2_vs_1_upreg)
length(DEgenes_3_vs_1_upreg)

upreg <- venn.diagram(x = list(" " = DEgenes_2_vs_1_upreg,
                            " " = DEgenes_3_vs_1_upreg),
                   fill = c("#00C17B","#4D0055"),
                   filename = NULL, fontfamily = "sans", rotation.degree = 180,
                   cex=1.7, scaled = FALSE)
#grid.draw(upreg)
#grid.text(c("2- to 4-cell stage \n     (n=3428)"), x=0.03, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))
#grid.text(c("4- to 8-cell stage \n      (n=1150)"), x=0.55, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))

#downregulated genes
DEgenes_2_vs_1_downreg=rownames(qlf.2vs1_allTags)[qlf.2vs1_allTags$logFC<0 & qlf.2vs1_allTags$FDR<0.05]
DEgenes_3_vs_1_downreg=rownames(qlf.3vs1_allTags)[qlf.3vs1_allTags$logFC<0 & qlf.3vs1_allTags$FDR<0.05]
length(DEgenes_2_vs_1_downreg)
length(DEgenes_3_vs_1_downreg)

downreg <- venn.diagram(x = list(" " = DEgenes_2_vs_1_downreg,
                            " " = DEgenes_3_vs_1_downreg),
                   fill = c("#00C17B","#4D0055"),
                   filename = NULL, fontfamily = "sans", rotation.degree = 180,
                   cex=1.7, scaled = FALSE)
#grid.draw(downreg)
#grid.text(c("2- to 4-cell stage \n     (n=3428)"), x=0.03, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))
#grid.text(c("4- to 8-cell stage \n      (n=1150)"), x=0.55, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))

grid.arrange(gTree(children=upreg), gTree(children=downreg),
             ncol = 2)
grid.text(c("upregulated genes \n"), x=0.1, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))
grid.text(c("downregulated genes \n"), x=0.575, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1.5))
grid.text(c("2-cell to 4-cell       4-cell to 8-cell"), x=0.075, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1))
grid.text(c("2-cell to 4-cell       4-cell to 8-cell"), x=0.575, y=0.85, vjust = 0, hjust = 0, gp = gpar(cex=1))

dev.off()




## Multiple Empirical Cumulative Distribution Functions (Ecdf) And Densities
#source("https://bioconductor.org/biocLite.R")
#biocLite("geneplotter")
par(mfrow = c(1,2))
par(mar=c(4.5,4.5,4,1))
multiecdf(fit2$counts, xlim=c(0,50),
          main = "", xlab = "Number of reads (pre-normalization)",
          col = RSC)
mtext("A", side = 3, cex = 2, adj = -0.15, line = 1)
mtext("B", side = 3, cex = 2, adj = 1.25, line = 1)
multiecdf(fit2$fitted.values, xlim=c(0,50),
          main = "", xlab = "Number of reads (fitted values)",
          col = RSC)

dev.off()


## Multi boxplots illustrating top DE genes
#png(filename = "Figure_11.png", width = 650, height = 800, units = "px", res = 72, pointsize = 14)
pdf(file = "Figure_10.pdf", width = 6.51, height = 6.5)
par(mfrow=c(3,3))
par(mar=c(2.25,2.25,2.25,1.25)+0.1)
par(oma=c(0.5,2.5,0.5,0))
boxplot(as.numeric(qlftest$fitted.values["Cluster-100067.0", ]) ~ group,
        ylab = "Read count",
        main = "Mpzl2", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("A", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-56726.2", ]) ~ group,
        ylab = "Read count",
        main = "Pol", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("B", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-103777.3", ]) ~ group,
        ylab = "Read count",
        main = "Atp10d", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("C", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-109982.0", ]) ~ group,
        ylab = "Read count",
        main = "Ucp2", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("D", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-218088.1", ]) ~ group,
        ylab = "Read count",
        main = "Nedd4", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("E", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-198655.2", ]) ~ group,
        ylab = "Read count",
        main = "Hsp70", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("F", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-59802.0", ]) ~ group,
        ylab = "Read count",
        main = "Lurap1l", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("G", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-45469.0", ]) ~ group,
        ylab = "Read count",
        main = "Bmp4", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("H", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)
boxplot(as.numeric(qlftest$fitted.values["Cluster-4574.0", ]) ~ group,
        ylab = "Read count",
        main = "Akr1b8", cex.axis = 1.1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("I", side = 3, line = -0.1, adj = -0.05, cex = 1.25,
        padj = -0.2)


mtext(c("Normalized read count"), cex = 1, side = 2,
      outer = TRUE, line = 1, adj = c(0.5))
mtext(c("Normalized read count"), cex = 1, side = 2,
      outer = TRUE, line = 1, adj = c(0.04))
mtext(c("Normalized read count"), cex = 1, side = 2,
      outer = TRUE, line = 1, adj = c(0.97))

dev.off()


#Dotplot (test)
group_split <- c(1,1,1,2,2,3,3,4,4,4)
dotplot(qlftest$fitted.values["Cluster-177194.1", ] ~ group_split,
        horizontal = FALSE, pch = "-", cex = 5,
        xlab = c("2C", "4C-early", "4C-late", "8C"),
        main = "Paf1 spiny mouse")

group <- c(1,1,1,2,2,2,2,3,3,3)
boxplot(as.numeric(qlftest$fitted.values["Cluster-177194.1", ]) ~ group,
        ylab = "Read count",
        main = "Paf1", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell"))

group <- c(1,1,1,2,2,2,2,3,3,3)
boxplot(as.numeric(pseudoCounts2["Cluster-177194.1", ]) ~ group,
        ylab = "Read count",
        main = "Paf1", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell"))


## Parallel coordinate plots comparing mice, humans and spiny mice
values_for_parcoord_2 <- c()
values_for_parcoord_2$two_cell <- rep(0, times = 50000)
values_for_parcoord_2$four_cell <- head(qlftest_3vs2$table$logFC, 50000)
values_for_parcoord_2$eight_cell <- head(qlftest_2vs1$table$logFC, 50000)
values_for_parcoord_2$Expression <- as.factor("2-cell to 4-cell stage")
df_pc1_2 <- as.data.frame(values_for_parcoord_2)

values_for_parcoord_2 <- c()
values_for_parcoord_2$two_cell <- rep(0, times = 253448)
values_for_parcoord_2$four_cell <- qlftest_3vs2$table$logFC
values_for_parcoord_2$eight_cell <- qlftest_3vs1$table$logFC
values_for_parcoord_2$Expression <- as.factor("4-cell to 8-cell stage")
df_pc_temp_2 <- as.data.frame(values_for_parcoord_2)
df_pc2_2 <- df_pc_temp_2[with(df_pc_temp_2,
                              (four_cell <= 0.005 & four_cell >= -0.005)), ]

df_pc3_2 <- rbind(df_pc1_2,df_pc2_2)

dim(df_pc1_2)
dim(df_pc2_2)
dim(df_pc3_2)

df_pc_order <- c(1,2,3)
p <- ggparcoord(data = df_pc3_2, columns = 1:3,
                scale = "globalminmax", missing = "median",
                order = df_pc_order, groupColumn = 4,
                alphaLines = 0.4, title = "B     Spiny mouse (Acomys cahirinus)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", plot.title = element_text(size = 10)) +
  scale_color_manual(values = c("4-cell to 8-cell stage" = "#4D0055",
                                "2-cell to 4-cell stage" = "#00C17B")) +
  xlab(" ") + ylab("Log2 fold change") + ylim(c(-15,12.5))

## Mouse
mouse_fpkm <- read.table("mouse_expression_stages_of_interest.txt",
                         header = TRUE)

colnames(mouse_fpkm) <- c("gene","2-cell_A","2-cell_B","2-cell_C",
                          "4-cell_A","4-cell_B","4-cell_C",
                          "8-cell_A","8-cell_B","8-cell_C")
mouse_fpkm$two_cell <- rowMeans(mouse_fpkm[,
                                           c("2-cell_A",
                                             "2-cell_B",
                                             "2-cell_C")],
                                na.rm=TRUE)
mouse_fpkm$four_cell <- rowMeans(mouse_fpkm[,
                                            c("4-cell_A",
                                              "4-cell_B",
                                              "4-cell_C")],
                                 na.rm=TRUE)
mouse_fpkm$eight_cell <- rowMeans(mouse_fpkm[,
                                            c("8-cell_A",
                                              "8-cell_B",
                                              "8-cell_C")],
                                 na.rm=TRUE)

mouse_fpkm_four_cell_FC <- log2(mouse_fpkm$four_cell) - log2(mouse_fpkm$two_cell)
mouse_fpkm_four_cell_FC[!is.finite(mouse_fpkm_four_cell_FC)] <- 0

mouse_fpkm_eight_cell_FC <- log2(mouse_fpkm$eight_cell) - log2(mouse_fpkm$four_cell)
mouse_fpkm_eight_cell_FC[!is.finite(mouse_fpkm_eight_cell_FC)] <- 0

mouse <- c()
mouse$two_cell <- rep(0, times = 13879)
mouse$four_cell <- mouse_fpkm_four_cell_FC
mouse$eight_cell <- mouse_fpkm_eight_cell_FC
mouse$Expression <- as.factor("2-cell to 4-cell stage")
df_mouse_temp <- as.data.frame(mouse)
df_mouse_1 <- df_mouse_temp[with(df_mouse_temp,
                              (eight_cell <= 7 & eight_cell >= -7)), ]

mouse <- c()
mouse$two_cell <- rep(0, times = 13879)
mouse$four_cell <- mouse_fpkm_four_cell_FC
mouse$eight_cell <- mouse_fpkm_eight_cell_FC
mouse$Expression <- as.factor("4-cell to 8-cell stage")
df_mouse_temp <- as.data.frame(mouse)
df_mouse_2 <- df_mouse_temp[with(df_mouse_temp,
                                 (four_cell <= 0.000001 & four_cell >= -0.000001)), ]

df_mouse_3 <- rbind(df_mouse_1,df_mouse_2)
dim(df_mouse_1)
dim(df_mouse_2)
dim(df_mouse_3)


df_mouse_order <- c(1,2,3)
p_mouse <- ggparcoord(data = df_mouse_3, columns = 1:3,
                scale = "globalminmax", missing = "median",
                order = df_mouse_order, groupColumn = 4,
                alphaLines = 0.4, title = "A     C57BL/6 mouse (Mus musculus)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", plot.title = element_text(size = 10)) +
  scale_color_manual(values = c("4-cell to 8-cell stage" = "#4D0055",
                                "2-cell to 4-cell stage" = "#00C17B")) +
  xlab(" ") + ylab("Log2 fold change") + ylim(c(-15,12.5))


## Human
#import the data
human_fpkm <- read.table("human_expression_stages_of_interest.txt",
                         header = TRUE)
colnames(human_fpkm) <- c("gene","2-cell_A","2-cell_B","2-cell_C",
                          "4-cell_A","4-cell_B","4-cell_C", "4-cell_D",
                          "8-cell_A","8-cell_B","8-cell_C",
                          "8-cell_D","8-cell_E","8-cell_F",
                          "8-cell_G","8-cell_H","8-cell_I",
                          "8-cell_J")
human_fpkm$two_cell <- rowMeans(human_fpkm[,
                                           c("2-cell_A",
                                             "2-cell_B",
                                             "2-cell_C")],
                                na.rm=TRUE)
human_fpkm$four_cell <- rowMeans(human_fpkm[,
                                            c("4-cell_A",
                                              "4-cell_B",
                                              "4-cell_C",
                                              "4-cell_D")],
                                 na.rm=TRUE)
human_fpkm$eight_cell <- rowMeans(human_fpkm[,
                                             c("8-cell_A","8-cell_B","8-cell_C",
                                               "8-cell_D","8-cell_E","8-cell_F",
                                               "8-cell_G","8-cell_H","8-cell_I",
                                               "8-cell_J")],
                                  na.rm=TRUE)

#Format the data for ggparcoord
human_fpkm_four_cell_FC <- log2(human_fpkm$four_cell) - log2(human_fpkm$two_cell)
human_fpkm_four_cell_FC[!is.finite(human_fpkm_four_cell_FC)] <- 0

human_fpkm_eight_cell_FC <- log2(human_fpkm$eight_cell) - log2(human_fpkm$four_cell)
human_fpkm_eight_cell_FC[!is.finite(human_fpkm_eight_cell_FC)] <- 0

human <- c()
human$two_cell <- rep(0, times = 14766)
human$four_cell <- human_fpkm_four_cell_FC
human$eight_cell <- human_fpkm_eight_cell_FC
human$Expression <- as.factor("2-cell to 4-cell stage")
df_human_1 <- as.data.frame(human)

human <- c()
human$two_cell <- rep(0, times = 14766)
human$four_cell <- human_fpkm_four_cell_FC
human$eight_cell <- human_fpkm_eight_cell_FC
human$Expression <- as.factor("4-cell to 8-cell stage")
df_human_temp <- as.data.frame(human)
df_human_2 <- df_human_temp[with(df_human_temp,
                                 (four_cell <= 0.001 & four_cell >= -0.001)), ]

df_human_3 <- rbind(df_human_1,df_human_2)
dim(df_human_1)
dim(df_human_2)
dim(df_human_3)

#Plot
df_human_order <- c(1,2,3)
p_human <- ggparcoord(data = df_human_3, columns = 1:3,
           scale = "globalminmax", missing = "median",
           order = df_human_order, groupColumn = 4,
           alphaLines = 0.4, title = "C     Human (Homo sapiens)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none", plot.title = element_text(size = 10)) +
  scale_color_manual(values = c("4-cell to 8-cell stage" = "#4D0055",
                                "2-cell to 4-cell stage" = "#00C17B")) +
  xlab(" ") + ylab("Log2 fold change") + ylim(c(-15,12.5))

pdf(file = "Figure_11_2.pdf", width = 5,
    height = 6.5)
grid.arrange(p_mouse, p, p_human, nrow=3, ncol=1)
dev.off()


#########################################
## Explore expression of specific genes

boxplot(as.numeric(qlftest$fitted.values["Cluster-207427.1", ]) ~ group,
        ylab = "Read count",
        main = "Eif2s1", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("P", side = 3, line = -0.1, adj = -0.2, cex = 1.25,
        padj = -0.75)


View(mouse_fpkm)
View(human_fpkm)

#########################################

## Plot gene expression grid
#top row

pdf(file = "Figure_S9.pdf", width = 6, height = 8)

par(mfrow=c(4,3))
par(mar=c(3,4,3,1))

EIF4E_mouse <- mouse_fpkm[4181,]
EIF4E_human <- human_fpkm[10567,]

group_mouse <- c(1,1,1,2,2,2,3,3,3)
boxplot(as.numeric(log2(EIF4E_mouse[,2:10])) ~ group_mouse,
        ylab = "Normalized read count",
        main = "Eif4e mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("A", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

boxplot(as.numeric(qlftest$fitted.values["Cluster-194351.0", ]) ~ group,
        ylab = "Normalized read count",
        main = "Eif4e spiny mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("B", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

group_human <- c(1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,3)
boxplot(as.numeric(log2(EIF4E_human[,2:18])) ~ group_human,
        ylab = "Normalized read count",
        main = "EIF4E human", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("C", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)


####################################
#second row

ELAVL1_mouse <- mouse_fpkm[5770,]
ELAVL1_human <- human_fpkm[9906,]

group_mouse <- c(1,1,1,2,2,2,3,3,3)
boxplot(as.numeric(ELAVL1_mouse[,2:10]) ~ group_mouse,
        ylab = "Normalized read count",
        main = "Elavl1 mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("D", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

boxplot(as.numeric(qlftest$fitted.values["Cluster-151266.1", ]) ~ group,
        ylab = "Normalized read count",
        main = "Elavl1 spiny mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("E", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

group_human <- c(1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,3)
boxplot(as.numeric(log2(ELAVL1_human[,2:18])) ~ group_human,
        ylab = "Normalized read count",
        main = "ELAVL1 human", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("F", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

#######################################
#third row

POU5F1_mouse <- mouse_fpkm[9331,]
POU5F1_human <- human_fpkm[12165,]

group_mouse <- c(1,1,1,2,2,2,3,3,3)
boxplot(as.numeric(POU5F1_mouse[,2:10]) ~ group_mouse,
        ylab = "Normalized read count",
        main = "Pou5f1 mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("G", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

boxplot(as.numeric(qlftest$fitted.values["Cluster-118574.4", ]) ~ group,
        ylab = "Normalized read count",
        main = "Pou5f1 spiny mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("H", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

group_human <- c(1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,3)
boxplot(as.numeric(POU5F1_human[,2:18]) ~ group_human,
        ylab = "Normalized read count",
        main = "POU5F1 human", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("I", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

#######################################
#Fourth row

EIF1A_mouse <- mouse_fpkm[4143,]
EIF1A_human <- human_fpkm[2270,]

group_mouse <- c(1,1,1,2,2,2,3,3,3)
boxplot(as.numeric(EIF1A_mouse[,2:10]) ~ group_mouse,
        ylab = "Normalized read count",
        main = "Eif1a mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("J", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

boxplot(as.numeric(qlftest$fitted.values["Cluster-168128.0", ]) ~ group,
        ylab = "Normalized read count",
        main = "Eif1a spiny mouse", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("K", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)

group_human <- c(1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3,3)
boxplot(as.numeric(EIF1A_human[,2:18]) ~ group_human,
        ylab = "Normalized read count",
        main = "EIF1A human", cex.axis = 1, col = c("#00C17B","#236790", "#a37aa0"),
        cex.main = 1.25, names=c("2-cell","4-cell","8-cell")) +
  mtext("L", side = 3, line = -0.1, adj = -0.4, cex = 1.25,
        padj = -0.75)



dev.off()
