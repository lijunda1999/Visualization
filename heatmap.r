library(pheatmap)

data <- read.csv("CAZy.csv", header=T, row.names=1)
otu <- read.delim("cazy.txt", header=T, row.names=1)
otu=as.data.frame(t(t(otu)/colSums(otu)*100))

#row grouping
anno_row <- read.delim("sig.txt", header=T,row.names=1)
#col grouping
anno_col <- read.table("group.txt", header=T,row.names=1)


#significant names
selected_rows <- rownames(anno_row)
# filter rows by row name
mat <- data[row.names(data) %in% selected_rows, ]


pheatmap(mat,
         cluster_cols = TRUE,
         scale = "row",
         cutree_rows=2,cutree_cols = 2,
         annotation_row = anno_row,
         annotation_col = anno_col,
         annotation_names_row= T,annotation_names_col=T,
         show_rownames=T,show_colnames=T,
         fontsize=6,display_numbers=F,
         cellwidth = 30)
