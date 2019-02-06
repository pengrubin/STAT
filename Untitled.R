library(gplots)
library(data.table)

df <- read.csv("/Users/hongwei/Desktop/maphot.csv",row.names = 1, fileEncoding = "UTF-8", header = T, stringsAsFactor = F)
x <- as.matrix(df)
#x[is.na(x)] <- 100
#sum(is.infinite(x))# check
dev.off()
heatmap(-x,dendrogram='none')
dev.off()
heatmap.2(-x,dendrogram='none', colv=FALSE , Rowv=FALSE,trace='none')



my_palette <- colorRampPalette(c("blue", "grey", "green"))(n = 100)
