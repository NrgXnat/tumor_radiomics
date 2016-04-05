rm(list=ls(all=TRUE))
cat_colors <- function(C){
	R <- C;
	for (i in 1:nrow(C)) {
		R[i] <- switch(C[i],"GI"="orange","OVARIAN"="palevioletred","MELANOMA"="brown","BREAST"="yellow","COLON"="violet","LUNG"="lightblue","RENAL"="green","RECTAL"="black","MISC"="red","UNKNOWN"="grey");
	}
	R
}
library (XLConnect)
setwd("C:/Users/mmilch01/Desktop/CONDR_METS")
wk=loadWorkbook("norm_feat.xls")
A=readWorksheet(wk,sheet="texture3")

#fix the dimension names.
A[,1] -> rows
A[,1] <- NULL
rownames(A) <- rows

#save categorical variable in a separate array.
CAT <- A[,'origin']
A[,'origin'] <- NULL
CCOLORS <- cat_colors(as.matrix(CAT))

#estimate best cluster model
library(mclust)
fit <- Mclust(A)
#plot(fit)

#kmeans clustering
#library(cluster)
#clusplot(A,fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

fit <- kmeans(A,3)
library(fpc)
plotcluster(A,fit$cluster)

#hierarchical clustering
#for plotting purposes, rename A dimensions.

#Ward hierarchy clustering
B=A
B[is.na(B)] <- 0
d <- dist(B,method="euclidean")
fit <- hclust(d,method="ward.D") #dendrogram
#plot(fit, labels = FALSE)
#groups <- cutree(fit,k=7) #cut tree into N clusters
#rect.hclust(fit,k=7,border="red") #draw dendrogram with red borders around clusters.

#heatmaps.
library(gplots)
matr <- as.matrix(B)
par(lend=1)
par(xpd=TRUE)
heatmap.2(matr,Rowv=as.dendrogram(fit),scale='none',trace="none",margins=c(12,9),RowSideColors=CCOLORS)
#heatmap.2(matr,scale='none',trace="none",margins=c(12,9))#,RowSideColors=CCOLORS,na.color="black")
#xy.coords(-6,-4.5)
legend("bottomleft",inset=-.12,lty=c(2,1),legend=c("GI","OVARIAN","MELANOMA","BREAST","COLON","LUNG","RENAL","RECTAL","MISC","UNKNOWN"),
col=c("orange","palevioletred","brown","yellow","violet","lightblue","green","black","red","grey"),
lwd=10,cex=0.5)

