#test variables in the sheet for significance in explaining variance for the test variable.
#build a multinomial regression and for prediction histology and estimate 
#how well does it predict data by dividing the data into training and test sets.



#rm(list=ls(all=TRUE))
#
library (XLConnect)
cat_colors <- function(C){
	R <- C;
	for (i in 1:nrow(C)) {
		R[i] <- switch(C[i],"GI"="orange","OVARIAN"="palevioletred","MELANOMA"="brown","BREAST"="yellow","COLON"="violet","LUNG"="lightblue","RENAL"="green","RECTAL"="black","MISC"="red","UNKNOWN"="grey");
	}
	R
}
#draw a heatmap of clustered data.
do_heatmap <- function(data,x)
{
#now create the heat map with significant variables only.
C=data[,x]
CAT=data[,'origin']
CCOLORS=cat_colors(as.matrix(CAT))
C[,'origin']=NULL
#C[is.na(C)] <- 0

dst<-dist(C,method="euclidean")
fit<-hclust(dst,method="ward.D")
plot(fit,labels = FALSE)

library(gplots)
matr <- as.matrix(C)
par(lend=1)
par(xpd=TRUE)

heatmap.2(matr,Rowv=as.dendrogram(fit),scale='none',trace="none",margins=c(12,9),RowSideColors=CCOLORS,na.color="orange",cexCol=1)
legend("bottomleft",inset=-.12,lty=c(2,1),legend=c("GI","OVARIAN","MELANOMA","BREAST","COLON","LUNG","RENAL","RECTAL"),
col=c("orange","palevioletred","brown","yellow","violet","lightblue","green","black"),
lwd=10,cex=0.5)
}

#initialisation.

thresh<-.7
conf<-.1
print(thresh)
print(conf)


setwd("D:/CONDR_METS")
wk=loadWorkbook("norm_feat.xls")
A=readWorksheet(wk,sheet="all_noperc")

wk1=loadWorkbook("radiomic_features_2.xlsx")
AA=readWorksheet(wk1,sheet="exclude")

#fix the dimension names.
A[,1] -> rows
A[,1] <- NULL
rownames(A) <- rows



A=A[AA$prior.gk==0,]
AA=AA[AA$prior.gk==0,]
A=A[AA$cavity==0,]
AA=AA[AA$cavity==0,]


#delete excluded cols

#delete some origin categories.
M=A[A$origin=="MISC",]
A=A[A$origin!="MISC",]
U=A[A$origin=="UNKNOWN",]
A=A[A$origin!="UNKNOWN",]
#A=A[A$origin!="SQCELL", ]


#attach frame.
nm <- names(A)
library("data.table")
B<-as.data.table(A);
bcols <- colnames(A)
attach(B,warn.conflicts=FALSE)

########################Determine significant variables.
x_v=c()
xi=1
for (i in 1:(ncol(B)-1))
{
	f <-as.formula(paste(nm[i],"~origin"))
	d=anova(lm(f));
#	d=summary(aov(f));
	if (d[1,5]<=conf)
	{
#		print(bcols[i]);
#		print(d);
		x_v[xi]<-i #index of significant variable.
		xi<-xi+1
	}
}
dim(x_v)<-xi-1

#show heatmap.
#do_heatmap(A,x_v)
C=A[,x_v]
C[,ncol(C)+1]<-A[,ncol(A)]
names(C)[ncol(C)]<-paste("origin")
#####################Multinomial regression.
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
cutR=round(thresh*nrow(C))
Ctrain=C[1:cutR,]
Ctest=C[cutR+1:nrow(C),]
ccols=colnames(Ctrain);
attach(Ctrain,warn.conflicts=FALSE)

f="origin~"
for (i in 1:dim(x_v))
{
	f<-paste(f,"+",paste(ccols[i]))
}


m<-multinom(as.formula(f))
z<-summary(m)$coefficients/summary(m)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2

#test prediction by model.
r=as.matrix(predict(m,Ctest,type="class"))
nmiss=0
ngood=0

fail=c(0,0,0,0,0,0,0,0)
names(fail)<-c("GI","OVARIAN","MELANOMA","BREAST","COLON","LUNG","RENAL","RECTAL")
#names(fail)<-c("GI","OVARIAN","SQCELL","BREAST","COLON","RENAL","RECTAL")
#names(fail)<-c("GI","OVARIAN","BREAST","COLON","RENAL","RECTAL")



for (i in 1:nrow(Ctest))
{
	if (is.na(r[i])){ next; }
	if (r[i]!=B[i+cutR,origin])
	{
#		print("mismatch")
		print(r[i])
		print(B[i+cutR,origin])
		fail[B[i+cutR,origin]]<-fail[B[i+cutR,origin]]+1
		nmiss<-nmiss+1
	}
	else
	{
		ngood<-ngood+1
	}
}
rat=print((1-nmiss/nrow(C))*100)
nf<-names(fail)
for (i in 1:length(nf))
{
	if (fail[i]>0)
	{
		fail[i]<-100*(fail[i]/nrow(Ctest[Ctest$origin==nf[i],]))
	}
}

#predict unknowns.
u=as.matrix(predict(m,U,type="class"))
m=as.matrix(predict(m,M,type="class"))

