#time to death
time_to_event1 <- function (DOT,FiF,FiFd,dlMRI,dd)
{
	TTE=c(rep(NA,length(FiF)))
	for (i in 1:length(FiF))
	{
		if(is.na(DOT[i])) 
		{
			TTE[i]=NA; next;
		}
		if(is.na(dd[i]))
				TTE[i]=as.numeric(as.Date(Sys.Date())-as.Date(DOT[i]))
		else
				TTE[i]=as.numeric(as.Date(dd[i])-as.Date(DOT[i]))
	}
	TTE
}


#time to progression (death is not treated as progression)
time_to_event <- function (DOT,FiF,FiFd,dlMRI,dd)
{
	TTE=c(rep(NA,length(FiF)))
	for (i in 1:length(FiF))
	{
		if(is.na(DOT[i])) next;
		if(FiF[i]==0)
		{
			if(is.na(dlMRI[i])) next;
			TTE[i]=as.numeric(as.Date(dlMRI[i])-as.Date(DOT[i]));
		}
		else
		{
			fd=FiFd[i]
			if (is.na(fd))
			{
				fd=dd[i]
				if(is.na(fd)) next;
			}
			TTE[i]=as.numeric(as.Date(fd)-as.Date(DOT[i]));
		}
	}
	TTE
}

#initialisation.

thresh<-.7
conf<-.05
print(thresh)
print(conf)

setwd("C:/Users/mmilch01/Desktop/CONDR_METS")
#wk=loadWorkbook("norm_feat.xls")
wk=loadWorkbook("radiomic_features_2_no_exclusions.xlsx")
#A=readWorksheet(wk,sheet="all_noperc")
A=readWorksheet(wk,sheet="Sheet1")
#wk1=loadWorkbook("radiomic_features_2.xlsx")
#AA=readWorksheet(wk1,sheet="Sheet1")

#fix the dimension names.
A[,4] -> rows
#A[,1] <- NULL
rownames(A) <- rows
#colnames(A) <- A[1,]

#A=A[AA$"prior gk"==0,]
#AA=AA[AA$"prior gk"==0,]
#A=A[AA$cavity==0,]
#AA=AA[AA$cavity==0,]
#add variable of interest.
#A[,ncol(A)+1]<-as.numeric(AA[,"Failed in Field"])
#attach frame.

delc=c(-1,-2,-3,-4,-195,-196,-197,-198,-199,-201,-202,-203,-204)
keepc=-delc
#delc=c(-1,-2,-3,-4)

C=A[keepc]
A=A[delc]

cn <- colnames(A)
library("data.table")
B<-as.data.table(A);
bcols <- colnames(A)
attach(B,warn.conflicts=FALSE)
attach(C,warn.conflicts=FALSE)

#event variable.
var=Failed.in.Field
#var=is.na(date.of.death)
#D=rep(1,length(var))
#D[var]=0
#var=D


########################Determine significant variables.
x_v=c()
xi=1
cnb=colnames(B)
for (i in 1:190)
{
	f <-as.formula(paste(cnb[i],"~var"))
	print(f)
	d=anova(lm(f));
#	d=summary(aov(f));
	if (d[1,5]<=conf)
	{
		print(d)
#		print(bcols[i]);
#		print(d);
		x_v[xi]<-i #index of significant variable.
		xi<-xi+1
	}
}
dim(x_v)<-xi-1
cn[x_v]

###############determine time to event.

TTE<-time_to_event(DOT,Failed.in.Field,Failed.in.Field.Date,Date.of.last.MRI,date.of.death)
#TTE<-time_to_event1(DOT,Failed.in.Field,Failed.in.Field.Date,Date.of.last.MRI,date.of.death)

library("survival")
par(mfrow=c(2,3))
#plot overall survival function.
sobj=Surv(TTE,var)
plot(survfit(sobj~1),main="Combined mortality rate",xlab="days",cex=.5)
#plot of survival function by histology.
H=c("RENAL","OVARIAN","LUNG","BREAST","MELANOMA")
for (i in 1:length(H)){
	sobj=Surv(TTE[origin==H[i]],var[origin==H[i]])
	plot(survfit(sobj~1),main=paste("Mortality rate: ",H[i]),xlab="days",cex=.5)
}

source("plot_km.R")
par(mfrow=c(3,3))
for (i in 10:15)
{
	plot_km(TTE,var,A[,x_v[i]],bcols[x_v[i]])
}

#clean up the x_v
xv1=x_v[c(1)]

#test if Cox proportional hazards will work
sobj=Surv(TTE,var)
l=length(xv1)
f="sobj~1"
for (i in 1:l)
	f=paste(f,paste( "+as.factor(", cn[xv1[i]],")"))

f=paste(f,"+cluster(study)")
print(f)
cph=coxph( formula(f),control=coxph.control(iter.max=100) )
zcph=cox.zph(cph,transform='log')
summary(cph)

plot(zcph,ylim=c(-20,20))
