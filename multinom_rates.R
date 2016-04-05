rm(list=ls(all=TRUE))
line_plot <- function(data, xaxis, xlb, ylb, titl, leg_data, leg_title)
{
	nthr=length(xaxis)
	yrange <- range(data)
	xrange <- range(xaxis)

	#create plot
	nline=ncol(data)
	pts=length(xaxis)

	plot(xrange,yrange,type="n",xlab=xlb,ylab=ylb)
	colors <- rainbow(nline)
	linetype <- c(1:nline)
	plotchar <- seq(18,18+nline,1)

	for(i in 1:nline)
	{
		lines(xaxis, data[,i],type="b",lwd=1.5,lty=linetype[i],col=colors[i],pch=plotchar[i])
	}
	title(titl)
#	legend(xrange[1],yrange[2],leg_data,cex=0.8,col=colors,pch=plotchar,lty=linetype,title=leg_title)
	legend("topright",leg_data,cex=0.8,col=colors,pch=plotchar,lty=linetype,title=leg_title,inset=0.05)

}

success_ratios=matrix(seq(1:20),5);

#confidvars=c(8,16,20,43)
thr=c(.3,.4,.5,.6,.7)
confid=c(.01,.05,.1,.2)

#confid=c(.03,.06,.12,.24)
confidvars=c(3,16,20,40)


nconfid=length(confid)
nthr=length(thr)

htypes=c("GI","OVARIAN","MELANOMA","BREAST","COLON","LUNG","RENAL","RECTAL")
#htypes=c("GI","OVARIAN","SQCELL","BREAST","COLON","RENAL","RECTAL")
#htypes=c("GI","OVARIAN","BREAST","COLON","RENAL","RECTAL")


fail_m=matrix(rep(0,length(htypes)*nthr),nthr);


colnames(fail_m)<-htypes

nvar=thr

#for (i_ind in 1:nconfid)
for (i_ind in 4:4)
{
	assign("conf",confid[i_ind],envir=.GlobalEnv)	
	for (j_ind in 1:nthr)
#	for (j_ind in 1:)
	{
		assign("thresh",thr[j_ind],envir=.GlobalEnv)
		source("anova.R")
		success_ratios[j_ind,i_ind]<-rat
		nvar[i_ind]=dim(x_v)
		fail_m[j_ind,]<-fail
		
	}
	confidvars[i_ind]=dim(x_v)
}
success_ratios
nvar
fail_m

#line_plot(success_ratios,thr,"Training set size/sample size","Prediction accuracy, %",
#	"MLR prediction accuracy of metastatic tumor histology", confidvars, "reg. variables")

line_plot(fail_m,thr,"Training set size/sample size","Fail rate, %",paste("MLR fail rates by histology type, ",confidvars[i_ind]," variables"),htypes, "histology")
