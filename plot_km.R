#plot the Kaplan-Meier survival curve for two levels of variable F.

plot_km <- function(TTE,var,f,varname)
{
	md=median(f,na.rm=TRUE)
	subs=(f<=md)
	sf=survfit(Surv(TTE,var)~subs)
	par(ps=12,cex.main=1)
	plot(sf,lty=2:3,ylim=c(0.4,1))
	legend("bottomleft", c(paste(varname,">",round(md,1)),paste(varname,"<",round(md,1))), lty=2:3,inset=0.05,cex=0.6)
	title(paste(varname),cex=0.5)
}

