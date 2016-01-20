Section.vs.Composite<-function(CONOP,section,col,cex=1, axes=TRUE){
	composfile <- CONOP@Composfile
	plcdfile <- CONOP@Plcdfile
	Nsect <- CONOP@Sectfile[grep(section,CONOP@Sectfile[,4]),3]
	equiv<-matrix(nrow=nrow(composfile),ncol=ncol(plcdfile)-1)
	for(i in 1:nrow(composfile)){
		c(composfile[i,4],plcdfile[plcdfile[,1]==composfile[i,1] & plcdfile[,2]==composfile[i,2],3:ncol(plcdfile)])->equiv[i,]
			}
	plot(0,0,xlim=c(min(composfile[,4]),max(composfile[,4])),ylim=c(0,1),axes=FALSE,xaxs="i",ann=FALSE,yaxs="i")
	rect(min(equiv[equiv[,1+Nsect]==max(equiv[,1+Nsect]),1]),0,max(equiv[equiv[,1+Nsect]==min(equiv[,1+Nsect]),1]),1,col=col,border=NA)
	rect(max(composfile[,4]),0,min(composfile[,4]),1, lwd=3)
	invisible(sapply(unique(composfile[,4]),function(x){segments(x,0,x,1,lwd=1)}))
	if(axes){
		axis(1,at=pretty(composfile[,4]), cex.axis=cex)
		axis(3,at=c(min(equiv[equiv[,1+Nsect]==max(equiv[,1+Nsect]),1]),max(equiv[equiv[,1+Nsect]==min(equiv[,1+Nsect]),1])), labels=c(max(equiv[,1+Nsect]),min(equiv[,1+Nsect])), cex.axis=cex)
		}
	mtext(CONOP@Sectfile[Nsect,4], side=2, line=1, cex=cex)
}