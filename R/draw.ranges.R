draw.ranges <-
function(CONOP, order="FAD", method="depth",...){
	if(order=="FAD"){a<-1}
	if(order=="LAD"){a<-2}
	if(method=="depth"){b<-4; d<-50; MetLab<-"Composite section depth"}
	if(method=="rank"){b<-5; d<-2; MetLab<-"Rank number"}
	if(method=="age"){b<-4; d<- -1; MetLab<-"Age\n(Ma)"}
	
	CONOP@Composfile->composfile
	CONOP@Eventfile->eventfile
	
	par(mar=c(1,5,5,1))
	if(method!="age"){min(composfile[,b])->y1; max(composfile[,b])->y2}
	if(method=="age"){max(composfile[,b])->y1; min(composfile[,b])->y2}
	plot(0, 0, ylim=c(y1,y2), xlim=c(0,length(unique(composfile[composfile[,2]!=5, 1]))+1), 
		type="n", xaxt="n", xlab="", ylab="", yaxt="n", xaxs="i", yaxs="i")
	axis(2,at=seq(floor(y1),floor(y2),by=d),las=2,cex.lab=10)
	mtext(MetLab,side=2,line=3)
	composfile[composfile[,2]==a,1]->evtorder
	cbind(evtorder,1:length(evtorder))->evtorder
	for(X in unique(composfile[composfile[,2]!=5,1])){
		composfile[composfile[,1]==X,]->Xrange
		if(length(Xrange)!=ncol(composfile)){
			segments(evtorder[evtorder[,1]==X,2],Xrange[Xrange[,2]%in%c(-1,1),b],evtorder[evtorder[,1]==X,2],Xrange[Xrange[,2]==2,b],...)
			mtext(eventfile[eventfile[,1]==X,3],font=3,las=2,at=evtorder[evtorder[,1]==X,2],line=0.5,...)
		}
		}
	}
