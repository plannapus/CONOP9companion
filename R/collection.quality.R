collection.quality<-function(CONOP, section){
	CONOP@Obsdfile->obsdfile
	CONOP@Plcdfile->plcdfile
	CONOP@Eventfile->eventfile
	if(nchar(site)<=3){CONOP@Sectfile[CONOP@Sectfile[,2]==section,1]->SITE}else{CONOP@Sectfile[CONOP@Sectfile[,4]==section,1]->SITE}
	layout(matrix(1:3,nrow=1),width=c(10,1,10))
	par(mar=c(3,3,3,0))
	x <- sapply(unique(plcdfile[,SITE+2]), function(x){sum(plcdfile[,SITE+2]==x)})
	y <- sapply(unique(obsdfile[!is.na(obsdfile[,SITE+2]),SITE+2]), function(x){sum(obsdfile[!is.na(obsdfile[,SITE+2]),SITE+2]==x)})
	plot(0,0, xaxs="i",yaxt="n", ylab="",bty="n",type="n", xlim=c(max(x), 0), ylim=range(unique(plcdfile[,SITE+2]), na.rm=T), xlab="")
	segments(x,unique(plcdfile[,SITE+2]), 0, unique(plcdfile[,SITE+2]), col="red")
	segments(y, unique(obsdfile[!is.na(obsdfile[,SITE+2]),SITE+2]), 0, unique(obsdfile[!is.na(obsdfile[,SITE+2]),SITE+2]), lwd=3)
	mtext("Number of events at given level",side=3, line=1)

	par(mar=c(3,0,3,0))
	plot(0,0,type="n",axes=F, xlim=c(0,1), ylim=range(unique(obsdfile[,SITE+2]), na.rm=TRUE))
	box()
	segments( x0=0, x1=1, y0=unique(obsdfile[,SITE+2]), y1=unique(obsdfile[,SITE+2]))

	par(mar=c(3,0,3,3))
	pl <- plcdfile[plcdfile[,2]%in%c(1,2),]
	ol <- obsdfile[obsdfile[,2]%in%c(1,2) & !is.na(obsdfile[,SITE+2]),]
	ps<-rep(0,length=length(unique(ol[,SITE+2])))
	os<-rep(0,length=length(unique(ol[,SITE+2])))
	for(i in eventfile[eventfile[,1]%in%pl[,1],1]){
		r <- pl[pl[,1]==i,SITE+2]
		s <- ol[ol[,1]==i,SITE+2]
		ps[unique(ol[,SITE+2])>=r[2] & unique(ol[,SITE+2])<=r[1]]<-ps[unique(ol[,SITE+2])>=r[2] & unique(ol[,SITE+2])<=r[1]]+1
		if(!is.na(s[1])){os[unique(ol[,SITE+2])>=s[2] & unique(ol[,SITE+2])<=s[1]]<-os[unique(ol[,SITE+2])>=s[2] & unique(ol[,SITE+2])<=s[1]]+1}
		}

	plot(0,0, xaxs="i",yaxt="n", ylab="",bty="n",type="n", xlim=c(0,max(ps)), ylim=range(unique(ol[,SITE+2])), xlab="")
	segments(0,unique(ol[,SITE+2]), ps, unique(ol[,SITE+2]), col="red")
	segments(0,unique(ol[,SITE+2]), os, unique(ol[,SITE+2]), lwd=3)
	mtext("Number of ranges crossing the level",side=3, line=1)
	}