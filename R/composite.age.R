composite.age<-function(CONOP, agemodels, tie.points, age.span, pch, bg)
{
		#agemodels is a list of published age models for each site: the numbering must follow that of sectfile
		#tie.points is the result of function LOC.tiepoints
		#pch & bg: graphic parameters, 1 shape and one color per site.
		#age.span: age constraint for the plot
		CONOP@Composfile->composfile
		
		if(!exists("age.span",inherits=FALSE)){
			age.span<-c()			
			for(i in 1:length(agemodels)){if(length(agemodels[[i]])!=0){range(c(age.span,agemodels[[i]][,2]),na.rm=T)->age.span}}
			}
		max(age.span)->age.old
		min(age.span)->age.young
		if(!exists("pch",inherits=FALSE)){pch<-rep(21,length(agemodels))}
		if(!exists("bg",inherits=FALSE)){bg<-rainbow(length(agemodels))}
		
		layout(matrix(c(1,2),nrow=2,byrow=T),width=20,height=c(3,20))
		par(mar=c(0,3,3,3))
		plot(0,0,type="n",xlim=range(composfile[,4], composfile[,4], na.rm=TRUE), ylim=c(0,10),
			 xaxt="n",yaxt="n",yaxs="i",xaxs="i",xlab="",ylab="",bty="n")
		for(i in 1:length(unique(composfile[,4]))){lines(c(unique(composfile[,4])[i],unique(composfile[,4])[i]),c(0,10),lty=1,col="black")}
		axis(3,at=seq(min(composfile[,4]),max(composfile[,4]),by=100))
		box()
		par(mar=c(3,3,0,3))
		plot(0,0,xaxs="i",yaxs="i",yaxt="n",xlab="",ylab="",xaxt="n",bty="o",ylim=c(age.old,age.young),xlim=c(min(composfile[,4]),max(composfile[,4])),type="n")
			
		for(SITE in 1:length(agemodels)){
			if(length(agemodels[[SITE]])!=0){
				cbind(approx(tie.points[[SITE]]$y,tie.points[[SITE]]$x,xout=agemodels[[SITE]][,2],rule=1)$y,agemodels[[SITE]][,1])->am
				points(am,pch=pch[SITE],bg=bg[SITE],cex=3)
				}
			}

		title(sub="Age models vs Composite Section",line=2,cex.sub=2)
		axis(2,at=seq(age.old,age.young,by=-1),las=2)
		box()
		
		locator(type="o",pch=3,lty=2)->age.model
		dev.copy2pdf(file="Age model.pdf",width=10,height=10)
		dev.off()
		
		unique(composfile[,4])->depth
		approx(age.model$x,age.model$y,depth,rule=2)->c.age
		cbind(c.age$x,c.age$y)->c.age
		compage<-composfile
		for(i in 1:nrow(compage)){compage[i,4]<-c.age[c.age[,1]==composfile[i,4],2]}
		CONOP@Composfile<-compage
		return(CONOP)
		}