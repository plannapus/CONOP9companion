culling <-
function(CONOP,cpcht,FO.col="blue",LO.col="red"){
	CONOP@Composfile->composfile
	max(cpcht,na.rm=T)->maxb
	min(cpcht,na.rm=T)->minb
	par(mar=c(4,3,2,2))
	plot(0,0, 
		type="n", xaxt="n", xlab="", ylab="", yaxt="n",xlim=c(-250,250), ylim=range(composfile[,4], na.rm=TRUE), 
		asp=1, yaxs="i", xaxs="i")
	mtext("Composite section",side=1,line=1.5)
	axis(2,las=2)
	apply(cpcht, 1, function(x){
		t <- seq(0, 2 * pi, by = 0.01)
		y0 <- (x[1] + x[2])/2
		r <- y0 - x[1]
		xt <- r * cos(t)
		yt <- r * sin(t) + y0
		lines(xt, yt, col=FO.col)
		if(!is.na(x[3])){
			y0 <- (x[3] + x[4])/2
			r <- y0 - x[3]
			xt <- r * cos(t)
			yt <- r * sin(t) +y0
			lines(xt, yt, col=LO.col)
		}})
	
	}
