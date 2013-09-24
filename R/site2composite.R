site2composite <-
function(CONOP, mbsf, section){
	composfile <- CONOP@Composfile
	plcdfile <- CONOP@Plcdfile
	Nsect <- CONOP@Sectfile[grep(section,CONOP@Sectfile[,2]),1]
	equiv<-matrix(nrow=nrow(composfile),ncol=ncol(plcdfile)-1)
	for(i in 1:nrow(composfile)){
		c(composfile[i,4],plcdfile[plcdfile[,1]==composfile[i,1] & plcdfile[,2]==composfile[i,2],3:ncol(plcdfile)])->equiv[i,]
		}
	approx(equiv[,Nsect+1],equiv[,1],(-1)*mbsf,rule=2,ties="ordered")$y->result
	return(result)
	}
