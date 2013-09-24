compact.ordinal <-
function(CONOP){
	CONOP@Plcdfile->plcdfile
	CONOP@Composfile->composfile
	unique(plcdfile[,3:ncol(plcdfile)])->lev
	
	id<-function(X){
		a<-vector(length=nrow(plcdfile))
		for(i in 1:nrow(plcdfile)){
			identical(plcdfile[i,3:ncol(plcdfile)],lev[X,])->a[i]
			}
		plcdfile[a,1:2]->b
		return(b)
		}
	lapply(1:nrow(lev),FUN=id)->ord
	rev(ord)->ord
	seq(1000,2000,by=1000/(length(ord)-1))->ord_depth
	composfile->composfile_ord

	n<-c();m<-1
	for(i in 1:length(ord)){
		length(ord[[i]])/2 -> n[i]
		composfile_ord[m:(m+n[i]-1),4]<-ord_depth[i]
		composfile_ord[m:(m+n[i]-1),5]<-i
		m+n[i]->m
		}
	CONOP@Composfile<-composfile_ord
	return(CONOP)
	}
