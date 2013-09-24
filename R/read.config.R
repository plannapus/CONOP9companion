read.config <-
function(file = file.choose()){
	options(warn=-1)
	readLines(file)->x
	options(warn=0)
	x[grep("[a-zA-Z/]",x)]->x
	if(length(grep("^ *_",x))>0){x[-grep("^ *_",x)]->x}
	grep("/",x)->y
	gsub(" *$", "", x)->x
	x[(grep("&getinn",x)+1):(y[1]-1)]->getinn
	x[(grep("&getans",x)+1):(y[2]-1)]->getans
	x[(grep("&getrun",x)+1):(y[3]-1)]->getrun
	x[(grep("&getout",x)+1):(y[4]-1)]->getout
	f<-function(l){
		do.call(rbind,strsplit(l,split="="))->l
		l<-gsub("^\\ +||| +$","",l)
		rn<-l[,1]
		data.frame(Value=gsub("\'","",l,fixed=T)[,2])->l
		row.names(l)<-rn
		l
		}
	list("PARAMETERS THAT IDENTIFY THE INPUT DATA"=f(getinn), "PARAMETERS THAT ALTER THE BEST SOLUTION"=f(getans), "PARAMETERS THAT INFLUENCE EFFICIENCY OF SEARCH FOR BEST SOLUTION"=f(getrun), "PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT"=f(getout))
	}