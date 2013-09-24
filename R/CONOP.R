setClass("CONOP", representation(Summary="vector",Config="list", Sectfile="data.frame", Eventfile="data.frame", Obsdfile="matrix", Plcdfile="matrix", Composfile="matrix", Extnfile= "matrix", Events= "matrix", Sections="list", Curvfile="matrix"))
setMethod(f= "print",signature= "CONOP", definition= function(x){
	cat(as.character(x@Config$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['PROJECT',1]))
	cat("\n")
	cat(paste("Number of Sections:",as.character(x@Config$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['SECTIONS',1]),sep="\t"))
	cat("\n")
	cat(paste("Number of Taxa:",as.character(x@Config$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['TAXA',1]),sep="\t"))
	cat("\n")
	cat(paste("Number of Events:", as.integer(as.character(x@Config$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['TAXA',1]))*2 + as.integer(as.character(x@Config$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['EVENTS',1])), sep="\t"))
	cat("\n")
	for(i in 1:3){cat(paste(names(x@Summary)[i],x@Summary[i], sep=":\t"));cat("\n")}
	})