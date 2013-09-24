setClass("CONOPinput", representation(Config="list", Sectfile="data.frame", Eventfile="data.frame", Loadfile= "data.frame"))
setMethod(f="write", signature="CONOPinput", definition=function(x){
	write.table(x@Loadfile, file=as.character(x@Config$"PARAMETERS THAT IDENTIFY THE INPUT DATA"$Value[7]), sep=" ", row.names=FALSE, col.names=FALSE, quote=FALSE)
	ev <- x@Eventfile
	ev$Taxon <- paste("\'",ev$Taxon,"\'",sep="")
	ev$Abbreviation <- paste("\'",ev$Abbreviation,"\'",sep="")
	write.table(ev, file=as.character(x@Config$"PARAMETERS THAT IDENTIFY THE INPUT DATA"$Value[13]), quote=FALSE, sep=" ", row.names=FALSE, col.names=FALSE)
	sct <- as.data.frame(x@Sectfile)
	sct[,2] <- paste("\'",sct[,2],"\'",sep="")
	sct[,4] <- paste("\'",sct[,4],"\'",sep="")
	write.table(sct, file=as.character(x@Config$"PARAMETERS THAT IDENTIFY THE INPUT DATA"$Value[9]), quote=FALSE, sep=" ", row.names=FALSE, col.names=FALSE)
	})