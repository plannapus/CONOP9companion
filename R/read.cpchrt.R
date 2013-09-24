read.cpchrt <-
function(file=file.choose()){
	readLines(file)->a
	a[12:length(a)]->a
	do.call(rbind,strsplit(a,split="\\["))[,1]->b
	strsplit(b,split=" +", perl=TRUE)->b1
	apply(do.call(rbind,lapply(b1,function(x)x[2:5])),2,as.numeric)->chart
	do.call(c,lapply(b1,function(x)paste(x[-(1:5)],collapse=" ")))->rownames(chart)
	if(is.na(chart[nrow(chart),1])){chart[-nrow(chart),]->chart}
	return(chart)
	}
