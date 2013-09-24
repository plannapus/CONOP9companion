prep.eventfile <-
function(Input, taxa_list){
	gsub("^\\ +||| +$","",taxa_list)->taxa_list
	unique(taxa_list)->taxa
	do.call(rbind,lapply(strsplit(taxa,split=" "), function(x)x[1:2]))->a
	paste(substr(a[,1],1,3),substr(a[,2],1,3),sep=" ")->abb
	eventfile <- data.frame(Code= 1:length(taxa), 
							Abbreviation= abb, 
							Taxon= taxa)
	Input@Eventfile<-eventfile[order(eventfile$Taxon),]
	return(Input)
	}