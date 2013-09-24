prep.weight <-
function(Input, method='badspecies', 
		bad, weight.bad='0.50', 
		inconsistent, weight.inconsistent='0.50', 
		paclist, abmat, mbsf, pacman.sample.ratio=0.10, weight.pacman='0.50'){
	Input@Loadfile->loadfile
	Input@Eventfile->eventfile
	Input@Sectfile->sectfile
	
	if(method=='badspecies'){
		for(i in 1:length(bad)){
			gsub("^\\ +||| +$","",bad[i])->bad[i]
			eventfile[eventfile[,3]==bad[i],1]->code
			loadfile[loadfile[,1]==code,8]<-format(as.numeric(weight.bad)*as.numeric(loadfile[loadfile[,1]==code,8]),digits=2,nsmall=2)
			loadfile[loadfile[,1]==code,7]<-format(as.numeric(weight.bad)*as.numeric(loadfile[loadfile[,1]==code,7]),digits=2,nsmall=2)
			}
		}
		
	if(method=='badevent'){
		for(i in 1:nrow(bad)){
			gsub("^\\ +||| +$","",bad[i,1])->bad[i,1]
			gsub("^\\ +||| +$","",bad[i,2])->bad[i,2]
			eventfile[eventfile[,3]==bad[i,1],1]->codeFO
			eventfile[eventfile[,3]==bad[i,2],1]->codeLO
			loadfile[loadfile[,1]==codeFO & loadfile[,2]%in%c(-1,1),8]<-format(as.numeric(weight.bad)*as.numeric(loadfile[loadfile[,1]==codeFO & loadfile[,2]%in%c(-1,1),8]),digits=2,nsmall=2)
			loadfile[loadfile[,1]==codeFO & loadfile[,2]%in%c(-1,1),7]<-format(as.numeric(weight.bad)*as.numeric(loadfile[loadfile[,1]==codeFO & loadfile[,2]%in%c(-1,1),7]),digits=2,nsmall=2)
			loadfile[loadfile[,1]==codeLO & loadfile[,2]%in%c(-2,2),8]<-format(as.numeric(weight.bad)*as.numeric(loadfile[loadfile[,1]==codeLO & loadfile[,2]%in%c(-2,2),8]),digits=2,nsmall=2)
			loadfile[loadfile[,1]==codeLO & loadfile[,2]%in%c(-2,2),7]<-format(as.numeric(weight.bad)*as.numeric(loadfile[loadfile[,1]==codeLO & loadfile[,2]%in%c(-2,2),7]),digits=2,nsmall=2)
			}
		}
		
	if(method=='pacman' | method=='pac'){
		names(paclist$Profiling[(paclist$Profiling/rowSums(abmat))>=pacman.sample.ratio])->outsamples
		if(length(outsamples)!=0){
			Code_Sect<-function(X){code<-0;if(length(grep(X,sectfile[,4]))!=0){sectfile[grep(X,sectfile[,4]),1]->code};return(code)}
			strsplit(outsamples,split=".",fixed=TRUE)->decomp
			sites<-c()
			for(i in 1:length(decomp)){
				strsplit(decomp[[i]][1],split="X")[[1]][2]->sites[i]
				if(is.na(sites[i])){decomp[[i]][1]->sites[i]}
				}
			as.vector(sapply(sites,FUN=Code_Sect))->outcodes
			mbsf[(paclist$Profiling/rowSums(abmat))>=pacman.sample.ratio]->outmbsf
			for(i in 1:length(outcodes)){
				loadfile[loadfile[,3]==outcodes[i] & loadfile[,4]==(-1)*as.vector(t(outmbsf))[i],8]<-format(as.numeric(weight.pacman)*as.numeric(loadfile[loadfile[,3]==outcodes[i] & loadfile[,4]==(-1)*as.vector(t(outmbsf))[i],8]),digits=2,nsmall=2)
				loadfile[loadfile[,3]==outcodes[i] & loadfile[,4]==(-1)*as.vector(t(outmbsf))[i],7]<-format(as.numeric(weight.pacman)*as.numeric(loadfile[loadfile[,3]==outcodes[i] & loadfile[,4]==(-1)*as.vector(t(outmbsf))[i],7]),digits=2,nsmall=2)

				}
			}
		}
		
	if(method=='inconsistent' | method=='gap'){
		for(i in 1:length(inconsistent)){
			gsub("^\\ +||| +$","",inconsistent[i])->inconsistent[i]
			eventfile[eventfile[,3]==inconsistent[i],1]->code
			loadfile[loadfile[,1]==code,7]<-format(as.numeric(weight.inconsistent)*as.numeric(loadfile[loadfile[,1]==code,7]),digits=2,nsmall=2)
			loadfile[loadfile[,1]==code,8]<-format(as.numeric(weight.inconsistent)*as.numeric(loadfile[loadfile[,1]==code,8]),digits=2,nsmall=2)			}
		}
	Input@Loadfile <- loadfile
	return(Input)
	}
