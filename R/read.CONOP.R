read.CONOP<-function(dir){
	CONOP<-new("CONOP")
	cfg <- read.config(dir(dir, full.names=TRUE, pattern="\\.[cC][fF][gG]"))
	sect <- read.table(paste(dir,cfg$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['SECTFILE',1],sep="/"),stringsAsFactors=FALSE)
	evt<- read.table(paste(dir,cfg$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['EVENTFILE',1],sep="/"))
	n <- as.integer(as.character(cfg$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['TAXA',1]))*2 + as.integer(as.character(cfg$`PARAMETERS THAT IDENTIFY THE INPUT DATA`['EVENTS',1]))
	unloadmain <- paste(dir,cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['UNLOADMAIN',1], sep="/")
	unloadsect <- paste(dir,cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['UNLOADSECT',1], sep="/")
	temp <- readLines(unloadmain)
	
	if(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['FITS_OUT',1]=="ON"){
		s <- c(as.numeric(gsub(" *TOTAL PENALTY FOR BEST SEQUENCE: *","",temp[grep("TOTAL PENALTY FOR BEST SEQUENCE:", temp)])),
		   		as.numeric(gsub(" *Penalized fraction of improvable events: *","",temp[grep("Penalized fraction of improvable events:", temp)])), 
		   		as.numeric(gsub(" *per observed event: *","",temp[grep("per observed event:", temp)][1])))
		names(s)<-c("Total penalty for best sequence", "Penalized fraction of improvable events", "Penalty used per observed event")
		} else {
			s <- as.numeric(gsub(" *TOTAL PENALTY FOR BEST SEQUENCE: *","",temp[grep("TOTAL PENALTY FOR BEST SEQUENCE:", temp)]))
			names(s) <- "Total penalty for best sequence"
			}
	CONOP@Summary <- s; CONOP@Config <- cfg
	CONOP@Sectfile <- as.data.frame(sect,stringsAsFactors=FALSE)
	CONOP@Eventfile <- as.data.frame(evt,stringsAsFactors=FALSE)
	
	if (!substr(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['OBSDFILE',1],1,3)%in%c("off","OFF")){
		a <- gsub("^[aA][dD][dD]","",cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['OBSDFILE',1])
		obsdfile <- readLines(paste(dir,a,sep="/"))
		options(warn=-1)
		obsdfile <- apply(do.call(rbind,strsplit(obsdfile, split=" +"))[,-1], 2, as.numeric)
		options(warn=0)
		colnames(obsdfile)<-c("Event","type",CONOP@Sectfile[,2])
		obsdfile[obsdfile==0]<-NA
		CONOP@Obsdfile <- obsdfile
		} else if(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['OBS_OUT',1]=="ON"){
			ind <- grep("OBSERVED HORIZONS",temp)
			ind <- ind + grep("\\[",temp[(ind+1):length(temp)])[1]
			x <- temp[ind:(ind+n-1)]
			do.call(rbind,strsplit(x,split="[\\[\\]]", perl=TRUE))->y
			options(warn=-1)
			apply(do.call(rbind,strsplit(paste(y[,1],y[,3],sep=" "),split=" +", perl=TRUE))[,-1],2,as.numeric)->obsdfile
			options(warn=0)
			colnames(obsdfile)<-c("Event","type",CONOP@Sectfile[,2])
			CONOP@Obsdfile <- obsdfile
			}
	
	if (!substr(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['PLCDFILE',1],1,3)%in%c("off","OFF")){
		a <- gsub("^[aA][dD][dD]","",cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['PLCDFILE',1])
		plcdfile <- readLines(paste(dir,a,sep="/"))
		options(warn=-1)
		plcdfile <- apply(do.call(rbind,strsplit(plcdfile, split=" +"))[,-1], 2, as.numeric)
		options(warn=0)
		colnames(plcdfile)<-c("Event","type",CONOP@Sectfile[,2])
		CONOP@Plcdfile <- plcdfile
		} else if(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['LOC_OUT',1]=="ON"){
			ind <- grep("THE LINE OF CORRELATION",temp)
			ind <- ind + grep("\\[",temp[(ind+1):length(temp)])[1]
			x <- temp[ind:(ind+n-1)]
			y <- do.call(rbind,strsplit(x,split="[\\[\\]]", perl=TRUE))
			options(warn=-1)
			apply(do.call(rbind,strsplit(paste(y[,1],y[,3],sep=" "),split=" +", perl=TRUE))[,-1],2,as.numeric)->plcdfile
			options(warn=0)
			colnames(plcdfile)<-c("Event","type",CONOP@Sectfile[,2])
			CONOP@Plcdfile <- plcdfile
			}

	if(!substr(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['COMPOSFILE',1],1,3)%in%c("off","OFF")){
		a <- gsub("^[aA][dD][dD]","",cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['COMPOSFILE',1])
		composfile <- readLines(paste(dir,a, sep="/"))
		composfile <- apply(do.call(rbind,strsplit(composfile,split=" +"))[,-1],2,as.numeric)
		colnames(composfile) <- c("Event", "Type", "-", "Level", "Level rank", "Move", "-", "-")
		CONOP@Composfile <- composfile
		} else if (cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['COMP_OUT',1]=="ON"){
			temp[(grep("COMPOSITE",temp)+2):(grep("COMPOSITE",temp)+1+n)]->x
			do.call(rbind,strsplit(x,split="{",fixed=TRUE))->x
			apply(do.call(rbind,strsplit(x[,1],split=" +", perl=TRUE))[,-1],2,as.numeric)[,1:5]->composfile
			colnames(composfile) <- c("Event", "Type", "Space below", "Level", "Sections")
			CONOP@Composfile <- composfile
			}
	
	if (!substr(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['EXTNFILE',1],1,3)%in%c("off","OFF")){
		a <- gsub("^[aA][dD][dD]","",cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['EXTNFILE',1])
		extnfile <- readLines(paste(dir,a,sep="/"))
		options(warn=-1)
		extnfile <- apply(do.call(rbind,strsplit(extnfile, split=" +"))[,-1], 2, as.numeric)
		options(warn=0)
		colnames(extnfile)<-c("Event","type",CONOP@Sectfile[,2])
		CONOP@Extnfile <- extnfile
		} else if(cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['INCR_OUT',1]=="ON"){
			ind <- grep("INCREMENTAL",temp)
			for(i in ind){
				ind2 <- i+grep("\\[",temp[-(1:i)])[1]
				if(length(grep("WARNING",temp[ind2:(ind2+n-1)]))>=0){ x <- temp[ind2:(ind2+n-1)]}
				}
			y <- do.call(rbind,strsplit(x,split="[\\[\\]]", perl=TRUE))
			options(warn=-1)
			extnfile <- apply(do.call(rbind,strsplit(paste(y[,1],y[,3],sep=" "),split=" +", perl=TRUE))[,-1],2,as.numeric)
			options(warn=0)
			colnames(extnfile)<-c("Event","type",CONOP@Sectfile[,2])
			CONOP@Extnfile <- extnfile
			}
			
	if (cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['EVNT_OUT',1]=="ON"){
		unloadevnt <- readLines(paste(dir,cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['UNLOADEVNT',1], sep="/"))
		unloadevnt <- unloadevnt[grep("\\{",unloadevnt)[1]:(n+grep("\\{",unloadevnt)[1]-1)]
		x <- do.call(rbind,strsplit(unloadevnt,split="{",fixed=TRUE))[,1]
		x <- gsub("--","- -",x)
		unloadevnt <- apply(do.call(rbind,strsplit(x,split=" +"))[,c(2,4:7)],2,as.numeric)
		colnames(unloadevnt)<-c("Event","Type","Extension","Penalty","Sections")
		rownames(unloadevnt)<-1:n
		CONOP@Events<-unloadevnt
		}
		
	if (cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['SECT_OUT',1]=="ON"){
		Output_Section<-list()
		unloadsect <- readLines(paste(dir,cfg$`PARAMETERS THAT DETERMINE THE NATURE AND LOCATION OF OUTPUT`['UNLOADSECT',1], sep="/"))
		for(j in 1:nrow(CONOP@Sectfile)){
			y <- unloadsect[(grep(substr(CONOP@Sectfile[j,4],1,20),unloadsect)+3):(grep(substr(CONOP@Sectfile[j,4],1,20),unloadsect)+2+n)]
			gsub("not seen","NA",y)->y
			gsub("unseen","NA",y)->y
			do.call(rbind,strsplit(y,split="{",fixed=TRUE))->yy
			yy <- gsub("--","- -",yy)
			do.call(rbind,strsplit(yy[,1],split=" +"))->yy1
			options(warn=-1)
			apply(yy1[,c(2,4:9)],2,as.numeric)->yyy
			options(warn=0)
			colnames(yyy)<-c("Event number", "Event type", "Observed", "Placed", "Unweighted misfit","Weighted misfit","Level misfit")
			rownames(yyy)<-1:nrow(yy)
			rm(y,yy1,yy)
			Output_Section[[j]]<-yyy
			names(Output_Section)[j]<-as.character(CONOP@Sectfile[j,4])
			}
		
		CONOP@Sections <- Output_Section
		}	
		
	if(!substr(cfg$`PARAMETERS THAT INFLUENCE EFFICIENCY OF SEARCH FOR BEST SOLUTION`['CURVFILE',1],1,3)%in%c("off","OFF")){
		a <- gsub("^[aA][dD][dD]","",cfg$`PARAMETERS THAT INFLUENCE EFFICIENCY OF SEARCH FOR BEST SOLUTION`['CURVFILE',1])
		curvfile <- readLines(paste(dir,a,sep="/"))
		curvfile <- apply(do.call(rbind,strsplit(curvfile,split=" +"))[,-1],2,as.numeric)
		colnames(curvfile) <- paste("Position", 1:n, sep=" ")
		rownames(curvfile) <- paste("Event", 1:n, sep=" ")
		CONOP@Curvfile <- curvfile
		}
	
	CONOP
	}