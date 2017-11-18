
	### will need a bit more work but I think this is pretty good.
	
percent.change <- function(data,onsets){


	baselines <- unlist((onsets[which(onsets[,1]=="ONSET"),3:ncol(onsets)]+1))
	rep.baselines <- unlist(onsets[which(onsets[,1]=="BLOCK_LENGTH"),3:ncol(onsets)])
	endpoints <- (baselines-1) + rep.baselines
	
	event.design <- 	 rep(rep(unique(onsets[,2]),length(3:ncol(onsets))),rep.baselines)	
	perc.change.data <- (data[min(baselines):max(endpoints),] - data[rep(baselines,rep.baselines),]) / data[rep(baselines,rep.baselines),]
	
	perc.change.data <- perc.change.data[-c(baselines),]
	event.design <- event.design[-c(baselines)]
	

	
	block.design <- makeNominalData(as.matrix( rep(paste(rep(unique(onsets[,2]),length(3:ncol(onsets))),rep(1:length(3:ncol(onsets)),each=length(unique(onsets[,2]))),sep="__.__"),rep.baselines) ))
	
	
	block.design <- block.design[-c(baselines),]
	block.design <- t(apply(block.design, 1, "/", colSums(block.design)))
	ave.perc.change.data <- t(block.design) %*% perc.change.data
	rownames(ave.perc.change.data) <- colnames(block.design)
	
	ave.row.design <- unlist(lapply(strsplit(gsub("\\.","",rownames(ave.perc.change.data)),"____"),function(x){x[1]}))
	
	
	return(list( 	perc.change.data = perc.change.data, 
					event.design = event.design, 
					ave.perc.change.data=ave.perc.change.data, 
					ave.row.design= ave.row.design
				))
	
	
}