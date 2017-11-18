

percent.change <- function(data,onsets){


	baselines <- unlist((onsets[which(onsets[,1]=="ONSET"),3:ncol(onsets)]+1))
	rep.baselines <- unlist(onsets[which(onsets[,1]=="BLOCK_LENGTH"),3:ncol(onsets)])
	endpoints <- (baselines-1) + rep.baselines
	
	perc.change.data <- (data[min(baselines):max(endpoints),] - data[rep(baselines,rep.baselines),]) / data[rep(baselines,rep.baselines),]
		## baselines here provide the "design"
		# make design based on baselines
			### this needs to be the weighted design ala BADA.
	ave.perc.change.data <- t(block.design) %*% perc.change.data
	rownames(ave.perc.change.data) <- colnames(block.design)
	
	return(list(perc.change.data= perc.change.data,ave.perc.change.data=ave.perc.change.data))
	
	
}