
subject.data.list <- function(subj.list){

  masked.subject.data <- vector("list",length(subj.list))
  names(masked.subject.data) <- names(subj.list)

  for(s in 1:length(subj.list)){
    masked.subject.data[[s]] <- volsToMatrix(subj.list[[s]]$data, subj.list[[s]]$masks,subj.list[[s]]$design)
  }

  return(masked.subject.data)

}
