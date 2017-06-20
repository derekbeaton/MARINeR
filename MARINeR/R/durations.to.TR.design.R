### work in progress
duration.to.TR.design<-function(onsets,durations,condition,TR_length){
  
  design.in<-read.csv(paste0(datadir,'mariner_run-01_design.csv'))
  
  onsets<-design.in$seconds - 1
  durations<-1
  condition<-design.in$condition
  TR<-3
  
  if(length(durations)==1){
    print('1 input for duration detected -- setting duration to be equal across trials')
    durations.tot<-rep(durations, length(onsets))
  }
  
  TR.starts<-cumsum(onsets+durations) %% TR
  TR.design<-design.in$condition[which(TR.starts==1)]
  
  
}