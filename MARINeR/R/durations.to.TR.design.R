### work in progress
duration.to.TR.design<-function(onsets,durations,condition,TR_length){
  
  design.in<-read.table(paste0(datadir,'sub-01/func/',data.pref,'sub-01',data.mid,'run-01_events.tsv'),header=T,sep='\t')
  
  onsets<-design.in$onset
  durations<-design.in$duration
  condition<-design.in$trial_type
  TR<-3
  
  
  
  
  
  
}