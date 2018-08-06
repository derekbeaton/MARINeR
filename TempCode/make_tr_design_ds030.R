sub<-'sub-10304'

tsv.in<-read.table(paste0('../data/ds000030/',sub,'/func/',sub,'_task-scap_events.tsv'),header = T)

tr.design<-durations.to.TR.design(tsv.in$onset,tsv.in$duration,tsv.in$trial_type,2,
                                  T,paste0(paste0('../data/ds000030/',sub,'/func/',sub,'_TR_design.csv')))
