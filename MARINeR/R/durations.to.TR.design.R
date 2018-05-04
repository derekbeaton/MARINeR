#  durations to TR design - not sure how well this generalizes so use with caution!
#  A function to assign time based onsets/durations for each trial to respective TRs/conditions
#'
#'  @export
#'
#'  @title \code{durations.to.TR.design}: assigns time based trials to corresponding TR number
#'
#'  @description \code{durations.to.TR.design} assign time based onsets/durations for each trial to respective TRs/conditions
#'
#'  @param onsets A numeric array of onset times for each trial
#'  @param durations The corresponding durations for each trial onset; if duration is the same across trials, one value can be entered;
#'  @param condition The corresponding condition label for each trial
#'  @param TR.length A single value representing the length of the TR in the same units as onset and durations (i.e., ms or sec)

#'
#'  @return
#'  A list with 3 elements
#'  \item{TR.design} An array of condition labels corresponding to each TR
#'  @seealso
#'
#'  @examples
#'
#'  @author Jenny Rieck

durations.to.TR.design<-function(onsets,durations,condition,TR.length,save.csv=T,csv.fn='myTRdesign.csv'){

  if(length(durations)==1){
    print('1 input for duration detected -- setting duration to be equal across trials')
    durations<-rep(durations, length(onsets))
  }

  if(length(onsets)!=length(condition)){
    stop('Onsets and conditions must be the same length')
  }
  # TR.starts<-cumsum(onsets+durations) %% TR.length
  # TR.design<-condition[which(TR.starts==1)]

  #
  # TR.starts<-round(onsets/TR.length,digits=0)

  time.starts <- onsets
  time.ends <- onsets + durations
  if(any(time.ends-time.starts<TR.length)){
    warning('Beware! Multiple events per TR')
  }

  tr.starts<-round(time.starts/TR.length)
  tr.ends<-round(time.ends/TR.length)

  overlap.trs <- tr.ends %in% tr.starts
  ## find any overlapping TRs and round the end time down one TR
  if(any(overlap.trs)){
    tr.ends[which(overlap.trs)] <- tr.ends[which(overlap.trs)]-1
  }
  ## check that all ends points end before the next TR begins
  if(any(tr.ends<tr.starts)){
    stop('Dear user: durations and onsets cover overlapping TRs.
        Please rethink your design to allow for unique TRs for each event')

  }

  ### Ok now actually make the TR design.
  TR.design<-c()
  for(t in 1:length(tr.starts)){
    TR.design[tr.starts[t]:tr.ends[t]]<-as.factor(condition)[t]
  }
  TR.design[is.na(TR.design)]<-'NULL.COND'

  TR.num<-seq(1:length(TR.design))

  if(save.csv==T){
    write.table(as.data.frame(TR.design),sep=",",file=csv.fn,row.names=F,col.names = F)
  }
  return(list(TR.design=TR.design,TR.num=TR.num,csv.fn=csv.fn))

}
