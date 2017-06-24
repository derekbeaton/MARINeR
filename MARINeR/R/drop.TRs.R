#  Drop TRs based on condition label in design or by TR#

#'  @export
#'
#'  @title \code{drop.TRs}: Drop TRs based by design or TR index
#'
#'  @description \code{drop.TRs} After reading in data files via \code{\link{subject.data.list}}, drop TRs based on design conditions or TR number; This function will drop TRs for each subject in the data.list
#'
#'  @param x a list of subject data derived from \code{\link{subject.data.list}}
#'  @param TRs2drop condition labels or TR numbers to be dropped from analysis (default="drop"); TR numbers should indexx across concatenated runs (rather than bye each run individually)
#'  @return x a list of subject data TRs dropped from $dataMatrix, $runDesign, and $dataDesign
#'  @seealso
#'  
#'  @examples
#'  # To drop trials of condition 'drop' or 'FIXATION'
#'  data.list.dropped<-drop.TRs(data.list,c("drop","FIXATION"))
#'  
#'  # To drop the first and last 5 TRs of each run
#'  run01.drop<-c(head(which(data.list$sub.09$runDesign==1),5), tail(which(data.list$sub.09$runDesign==1),5))
#'  run02.drop<-c(head(which(data.list$sub.09$runDesign==2),5), tail(which(data.list$sub.09$runDesign==2),5))
#'  data.list.dropped<-drop.TRs(data.list,c(run01.drop,run02.drop))
#'  TRs2drop<-c(run01.drop, run02.drop)
#'  
#'  @author Jenny Rieck


drop.TRs<-function(x, TRs2drop=c("drop")){
  

  subjs<-names(x)
  
  for(s in 1:length(subjs)){
    this.subj<-x[[subjs[s]]]
    
    if(class(TRs2drop)=="character"){
      
      dropTRs<-c()
      for(d in 1:length(TRs2drop)){
        this.drop<-which(this.subj$dataDesign== TRs2drop[d])
        dropTRs<-c(dropTRs, this.drop)
      }
    }else if(class(TRs2drop)=="numeric"){
      dropTRs<-TRs2drop
      
    }else {
      print("Please input TRs2drop as class character (TR design condition) or numeric (TR index)")
    }
    
    x[[subjs[s]]]$dataDesign<-as.data.frame(this.subj$dataDesign[-dropTRs,])
    x[[subjs[s]]]$runDesign<-as.matrix(this.subj$runDesign[-dropTRs,])
    x[[subjs[s]]]$dataMatrix<-this.subj$dataMatrix[-dropTRs,]
    
  }
  return(x)
}