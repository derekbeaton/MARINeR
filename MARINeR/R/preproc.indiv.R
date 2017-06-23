#  Preprocessing on the individual subject level

#'  @export
#'
#'  @title \code{preproc.indiv}: Preprocessing on the individual subject level
#'
#'  @description \code{preproc.indiv} Preprocessing on the individual subject level; includes linear and quadratric detrend, and ?!?!?
#'
#'  @param x a list of subject data derived from \code{\link{subject.data.list}}
#'  @param detrend Flag (default=T) for linear detrend (for each run separately)
#'  @param detrend.degrees (default=c(1,2)) - degrees of polynomial to detrend; for linear detrend c(1); for linear then quadratic c(1,2)
#'  @param global.signal.regreesion Flag (default=T) for regressing out mean of matrix
#'  @param svd.norm Flag (default=T) to svd normalize (divide by first singular value of the entire subject)
#'  
#'  @return x a list of subject data with preprocessed data included per subject under x$subj$dataMatrixPreproc

#'  @seealso
#'  
#'  @examples
#'  
#'  @author Jenny Rieck

preproc.indiv<-function(x,gsr =T, detrend=T, detrend.degrees=c(1,2),svdn=T, svdlowrank=T){
  
  subjs<-names(x)
  
  ### based on functions in ./TempCode/PreProc_Funcs.R
  
  for(s in 1:length(subjs)){
    
    this.subj<-x[[subjs[s]]]
    dataMatrixPreproc<-this.subj$dataMatrix
    
    ### run based preprocessing
    runs<-unique(this.subj$runDesign)
    
    if(gsr==T){
      dataMatrixGSR<-c()
      
      for(r in 1:length(runs)){
        print(paste('Running global signal regression: run', r, 'for', subjs[s]))
        this.run<-dataMatrixPreproc[which(this.subj$runDesign==runs[r]),]
        this.run.gsr<-global.signal.regression(this.run)
        dataMatrixGSR<-rbind(dataMatrixGSR, this.run.gsr)
      }
      dataMatrixPreproc<-dataMatrixGSR
      
    }
    
    if(detrend==T){
      dataMatrixDetrend<-c()
      
      for(r in 1:length(runs)){
        print(paste('Running detrend: run', r, 'for', subjs[s]))
        this.run<-dataMatrixPreproc[which(this.subj$runDesign==runs[r]),]
        
        for(d in 1:length(detrend.degrees)){
          this.run.detrend<-degree.detrend(this.run,deg=detrend.degrees[d])
          this.run<-this.run.detrend
        }
        dataMatrixDetrend<-rbind(dataMatrixDetrend,this.run)
      }
      dataMatrixPreproc<-dataMatrixDetrend
    }
    
    
    if(svdn==T){
    print(paste('Running svd norm for', subjs[s]))
  
    dataMatrixSVDn<-svd.norm(dataMatrixPreproc)
    dataMatrixPreproc<-dataMatrixSVDn
      
    }
    
    if(svdlowrank==T){
      print(paste('Running svd low rank for', subjs[s]))  
      dataMatrixSVDlow<-svd.low.rank.rebuild(dataMatrixPreproc)
      dataMatrixPreproc<-dataMatrixSVDlow
      
    }
    
    x[[subjs[s]]]$dataMatrixPreproc<-dataMatrixPreproc
 }
  return(x)
}