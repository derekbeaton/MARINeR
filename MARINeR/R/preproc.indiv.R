#  Preprocessing on the individual subject level

#'  @export
#'
#'  @title \code{preproc.indiv}: Preprocessing on the individual subject level
#'
#'  @description \code{preproc.indiv} Preprocessing on the individual subject level; includes linear and quadratric detrend, and ?!?!?
#'
#'  @param x a list of subject data derived from \code{\link{subject.data.list}}
#'  @param lin.detrend Flag (default=T) for linear detrend (for each run separately)
#'  @param quad.detrend Flag (default=T) for quadratic detrend (for each run separately) on top of linear detrend
#'  
#'  @return x a list of subject data with preprocessed data included per subject

#'  @seealso
#'  
#'  @examples
#'  
#'  @author Jenny Rieck

preproc.indiv<-function(x,lin.detrend=T, quad.detrend=T){
  
  subjs<-names(x)
  
  for(s in 1:length(subjs)){
    
    this.subj<-x[[subjs[s]]]
    
    if(lin.detrend==T){
      print('Running linear detrend per run per subject')
      ##run.breaks<-which(x$runDesign[-1L] != x$runDesign[-length(x$runDesign)]) ## find when the run index changes
      runs<-unique(this.subj$runDesign)
      dataMatrixLinDetrend<-c()
      
      for(r in 1:length(runs)){
        this.run<-this.subj$dataMatrix[which(this.subj$runDesign==runs[r]),]
        
        time<-seq(1:dim(this.run)[1])
        this.run.detrend<-residuals(lm(this.run~time))
        dataMatrixLinDetrend<-rbind(dataMatrixLinDetrend,this.run.detrend)
      }
      this.subj$dataMatrixLinDetrend<-dataMatrixLinDetrend
      
      if(quad.detrend==T){
        print('Running additional quadtric detrend per run per subject')
        dataMatrixQuadDetrend<-c()
        
        for(r in 1:length(runs)){
          this.run<-this.subj$dataMatrixLinDetrend[which(this.subj$runDesign==runs[r]),]
          
          time<-seq(1:dim(this.run)[1])^2
          this.run.detrend<-residuals(lm(this.run~time))
          dataMatrixQuadDetrend<-rbind(dataMatrixQuadDetrend,this.run.detrend)
        }
        this.subj$dataMatrixQuadDetrend<-dataMatrixQuadDetrend
      }   
    }
    x[[subjs[s]]]$dataMatrixLinDetrend<-this.subj$dataMatrixLinDetrend
    x[[subjs[s]]]$dataMatrixQuadDetrend<-this.subj$dataMatrixQuadDetrend
  }
  return(x)
}