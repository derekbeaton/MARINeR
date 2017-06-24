#  Preprocessing on the individual subject level

#'  @export
#'
#'  @title \code{preproc.indiv}: Preprocessing on the individual subject level
#'
#'  @description \code{preproc.indiv} Preprocessing on the individual subject level; includes linear and quadratric detrend, and ?!?!?
#'
#'  @param x a list of subject data derived from \code{\link{subject.data.list}}
#'  @param svd.rank.rebuild (default=F) a boolean. Do you want to use a lower rank approximation of your data?
#'  @param rebuild.rank   (default=0) create low rank approximation of data matrix. Can take: an integer for number of components, a decimal > 0 and < 1 for percent explained variance, or a vector of values to select for specific components.
#'  @param detrend (default=T) for polynominal detrend (for each run separately)
#'  @param detrend.degrees (default=c(1,2)) - degrees of polynomial to detrend; for linear detrend c(1); for linear then quadratic c(1,2)
#'  @param gsr (default=T) for global signal regression (regress out the mean per row, usually volume)
#'  @param svd.norm (default=T) to svd normalize (i.e., matrix z-score; divide the subject matrix by its first singular value)
#'
#'  @return x a list of subject data with preprocessed data included per subject under x$subj$this.subj$dataMatrix

#'  @seealso
#'
#'  @examples
#'
#'  @author Jenny Rieck
preproc.indiv<-function(x, gsr =T, detrend=T, detrend.degrees=c(1,2), svd.norm = T, svd.low.rank.approx = T){

  ## may not be necessary.
  subjs<-names(x)

  for(s in 1:length(subjs)){

    this.subj<-x[[subjs[s]]]
    #dataMatrixPreproc<-this.subj$dataMatrix

    ### run based preprocessing
    runs<-unique(this.subj$runDesign)

    if(svd.rank.rebuild==T){
      print(paste('Running svd low rank for', subjs[s]))
      dataMatrixSVDlow<-svd.low.rank.rebuild(this.subj$dataMatrix)
      this.subj$dataMatrix<-dataMatrixSVDlow
    }

    if(gsr==T){
      dataMatrixGSR<-c()

      for(r in 1:length(runs)){
        print(paste('Running global signal regression: run', r, 'for', subjs[s]))
        this.run<-this.subj$dataMatrix[which(this.subj$runDesign==runs[r]),]
        this.run.gsr<-global.signal.regression(this.run)
        dataMatrixGSR<-rbind(dataMatrixGSR, this.run.gsr)
      }
      this.subj$dataMatrix<-dataMatrixGSR

    }

    if(detrend==T){
      dataMatrixDetrend<-c()

      for(r in 1:length(runs)){
        print(paste('Running detrend: run', r, 'for', subjs[s]))
        this.run<-this.subj$dataMatrix[which(this.subj$runDesign==runs[r]),]

        for(d in 1:length(detrend.degrees)){
          this.run.detrend<-degree.detrend(this.run,deg=detrend.degrees[d])
          this.run<-this.run.detrend
        }
        dataMatrixDetrend<-rbind(dataMatrixDetrend,this.run)
      }
      this.subj$dataMatrix<-dataMatrixDetrend
    }


    if(svd.norm==T){
    print(paste('Running svd norm for', subjs[s]))

    dataMatrixSVDn<-svd.norm(this.subj$dataMatrix)
    this.subj$dataMatrix<-dataMatrixSVDn

    }


    x[[subjs[s]]]$this.subj$dataMatrix<-this.subj$dataMatrix
 }
  return(x)
}
