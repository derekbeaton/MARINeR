#  Preprocessing on the individual subject level

#'  @export
#'
#'  @title \code{preproc.indiv}: Preprocessing on the individual subject level
#'
#'  @description \code{preproc.indiv} Preprocessing on the individual subject level; includes linear and quadratric detrend, and ?!?!?
#'
#'  @param x a list of subject data derived from \code{\link{subject.data.list}}
#'  @param col.center
#'  @param col.scale
#'  @param row.center
#'  @param row.scale
#'  @param svd.rank.rebuild (default=F) a boolean. Do you want to use a lower rank approximation of your data?
#'  @param rank.rebuild   (default=NA) create low rank approximation of data matrix. Can take: an integer for number of components, a decimal > 0 and < 1 for percent explained variance, or a vector of values to select for specific components. The default is NA and will return a low rank approximation of 90% variance or 10 components (which ever is the fewer # of components).
#'  @param gsr (default=T) for global signal regression (regress out the mean per row, usually volume)
#'  @param gsr.vals (default = "mean"). Values to use for the global signal regression; default is mean per volume (row). "median" is also available or you can pre-specifify values. ## NOTE FOR OURSELVES: if gsr.vals comes in, it will have to be a list of length x, wherein each item is of nrow per subj.
#'  @param detrend.per.run (default=T) for polynominal detrend (for each run separately)
#'  @param detrend.degrees (default=1) - degrees of polynomial to detrend; 1 for linear, 2 for linear & quadratic, and so on... (uses R's poly()).
#'  @param svd.norm (default=T) to svd normalize (i.e., matrix z-score; divide the subject matrix by its first singular value)
#'
#'  @return x a list of subject data with preprocessed data included per subject under x$subj$this.subj$dataMatrix

#'  @seealso
#'
#'  @examples
#'
#'  @author Jenny Rieck
preproc.indiv<-function(x, col.center = T, col.scale = F, row.center = F, row.scale = F, svd.rank.rebuild = T, rank.rebuild = NA, gsr =T, gsr.vals="mean", detrend.per.run=T, detrend.degrees=1, svd.norm = T){

  ## may not be necessary.
  subjs<-names(x)

  for(s in 1:length(subjs)){

    #this.subj<-x[[subjs[s]]]
    #dataMatrixPreproc<-this.subj$dataMatrix
    x[[subjs[s]]]$dataMatrix <- expo.scale(x[[subjs[s]]]$dataMatrix,center=col.center,scale=col.scale)
    ## no row norms for now...
    
    ### run based preprocessing
    runs<-unique(x[[subjs[s]]]$runDesign)

    if(svd.rank.rebuild==T){
      print(paste('Running svd low rank for', subjs[s]))
      x[[subjs[s]]]$dataMatrix<-svd.low.rank.rebuild(x[[subjs[s]]]$dataMatrix,rank.rebuild=rank.rebuild)
    }

    if(gsr==T){
      x[[subjs[s]]]$dataMatrix <- gsr(x[[subjs[s]]]$dataMatrix,gsr.vals=gsr.vals)
    }

    if(detrend.per.run==T){
      for(r in 1:length(runs)){
        print(paste('Running detrend: run', r, 'for', subjs[s]))
        x[[subjs[s]]]$dataMatrix[which(x[[subjs[s]]]$runDesign==runs[r]),] <- degree.detrend(x[[subjs[s]]]$dataMatrix[which(x[[subjs[s]]]$runDesign==runs[r]),],deg=detrend.degrees)
      }
    }

    if(svd.norm==T){
      print(paste('Running svd norm for', subjs[s]))
      x[[subjs[s]]]$dataMatrix<-svd.norm(x[[subjs[s]]]$dataMatrix)
    }

 }
  return(x)
}
