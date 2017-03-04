##requires expo.scale

scale.control.impute <- function(data.in,center=T,scale=T,control.for=T,impute=T,regressors=NULL){

		data.out <- expo.scale(data.in,center=center,scale=scale)

		if(control.for){
			if(is.null(regressors)){
				stop("regressors set to NULL")
			}
			if(  !(all.equal(rownames(regressors),rownames(data.out))) ) {
				stop("Rownames for 'data.in' and 'regressors' do not match")
			}

		  regressor.class.types <- sapply(regressors,class)
		  regressor.class.types_isFactor <- regressor.class.types=="factor"

		  ## This should help guarantee order and arbitrary number of regressors.
		  regressors <- as.data.frame(regressors[rownames(data.out),])
		  rownames(regressors) <- rownames(data.out)

		  ## R is weird.
		  if(ncol(regressors)==1){
		    this.form <- "x~."
		  }else{
		    this.form <- paste0("x~.^",ncol(regressors))
		  }

		  if( sum( regressor.class.types_isFactor ) ){ ## yes, there is a factor.
		    factor.contrasts <- paste0("list(",paste0(names(which(regressor.class.types_isFactor)),"='contr.sum'",collapse=","),")")
		    data.out <- apply(data.out,
										2,
										function(x){
										resid( lm(
										  as.formula(this.form),
											regressors,
											na.action=na.exclude,
											contrasts=eval(parse(text=factor.contrasts))
										))
										}
									)
		  }else{
		    data.out <- apply(data.out,
		                                   2,
		                                   function(x){
		                                     resid( lm(
		                                          as.formula(this.form),
											 	                      regressors,
		                                     	    na.action=na.exclude
		                                     ))
		                                   }
		    )
		  }
			if(impute){
			  data.out <- apply(data.out,2,
					function(x){ x[which(is.na(x))]<-mean(x,na.rm=T); return(x) }
				)
				return(data.out)
			}
			return(data.out)
		}else{
			if(impute){
			  data.out <- apply(data.out,2,
					function(x){ x[which(is.na(x))]<-mean(x,na.rm=T); return(x) }
				)
				return(data.out)
			}
			return(data.out)
		}
}
