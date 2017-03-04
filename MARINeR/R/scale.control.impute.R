
	##requires expo.scale
scale.control.impute <- function(data.in,center=T,scale=T,control.for=T,impute=T,regressors=NULL,tol=.Machine$double.eps * 100){

				
		data_scaled <- expo.scale(data.in,center=center,scale=scale)
    
		
		
		if(control.for){
			if(is.null(regressors)){
				stop("regressors set to NULL")
			}
			if(  !(all.equal(rownames(regressors),rownames(data_scaled))) ) {
				stop("Rownames for 'data.in' and 'regressors' do not match")
			}
			
		  regressor.class.types <- sapply(regressors,class)
		  regressor.class.types_isFactor <- regressor.class.types=="factor"
		  
		  ## This should help guarantee order and arbitrary number of regressors.
		  regressors <- as.data.frame(regressors[rownames(data_scaled),])
		  rownames(regressors) <- rownames(data_scaled)
		  
		  ## R is weird.
		  if(ncol(regressors)==1){
		    this.form <- "x~."
		  }else{
		    this.form <- paste0("x~.^",ncol(regressors))
		  }
		  
		  if( sum( regressor.class.types_isFactor ) ){ ## yes, there is a factor.
		    factor.contrasts <- paste0("list(",paste0(names(which(regressor.class.types_isFactor)),"='contr.sum'",collapse=","),")")
		    data_scaled_regressed <- apply(data_scaled,
										2,
										function(x){
										resid( lm( 
											#as.formula(paste0("x~.^",ncol(regressors))),
										  as.formula(this.form),
											#regressors[rownames(data_scaled), ],
											regressors,
											na.action=na.exclude,
											contrasts=eval(parse(text=factor.contrasts))
										))
										}
									)
		  }else{
		    data_scaled_regressed <- apply(data_scaled,
		                                   2,
		                                   function(x){
		                                     resid( lm( 
											 	                      #as.formula(paste0("x~.^",ncol(regressors))),
		                                          as.formula(this.form),
											 	                      #regressors[rownames(data_scaled), ],
											 	                      regressors,
		                                     	    na.action=na.exclude
		                                     ))
		                                   }
		    )
		  }
			if(impute){
				data_scaled_regressed_imputed <- apply(data_scaled_regressed,2,
					function(x){ x[which(is.na(x))]<-mean(x,na.rm=T); return(x) } 
				)
				data_scaled_imputed <- apply(data_scaled,2,
					function(x){ x[which(is.na(x))]<-mean(x,na.rm=T); return(x) } 
				)	
				
				this.list <- list(
					data_scaled = data_scaled, 
					data_scaled_imputed = data_scaled_imputed,
					data_scaled_regressed = data_scaled_regressed, 					
					data_scaled_regressed_imputed = data_scaled_regressed_imputed
				)
				this.list <- lapply(this.list,function(x){ x[abs(x) < tol] <- 0; x } )
				return(this.list)
				
			}

			this.list <- list(
				data_scaled = data_scaled, 
				data_scaled_regressed = data_scaled_regressed
			)
			this.list <- lapply(this.list,function(x){ x[abs(x) < tol] <- 0; x } )
			return(this.list)			
												
		}else{
			
			if(impute){
				data_scaled_imputed <- apply(data_scaled,2,
					function(x){ x[which(is.na(x))]<-mean(x,na.rm=T); return(x) } 
				)
				
				this.list <- list(
					data_scaled = data_scaled, 
					data_scaled_imputed = data_scaled_imputed
				)
				return(this.list)
			}

			this.list <- list(
				data_scaled = data_scaled
			)
			
			this.list <- lapply(this.list,function(x){ x[abs(x) < tol] <- 0; x } )
			return(this.list)
		}
		
}
