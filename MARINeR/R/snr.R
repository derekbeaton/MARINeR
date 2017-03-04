snr <- function(DATA){
  return(apply(DATA,2,function(x){sqrt(mean(x)/sd(x))}))
}

