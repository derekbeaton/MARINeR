require(assertthat)
require(data.table)
require(abind)

have.same.number.vols <- function(subject.data, design){
  assert_that(not_empty(subject.data), not_empty(design))
  assert_that(is.numeric(subject.data), is.factor(design))
  dim(subject.data)[1] == dim(design)[1]
}

on_failure(have.same.number.vols) <- function(call, env) {
  paste0(deparse(call$x), " Your data and design list have a different number of rows (i.e., volumes)")
}


vectorize.data <- function(data){
  orig.data.dim <- dim(data)
  data <- cbind(c(data))
  return(data)
}

vectorize.by.vol.task <- function(subject.data, design){
  rownames(subject.data) <- make.names(design, unique = TRUE)
  return(as.data.frame(subject.data))
}


concatenate.data <- function(masked.subject.data, concat.method='by_vols'){
  data.vec <- vector(list,length(masked.subject.data))
  ## need to do some quick checks here on sizes of each data matrix.
  for (subj in seq(1,length(masked.subject.data))){

    #if (concat.method == 'by_voxels'){   ## concatenate col-wise where voxels are repeated but vols are common
      have.same.number.vols(masked.subject.data[[subj]]$dataMatrix, masked.subject.data[[subj]]$dataDesign[[1]])
      data.vec[[subj]] <- vectorize.by.vol.task(masked.subject.data[[subj]]$dataMatrix, masked.subject.data[[subj]]$dataDesign[[1]])
    #}
    #if (concat.method == 'by_vols'){  ## concatenate row-wise where vols are repeated but voxels are common

    #}
    #if (concat.method == 'depth'){   ## concatenate depth-wise where voxels and vols are repeated in a tensor/array
    #}
  }

  if(concat.method == 'by_voxels'){
    data.mat <- do.call('cbind', data.vec)
      ## need to guarantee an order before this
  }
  if(concat.method == 'by_vols'){
    data.mat <- do.call('rbind', data.vec)
    ## need to guarantee an order before this
  }
  if(concat.method == 'depth'){
    data.mat <- do.call('abind', c(data.vec,along=3))
    ## need to guarantee an order before this
  }

  return(data.mat)
}

#datamatrix <- concatenate.data(data.list, concat.method = 'tr-task-by-voxel')
