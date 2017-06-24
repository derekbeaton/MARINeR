require(assertthat)
require(data.table)

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


concatenate.data <- function(masked.subject.data, concat.method='tr-task-by-voxel'){
  data.vec <- list()
  ## need to do some quick checks here on sizes of each data matrix.
  for (subj in seq(1,length(masked.subject.data))){
    have.same.number.vols(masked.subject.data[[subj]]$dataMatrix, masked.subject.data[[subj]]$dataDesign[[1]])
    if (concat.method == 'tr-task-by-voxel'){
      data.vec[[subj]] <- vectorize.by.vol.task(masked.subject.data[[subj]]$dataMatrix, masked.subject.data[[subj]]$dataDesign[[1]])
      #ncol.subj <- dim(data.vec[[subj]])[2]
    }
  }
  data.mat <- do.call('cbind', data.vec)
  return(data.mat)
}

#datamatrix <- concatenate.data(data.list, concat.method = 'tr-task-by-voxel')
