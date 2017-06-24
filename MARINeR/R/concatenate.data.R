require(assertthat)

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

vectorize.by.task <- function(subject.data, design){
  rownames(subject.data) <- make.names(design, unique = TRUE)
  return(subject.data)
}

# create a single row for each task -- problem with different number of volumes per task
vectorize.by.task_old <- function(subject.data, design){
  data.vec <- list()
  count <- 0
  for (level in levels(design)){
    print(level)
    count <- count + 1
    data.vec[[count]] <- vectorize.data(subject.data[design == level,])
    print(data.vec[[count]])
  }
  data.vec <- data.frame(matrix(unlist(data.vec), nrow=length(levels(design)), byrow=T))
  rownames(data.vec) <- levels(design)
  return(data.vec)
}

concatenate.data <- function(masked.subject.data, concat.method = 'task'){

  ## need to do some quick checks here on sizes of each data matrix.
  for (i in length(data.list)){
    have.same.number.vols(data.list[[i]]$dataMatrixPreproc, data.list[[i]]$dataDesign[[1]])
    if (concat.method == 'task'){
      toto <- vectorize.by.task(data.list[[i]]$dataMatrixPreproc, data.list[[i]]$dataDesign[[1]])
    }
  }

}

