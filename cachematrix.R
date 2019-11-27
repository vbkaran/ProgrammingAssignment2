## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(sample(1:200,10),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmean <- function(mean) s <<- mean
  getmean <- function() s
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
##
cachemean <- function(x, ...) {
  s <- x$getmean()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- mean(data, ...)
  x$setmean(s)
  s
}

