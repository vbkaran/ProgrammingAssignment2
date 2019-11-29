## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) { ## define the argument with a default object of matrix
  
  inv <- NULL                              ## initialize inv as a null value, this will hold the inverse of the matrix
  set <- function(y) {                     ## define the set function to assign new
    x <<- y                                ## set the value of matrix in the parent environment
    inv <<- NULL                           ## if a new matrix is passed, this will assign it to null
  }
  get <- function() x                      ## define the get fucntion, this returns value of the matrix argument
  setinv <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment
  getinv <- function() inv                    ## gets the value of inv where called
  list(set = set, get = get, setinv = setinv, getinv = getinv) ## you need this in order to refer to the functions with the $ operator 
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

 
#m <- matrix(rnorm(16),4,4)
#y <- makeCacheMatrix(m)
#cacheSolve(y)
