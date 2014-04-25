

## The objective of this functions is to save in cache, the inverse of
## a matrix.  This procedure has heavy computation cost, and this syntax
## can be usefull in this kind of procedures. The process is compound of
## 2 functions.


## The first one is used to set the matrix, get the matrix
## set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The objective of the second function is to ask to the first function,
## if the inverse was calculated before, and return the value pre-calculated,
## if not, the inverse is calculated and set with the first function with
## the <<- operator.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {   ## if is not null, and is saved in cache, return (i)
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)  ## if is not calculated before, solve calcuate inverse
  x$setinverse(i)   ## this command set the inverse in cache
  i
}






