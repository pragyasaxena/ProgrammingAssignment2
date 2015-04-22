## cachesolve computes the inverse of the o/p returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves it 
## from the cache.

## makeCacheMatrix returns a list of four functions to be applied on the
## input matrix whose inverse needs to be cached.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  get <-function() x
  setinverse<-function(inv) inverse<<-inv
  getinverse<-function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve takes the o/p of makeCacheMatrix as input and checks if
## inverse is already calculated or not. Then returns the inverse of the 
## matrix X

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get
  inverse<-solve(data, ...)
  x$setinverse(inverse)
  inverse
}

