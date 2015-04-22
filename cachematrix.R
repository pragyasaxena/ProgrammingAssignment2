## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores a list of four functions to be applied on the
## input matrix

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

