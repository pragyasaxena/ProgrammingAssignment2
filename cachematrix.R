## cachesolve computes the inverse of the o/p returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves it 
## from the cache.

## makeCacheMatrix returns a list of four functions to be applied on the
## input matrix whose inverse needs to be cached.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL     #sets the inverse defined in makeCachematrix to NULL
  set <- function(y){ #set funcn sets the value of matrix x defined in 
    x<<-y             #makeCachematrix to y and since matrix changes, inverse
    inverse<<-NULL    #also needs to be reset
  }
  get <-function() x  #returns x
  setinverse<-function(inv) inverse<<-inv #sets inverse to the value inv
  getinverse<-function() inverse          #returns the inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve takes the o/p of makeCacheMatrix as input and checks if
## inverse is already calculated or not. Then returns the inverse of the    
## matrix X

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()            #fetches inverse in the o/p of makeCacheMatrix
  if(!is.null(inverse)){
    message("getting cached data") #checks if inverse is not NUll
    return(inverse)                #fetches it from cache
  }
  data<-x$get()                      #returns the matrix x into data
  inverse<-solve(data, ...)        #calculates inverse
  x$setinverse(inverse)            #sets the inverse
  inverse                          #returns inverse of X
}

