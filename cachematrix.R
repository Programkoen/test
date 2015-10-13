## With the makeCacheMatrix it is possible to set and get a matrix object and cache its inverse.
## This function computes the inverse of the matrix set by the makeCachematrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The makeCacheMatrix  is a function with several arguments 
## $set: sets the matrix
## $get: returns the set matrix
## $setinverse: caches the inverse of the matrix that is put into this function.
## $getinverse: returns the cached inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set<-function(y){
                x<<-y
                I<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) I<<- solve
        getinverse<-function() I
        
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve(a): returns the inverse of the matrix set by: a<- makeCachematrix(), or the inverse from the cache when it had been set.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)){
                message("Getting cached data")
                return(I)}
        data <- x$get()
        I <- solve(data)
        x$setinverse(I)
        return(I)
        }
      


  
